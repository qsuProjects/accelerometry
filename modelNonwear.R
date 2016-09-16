# Wed Dec 16 10:22:23 2015 ------------------------------
if(FALSE) {
  rm(list = ls())
  gc()
  source("~/.Rprofile")
}
########################################
# this script loads 1 minute choi-classified 
# data and models nonwear data patterns as 
# a function of season, time of day
########################################

setwd("/Volumes/QSU/Datasets/SPRC/Robinson/imputationProject/")
source("src/r/projectFunctions.R")

# file_list <- list.files("/Volumes/QSU/Datasets/SPRC/Robinson/imputationProject/data/simData/")

library(lubridate)
library(plyr)
library(dplyr)
library(scales)
library(stringr)
# library(PhysicalActivity)


#define beginning and end of daytime
daytime <- c("07:00:00", "21:00:00")

#define start end dates of school year
#based on oakland public school start dates?
school_year <- c("09/01/", "05/31")

getDayOfYear <- function(.date) {
  library(lubridate)
  as.numeric( .date - mdy(paste0( "01/01/", year(.date) ) ) )
}


#load choi'ed data
class_files <- openFilesInDirectory(directory = "/Volumes/QSU/Datasets/SPRC/Robinson/imputationProject/data/choi1Min/", 
                                    merge = TRUE, delim_str = ",", match_string = ".csv", header = TRUE)

#set leading or trailing nonwear epochs to wear
class_files <- ddply(class_files, .(loaded_file_name), function(.class_files) {
  
  cat(.class_files$loaded_file_name[1], "\n")
  
  #check and fix leading nonwear epochs
  if(.class_files$wearing_choi_axis1[1] == "nw") {
    .nonwear_end_time <- min(which(.class_files$wearing_choi_axis1 == "w")) - 1
    .class_files$wearing_choi_axis1[1:.nonwear_end_time] <- "w"
  }
  
  if(tail(.class_files$wearing_choi_axis1, 1) == "nw") {
    .nonwear_start_time <- max(which(.class_files$wearing_choi_axis1 == "w")) + 1
    .class_files$wearing_choi_axis1[.nonwear_start_time:nrow(.class_files)] <- "w"
  }
  
  .class_files
})

#format ids
class_files$fID <- gsub("CHOI_|t.{1,}", "", class_files$loaded_file_name)

#create numeric version of wear
class_files$nonwear <- 0
class_files$nonwear[class_files$wearing_choi_axis1 == "nw"] <- 1

#generate timestamp variable
class_files$ts <- mdy(class_files$Date) + hms(class_files$Time)

#generate hour variable
class_files$hour <-   hour(hms(class_files$Time)) 

#generate season variable
class_files$start_doy <- getDayOfYear(mdy( paste0( school_year[1], year( mdy( class_files$Date) ) ) ))
class_files$end_doy <- getDayOfYear(mdy( paste0( school_year[2], year( mdy( class_files$Date) ) ) ))
class_files$doy <- getDayOfYear(mdy( class_files$Date ) )

class_files$season <- "Summer Vacation"
class_files$season[class_files$doy >= class_files$start_doy | 
                     class_files$doy <= class_files$end_doy ] <- "School Year"



#load demo data
demo_data <- read.table(file = "data/other//GOALS_T1.csv", sep = ",", 
                        header = T, stringsAsFactors = F, quote = "\"")

#merge demo_data and class_act
demo_class <- merge(x = class_files,
                    y = demo_data,
                    by.x = "fID",
                    by.y = "ID_KID",
                    all.x = TRUE)

########################################
#check ICCs
########################################
# 
# demo_class$fID_f <- factor(demo_class$fID)
# library(ICC)
# library(lme4)
# library(glmmADMB)
# install.packages("glmmADMB")
# install.packages("R2admb")
# install.packages("glmmADMB", 
#                  repos=c("http://glmmadmb.r-forge.r-project.org/repos",
#                          getOption("repos")),
#                  type="source")
# #combined
# emp_icc <- ICCest(x = fID_f, y = counts_axis1, data = filter(demo_class, wearing_choi_axis1 == "w"))
# emp_icc$ICC
# # 0.01482691 for all data
# # 0.01434633 for only wear data
# emp_icc$vara / (emp_icc$varw + emp_icc$vara)
# summary(demo_class$counts_axis1)
# emp_mod <- lmer(counts_axis1 ~ (1|fID), data = demo_class)
# summary(emp_mod)
# 8582 / (8582 + 585291)
# emp_mod_nb <- glmmadmb(counts_axis1 ~ (1|fID_f), data = demo_class, family = "nbinom")
# summary(emp_mod)
# 
# #boys and girls
# demo_class[1,]
# class(demo_class$T1SEX)
# emp_icc_boys <- ICCest(x = fID, y = counts_axis1, data = filter(demo_class, T1SEX == "Male" & wearing_choi_axis1 == "w") )
# emp_icc_boys$ICC 
# # 0.01344 for all data
# # 0.01074234 for only wear data
# emp_icc_girls <- ICCest(x = fID, y = counts_axis1, data = filter(demo_class, T1SEX != "Male" & wearing_choi_axis1 == "w") )
# emp_icc_girls$ICC 
# # 0.01355 for all data
# # 0.01470828 for only wear data


#Evenson
demo_class$activity_levels <- classifyEvenson(demo_class, "counts_axis1")
table(demo_class$activity_levels)


#PUYAU
# demo_class$activity_levels <- classifyPuyau(demo_class, "counts_axis1")
# row(
demo_class$activity_levels <- factorConvert(demo_class$activity_levels, to_type = "c")
demo_class$activity_levels[demo_class$nonwear == 1] <- "nonWear"

table(demo_class$activity_levels, demo_class$nonwear )

nonwear_dists <- ddply(demo_class, .(fID), function(.demo_class) {
  
  .demo_class$rownum <- row(.demo_class)
  .demo_class$lastrownum <- max(row(.demo_class))
  .demo_class$nonwear_num <- (cumsum( c(1, abs( diff( .demo_class$nonwear ) ) ) ) ) * .demo_class$nonwear
  
  if( max(.demo_class$nonwear_num) > 0 ) {
    .tor <- ddply(.demo_class[.demo_class$nonwear == 1,], .(nonwear_num), function(..cont_nonwear) {
      data.frame(nonwear_length = nrow(..cont_nonwear),
                 row_start = min(..cont_nonwear$rownum),
                 last_row = ..cont_nonwear$lastrownum[1]) 
    } )
  } else {
    .tor <- data.frame(nonwear_length = NA ) 
  }
  .tor$nonwear_seq <- 1:nrow(.tor)
  # print(paste0(.demo_class$fID[1], names(.tor)) )
  .tor
})

nonwear_dists <- nonwear_dists[!is.na(nonwear_dists$nonwear_num),]


pdf("output/plots/nonwearDurationDists.pdf")
ggplot(data = nonwear_dists, aes(x = nonwear_length) ) +
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = 90, color = "red", size = 2, linetype = 2) +
  scale_x_continuous("Nonwear Interval Duration (minutes)", limits = c(0,1000)) +
  scale_y_continuous("Number of\nNonwear\nIntervals") 
dev.off()

demo_class$activity_levels_n <- c(0:4)[match(demo_class$activity_levels ,c("nonWear", "Sedentary", "Light", "Moderate", "Vigorous"))  ]

demo_class$activity_levels_f <- factor(demo_class$activity_levels, levels = unique(demo_class$activity_levels))


########################################
#generate models of missingness
########################################
         
#generate a bunch of models of probability of missingness

#define noninformative offsets
demo_class$any_offset <- 1
demo_class$all_offset <- 1
demo_class$extent_offset <- 1

#calculate within-hour stats
#split data by person x season x hour x weekday and determine if any nonwear ocurred, 
#and if so, determine how much
by_epoch_cohort <- ddply(demo_class, .(fID, season, hour, weekday, Date), function(.demo_class) {
  cat(.demo_class$fID[1], "\n")
  .tor <- .demo_class[1, ]
  .tor$any_nonwear <- max(.demo_class$nonwear) 
  
  #if there is nonwear, determine characteristics of nonwear
  if(.tor$any_nonwear) {
    .tor$n_nonwear_epochs <- sum(.demo_class$nonwear)
    .tor$n_wear_epochs <- nrow(.demo_class) - sum(.demo_class$nonwear)
    .tor$n_nonwear_periods <- 
      as.numeric(.demo_class$nonwear[1] == 1 ) + #did the interval start with nonwear
      sum(diff( .demo_class$nonwear ) == 1) #how many times did person go from wear to nonwear
    .tor$any_wear <- as.numeric( .tor$n_nonwear_epochs != nrow(.demo_class) )
  } else {
    .tor$n_nonwear_epochs <- 0
    .tor$n_nonwear_periods <- 0
    .tor$n_wear_epochs <- nrow(.demo_class)
    .tor$any_wear <- 1
  }
  
  #tabulate activity levels
  .prop_activity_levels <- prop.table(table(.demo_class$activity_levels_f))
  .tor <- cbind(.tor, matrix(.prop_activity_levels, nrow = 1) )
  names(.tor)[grepl("^[:0-9:]$", names(.tor) )] <- paste0("epoch_", levels(.demo_class$activity_levels_f))

  .tor
  
})

#calculate observed activity levels
observed_activity_levels <- ddply(demo_class, .(fID), function(.demo_class) {
  .tor <- .demo_class[1, ]
  
  #tabulate activity levels
  .prop_activity_levels <- prop.table(table(.demo_class$activity_levels_f))
  .tor <- cbind(.tor, matrix(.prop_activity_levels, nrow = 1) )
  names(.tor)[grepl("^[:0-9:]$", names(.tor) )] <- levels(.demo_class$activity_levels_f)
  
  .tor[ ,levels(.demo_class$activity_levels_f)]
  
})

#merge activity levels with hour-level data
any_missing_cohort <- merge(x = by_epoch_cohort, 
                            y = observed_activity_levels,
                            by = "fID")
  
names(any_missing_cohort) <- gsub("-", "", names(any_missing_cohort) )

#model missingess
model_variables <- c("season", "factor(hour)", "weekday", "T1SEX", "T1CRACE", "t1_age", "t1_BMIPCT")

#Vigorous is omitted bc all categories sum to one
activity_variables <- c("nonWear", "Sedentary", "Light", "Moderate")

model_activity_variables <- c(model_variables, activity_variables)

#I originally included T1CHISP, but it's collinear with T1RACE
table(any_missing_cohort$T1CHISP, any_missing_cohort$T1CRACE)

#                        American Indian Asian Black Multi Other/Unknown White
# Hispanic or Latino                 539     0     0   685         38767  7678
# Not Hispanic or Latino               0   140   640     0             0     0


#Is nonwear during the previous hour associated 
#with nonwear during the current hour?
test <- any_missing_cohort
test$prev_miss <- c(0, head(test$any_nonwear, -1))
testmod <- glm(any_nonwear ~ prev_miss, data = test, family = "binomial")
summary(testmod)

################################################
#generate quasi-unconditional missingness models 
################################################

#logistic model of probability of missingness within a time period
# missing_prob_formula <- as.formula(paste0("any_nonwear ~ 1 + offset(log(any_offset))") ) 
# missing_prob_mod <- glm(data = any_missing_cohort, missing_prob_formula, family = "binomial") 
# summary(missing_prob_mod)

#model including probability that previous epoch was missing

#generate variable for nonwear during previous hour
#nonwear during first hour uses the sample-wide prob
any_missing_cohort$prev_miss <- c(rbinom(1, size = 1, prob = mean(any_missing_cohort$any_nonwear)), 
                                  head(any_missing_cohort$any_nonwear, n = -1))

#model
missing_prob_lastprob_formula <- as.formula(paste0("any_nonwear ~ 1 + ", 
                                                            "prev_miss + ",
                                                            "offset(log(any_offset))") )  
missing_prob_lastprob_mod <- glm(data = any_missing_cohort, 
                                 missing_prob_lastprob_formula, family = "binomial") 

summary(missing_prob_lastprob_mod)

#logistic model of probability that whole time period is missing
all_missing_prob_formula <- as.formula(paste0("any_wear ~ 1 + offset(log(all_offset))") ) 
all_missing_prob_mod <- glm(data = filter(any_missing_cohort, any_nonwear == 1), 
                            all_missing_prob_formula, family = "binomial") 
summary(all_missing_prob_mod)

#model missing period duration for intervals that weren't all wear or nonwear
period_length_formula <-  as.formula(paste0("n_nonwear_epochs ~ 1 + offset(log(extent_offset))" ) ) 
period_length_mod <- glm(data = filter(any_missing_cohort, any_nonwear == 1 & any_wear == 1), 
                         period_length_formula, family = "poisson") 
summary(period_length_mod)

################################################
#generate missingness models based on covariates
################################################
# 
# #logistic model of probability of missingness within a time period
# missing_prob_covs_formula <- as.formula(paste0("any_nonwear ~ ", 
#                                                paste0(c(model_variables, 
#                                                         "offset(log(any_offset))"), 
#                                                       collapse = " + ") ) ) 
# 
# missing_prob_covs_mod <- glm(data = any_missing_cohort, missing_prob_covs_formula, family = "binomial") 

#add nonwear probability of previous epoch to data
any_missing_cohort$prev_miss <- c(rbinom(1, size = 1, prob = mean(any_missing_cohort$any_nonwear)), 
                                       head(any_missing_cohort$any_nonwear, n = -1))

#model
missing_prob_covs_lastprob_formula <- as.formula(paste0("any_nonwear ~ ", 
                                                        paste0(c(model_variables, 
                                                                 "prev_miss", 
                                                                 "offset(log(any_offset))"), 
                                                               collapse = " + ") ) ) 
missing_prob_covs_lastprob_mod <- glm(data = any_missing_cohort, 
                                      missing_prob_covs_lastprob_formula, family = "binomial") 

summary(missing_prob_covs_lastprob_mod)


#logistic model of probability that whole time period is missing
all_missing_prob_covs_formula <- as.formula(paste0("any_wear ~ ", paste0(c(model_variables, "offset(log(all_offset))"), collapse = " + ") ) ) 
all_missing_prob_covs_mod <- glm(data = filter(any_missing_cohort, any_nonwear == 1), 
                            all_missing_prob_covs_formula, family = "binomial") 
summary(all_missing_prob_covs_mod)

#model missing period duration for intervals that weren't all wear or nonwear
period_length_covs_formula <-  as.formula(paste0("n_nonwear_epochs ~ ", paste0(c(model_variables, "offset(log(any_offset))"), collapse = " + ") ) ) 
period_length_covs_mod <- glm(data = filter(any_missing_cohort, any_nonwear == 1 & any_wear == 1), 
                         period_length_covs_formula, family = "poisson") 
summary(period_length_covs_mod)


################################################
#generate missingness models based on covariates + observed activity levels
################################################

#logistic model of probability of missingness within a time period
# missing_prob_covs_act_formula <- as.formula(paste0("any_nonwear ~ ", 
#                                                    paste0(model_activity_variables, collapse = " + ") ) ) 
# missing_prob_covs_act_mod <- glm(data = any_missing_cohort, 
#                                  missing_prob_covs_act_formula, family = "binomial") 
# summary(missing_prob_covs_act_mod)


#model including probability that previous epoch was missing
any_missing_cohort$prev_miss <- c(rbinom(1, size = 1, prob = mean(any_missing_cohort$any_nonwear)), 
                                       head(any_missing_cohort$any_nonwear, n = -1))

#model
missing_prob_covs_act_lastprob_formula <- as.formula(paste0("any_nonwear ~ ", 
                                                        paste0(c(model_activity_variables, 
                                                                 "prev_miss", 
                                                                 "offset(log(any_offset))"), 
                                                               collapse = " + ") ) ) 
missing_prob_covs_act_lastprob_mod <- glm(data = any_missing_cohort, 
                                      missing_prob_covs_act_lastprob_formula, family = "binomial") 
summary(missing_prob_covs_act_lastprob_mod)

#logistic model of probability that whole time period is missing
all_missing_prob_covs_act_formula <- as.formula(paste0("any_wear ~ ", 
                                                       paste0(model_activity_variables, collapse = " + ") ) ) 
all_missing_prob_covs_act_mod <- glm(data = filter(any_missing_cohort, any_nonwear == 1), 
                                 all_missing_prob_covs_act_formula, family = "binomial") 
summary(all_missing_prob_covs_act_mod)

#model missing period duration for intervals that weren't all wear or nonwear
period_length_covs_act_formula <-  as.formula(paste0("n_nonwear_epochs ~ ", 
                                                     paste0(model_activity_variables, collapse = " + ") ) ) 
period_length_covs_act_mod <- glm(data = filter(any_missing_cohort, any_nonwear == 1 & any_wear == 1), 
                              period_length_covs_act_formula, family = "poisson") 

summary(period_length_covs_act_mod)

#write models and variables used
save(list = c("model_variables", "model_activity_variables",
              "missing_prob_lastprob_mod", "all_missing_prob_mod", "period_length_mod",
              "missing_prob_covs_lastprob_mod", "all_missing_prob_covs_mod", "period_length_covs_mod",
              "missing_prob_covs_act_lastprob_mod", "all_missing_prob_covs_act_mod", "period_length_covs_act_mod"),
     file = "data/models/missingnessModels.Rda")
# "missing_prob_mod", "missing_prob_covs_mod", "missing_prob_covs_act_mod",


