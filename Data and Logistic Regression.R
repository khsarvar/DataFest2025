#####
###Association between the moderator effect of depression and antihypertensive drug use on uncontrolled blood pressure 

library(dplyr)
library(BayesTree)
library(haven)
library(survey)
library(tidyverse)
library(gtsummary)
library(svydiags)
#install.packages("svydiags")
#Data loading
library("cardioStatsUSA")
#cardioStats includes NHANES survey cycle: 1999-2000, 2001-2002, 2003-2004, 2005-2006, 2007-2008, 
#2009-2010, 2011-2012, 2013-2014, 2015-2016, 2017-2020. 
#data in cardioStatsUSA package is stored as nhanes_data

#Download depression data from NHANES. We only used 2005-2020 data because previous years did not use PHQ-9 screening.
#Depression data collection reference: https://www.cdc.gov/nchs/products/databriefs/db303.htm
# The following data was from "Mental Health - Depression Screener (DPQ) 2005-2020". The screening was based on DSM-IV 
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2005/DataFiles/DPQ_D.xpt", tf <- tempfile(), mode="wb")
DPQ_D <- foreign::read.xport(tf)

download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2007/DataFiles/DPQ_E.xpt", tf <- tempfile(), mode="wb")
DPQ_E <- foreign::read.xport(tf)

download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2009/DataFiles/DPQ_F.xpt", tf <- tempfile(), mode="wb")
DPQ_F <- foreign::read.xport(tf)

download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/DPQ_G.xpt", tf <- tempfile(), mode="wb")
DPQ_G <- foreign::read.xport(tf)

download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DPQ_H.xpt", tf <- tempfile(), mode="wb")
DPQ_H <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DPQ_I.xpt", tf <- tempfile(), mode="wb")
DPQ_I <- foreign::read.xport(tf)

download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DPQ.xpt", tf <- tempfile(), mode="wb")
P_DPQ <- foreign::read.xport(tf)

#combine depression to one dataset
DPQ <- bind_rows(DPQ_D, DPQ_E,DPQ_F, DPQ_G , DPQ_H, DPQ_I,P_DPQ)
#change SEQN to svy_id to match with the id in cardioStatsUSA
DPQ$svy_id<-DPQ$SEQN
#add depression data to the cardioStatsUSA data, excluding data from 1999-2000, 2001-2002, 2003-2004
data <- inner_join(DPQ, nhanes_data, by="svy_id") 

#Compute depression score by summing answers from DPQ010 to DPQ090 

###
completeData<-data%>%
#set response refused to answer or don't know to missing
mutate_at(vars(DPQ010:DPQ090), ~ifelse(. >=7, NA, .))
#compute depression score based on PHQ-9 (DPQ010:DPQ090)
completeData$depressionScore<-rowSums(completeData[,2:10])
#create a binary variable called depression (1= depression, 0 = no depression)
completeData$depression<-ifelse(completeData$depressionScore >=10, 1, 0)
#create indicator for insample data. We excluded missing data from our analysis 
completeData$inSample<-!is.na(completeData$depressionScore)


  
#Survey weights
nhanes <- svydesign(data=completeData, id=~svy_id, strata=~svy_strata, weights=~svy_weight_mec, nest=TRUE)

#subpopulation of interest where inSample=1
nhanesComplete<-subset(nhanes , inSample)

#function that returns unweighted frequency and weighted percentage
getSummary <- function(varformula, byformula, design){
  # Get mean, stderr, and unweighted sample size
  c <- svyby(varformula, byformula, design, unwtd.count) 
  p <- svyby(varformula, byformula, design, svymean) 
  
  # Ensure we are correctly removing 'se' columns by checking names
  c <- c[, !grepl("se", colnames(c))]
  p <- p[, !grepl("se", colnames(p))]
  
  # Combine the results
  outSum <- left_join(c, p) 
  
  outSum
}

#Descriptive statistics 

#Depression unweighted frequency and weighted percentage 
getSummary(~demo_gender, ~inSample, nhanesComplete)
confint(svyby(~demo_gender, ~ inSample, nhanesComplete, svymean ))
#    inSample    counts depression          se   cl
#1     TRUE       39467 0.07696438 0.001705452   (0.07362176, 0.08030701)

###### 
# install.packages("tableone")

library(tableone)

vars <- c("demo_gender", "demo_age_cat", "demo_race", "bp_med_use", 
          "depression", "cc_smoke", "cc_bmi", "cc_cvd_any", "depressionScore")

# Create table stratified by hypertension status
baseline_table <- CreateTableOne(vars = vars, strata = "bp_uncontrolled_130_80", data = completeData[completeData$inSample,], test = FALSE, addOverall = TRUE)

# Print with confidence intervals
print(baseline_table, showAllLevels = TRUE, quote = TRUE)

######

# Function to calculate proportions with CI
survey_proportion <- function(var) {
  svymean(~get(var), nhanesComplete, na.rm = TRUE)
}

# Example: Depression prevalence
survey_proportion("depression")

library(dplyr)
library(survey)

weighted_counts_gender <- svytable(~demo_gender, design = nhanesComplete)
print(weighted_counts_gender)

weighted_counts_age <- svytable(~demo_age_cat, design = nhanesComplete)
print(weighted_counts_age)

prop_18_30 <- svyciprop(~I(demo_age_cat == "18-30"), design = nhanesComplete, method = "logit")
print(prop_18_30)

######
#Depression over uncontrolled hypertension unweighted frequency and weighted percentage 
confint(svyby(~depression, ~ bp_uncontrolled_130_80, nhanesComplete, svymean ))
getSummary(~depression, ~ bp_uncontrolled_130_80, nhanesComplete)

library(gtsummary)
library(survey)
library(purrr)

# Build the survey summary table
tbl <- tbl_svysummary(
  data = nhanesComplete,
  include = c("demo_gender", 
              "demo_age_cat", 
              "demo_race", 
              "bp_med_use",
              "depression", 
              "cc_smoke", 
              "cc_bmi", 
              "cc_cvd_any"),
  percent = "column",
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
  type = list(c("demo_gender", 
                "demo_age_cat", 
                "demo_race", 
                "bp_med_use",
                "depression", 
                "cc_smoke", 
                "cc_bmi", 
                "cc_cvd_any") ~ "categorical"),
  digits = list(all_categorical() ~ c(0, 1)),
  label = list(
    demo_gender ~ "Gender",
    demo_age_cat ~ "Age Category",
    demo_race ~ "Race",
    bp_med_use ~ "Antihypertensive medication use",
    depression ~ "Depression",
    cc_smoke ~ "Smoking",
    cc_bmi ~ "BMI",
    cc_cvd_any ~ "History of CHD, MI, stroke or HF"
  )
) %>% 
  # Add confidence intervals. The `degf()` function extracts the degrees of freedom from the survey design.
  add_ci(
    style_fun = list(
      all_categorical() ~ partial(gtsummary::style_percent, digits = 1)
    ),
    df = degf(nhanesComplete)
  ) %>%
  modify_footnote(everything() ~ NA)

# Print the table
tbl

#Logistic regression with the interaction term. I used uncontrolled BP as 140/90 because doctors 
#will prescribe medications only when BP is higher than 140/90 

logit3 <- (svyglm(bp_uncontrolled_140_90~factor(demo_age_cat)+factor(demo_race)+depressionScore
                  +factor(bp_med_use)+factor(cc_smoke)+
                    factor(cc_cvd_any)+factor(htn_aware)+factor(bp_med_use)*depressionScore+
                    factor(cc_bmi), family=quasibinomial, design=nhanesComplete, na.action = na.omit))
summary(logit3)


#for model.matrix, it won't take interaction term. 
X.model<-model.matrix(~factor(demo_age_cat)+factor(demo_race)+depressionScore
                      +factor(bp_med_use)+factor(cc_smoke)+
                        factor(cc_cvd_any)+factor(htn_aware)+factor(bp_med_use):depressionScore+
                        factor(cc_bmi),data=data.frame(df))

