### title: Happiness Korea Data Preparation
### authors: Sun Y. Lee
### dates: 2021-07-23

# packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(skimr)
library(MASS)
library(sjlabelled)
library(gtools)
library(tableone)
library(survey)
library(gmodels)




# import data
data <- read_excel("C:/Users/USER/Desktop/Backup/Gap year/Korea Happiness Survey/data/raw_data_v3.xlsx")
covid_data <- read_excel("C:\\Users\\USER\\Desktop\\Backup\\Gap year\\Korea Happiness Survey\\data\\COVID-KR.xlsx")
covid_data <- covid_data[c(1:17),]

mydata <- data %>% 
  dplyr::rename( id=ID, census=집계구코드, area2=읍면동, area3=시군구, area=LOC, # levels 
                smoking_change=CO20, smoking_pieces=CO22T, # smoking outcomes 
                alcohol_change_days=CO21, alcohol_days=CO21T, # alcohol outcomes
                alcohol_change_drinks=CO22, alcohol_drinks=CO22T,
                socialladder=C1, home_payment=SQ107, residence_type=SQ108, # sociodemographic measures
                edu=DQ101, edu_paternal=DQ10101, edu_maternal=DQ10102,
                grad=DQ102, grad_paternal=DQ10201, grad_maternal=DQ10202,
                employed=DQ2, job_type=DQ3, employment_type1=DQ6, employment_type2=DQ7, work_hour=DQ4,
                income_individual=DQ10, income_household=DQ1001, cost_of_living=DQ11, basicincome=SQ109,
                marital_status=SQ906, age=AGE, sex=SQ903,
                happiness=A1, life_satisfaction=A302, # psychological wellbeing measures
                anxiety=CO23, worry=CO2301, depression=CO2302, apathy=CO2303, loneliness=CO2304,
                physical_health=DQ12, chronic_disease=DQ13, disability=DQ14 # health measures

               ) # %>% 
  # dplyr::select(c(area,
  #                 sex1))

colnames(data)

summary(factor(mydata$census))

levels(factor(mydata$sex))
class(mydata$sex1)

summary(factor(mydata$alcohol_change))
sum(is.na(mydata$area3))

mydata$a <- ifelse(mydata$sex == 1, 1, 0) 
mydata$a <- mydata$a * mydata$F_WT
sum(mydata$a)
# add covid data to the survey data
mydata$covid_cases <- NA
mydata$population <- NA

for (i in 1:17) {
  mydata$covid_cases[mydata$area==i] <- as.numeric(covid_data$cumulative_cases[covid_data$LOC==i])
  mydata$population[mydata$area==i] <- round(as.numeric(covid_data$population[covid_data$LOC==i]))
}




### modify variables ###

# levels
level_var <- c("id", "census", 'area') # specify level variables

mydata[,level_var] <- lapply(mydata[,level_var], as.factor) # categorize level variables

# dependent variables
mydata$smoking_change <- factor(mydata$smoking_change, levels = c("3", "2", "1"))

mydata$smoking_increased <- recode_factor(factor(mydata$smoking_change),'1'='yes', 
                                          '2'='no', '3'="no")
mydata$smoking_increased <- factor(mydata$smoking_increased, levels = c("no", "yes"))

mydata$smoking_decreased <- recode_factor(factor(mydata$smoking_change),'1'='no', 
                                          '2'='yes', '3'="no")
mydata$smoking_decreased <- factor(mydata$smoking_decreased, levels = c("no", "yes"))

mydata$alcohol_change[mydata$alcohol_change == 4] <- NA 
mydata$alcohol_change <- factor(mydata$alcohol_change, levels = c("3", "2", "1"))

mydata$alcohol_increased <- recode_factor(factor(mydata$alcohol_change),'1'='yes', 
                                          '2'='no', '3'="no")
mydata$alcohol_increased <- factor(mydata$alcohol_increased, levels = c("no", "yes"))

mydata$alcohol_decreased <- recode_factor(factor(mydata$alcohol_change),'1'='no', 
                                          '2'='yes', '3'="no")
mydata$alcohol_decreased <- factor(mydata$alcohol_decreased, levels = c("no", "yes"))


cat_dep_var <- c('smoking_change', 'smoking_increased', 'smoking_decreased',
                 'alcohol_change', 'alcohol_increased', 'alcohol_decreased')
mydata[,cat_dep_var] <- lapply(mydata[,cat_dep_var], as.factor) # categorize level variables

summary(factor(mydata$alcohol_change))
# independent variables

mydata$basicincome_binary <- recode_factor(factor(mydata$basicincome),'1'='yes', '2'='yes',
                                       '3'="no")
mydata$basicincome_binary <- factor(mydata$basicincome_binary, levels = c("no", "yes"))




CrossTable(mydata$alcohol_increased)

chisq.test(mydata$smoking_increased, mydata$basicincome)





summary(factor(mydata$alcohol_change))
a = na.omit(mydata)



summary(factor(mydata$sex1))













