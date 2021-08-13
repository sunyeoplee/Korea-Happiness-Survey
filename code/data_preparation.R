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
data <- read_excel("C:/Users/USER/Desktop/Backup/Gap year/Korea Happiness Survey/data/raw_data_v3_20210811.xlsx")
covid_data <- read_excel("C:\\Users\\USER\\Desktop\\Backup\\Gap year\\Korea Happiness Survey\\data\\COVID-KR.xlsx")
covid_data <- covid_data[c(1:17),]

mydata <- data %>% 
  dplyr::rename(id=ID, census=집계구코드, strata=strataid, area2=읍면동, area3=시군구, area=LOC, # levels 
                smoking_change_cat=CO20, cigarettes=CO20T, # smoking outcomes 
                alcohol_change_days_cat=CO21, alcohol_days=CO21T, # alcohol outcomes
                alcohol_change_drinks_cat=CO22, alcohol_drinks=CO22T,
                health_change=CO17, # health outcome
                covid_positive=CO2, # covid measures
                socialladder=C1, home_payment=SQ107, residence_type=SQ108, family_type=SQ1011, # sociodemographic measures
                edu=DQ101, edu_paternal=DQ10101, edu_maternal=DQ10102,
                grad=DQ102, grad_paternal=DQ10201, grad_maternal=DQ10202,
                employed=DQ2, job_type=DQ3, employment_type1=DQ6, employment_type2=DQ7, work_hour=DQ4,
                job_change=CO6, income_change_individual=CO7, income_change_household=CO701, family_relationship_change=CO8,
                income_individual=DQ10, income_household=DQ1001, cost_of_living=DQ11, basicincome=SQ109,
                marital_status=SQ906, age=AGE, sex=SQ903,
                happiness=A1, life_satisfaction=A302, # psychological wellbeing measures
                anxiety=CO23, worry=CO2301, depression=CO2302, apathy=CO2303, loneliness=CO2304,
                physical_health=DQ12, chronic_disease=DQ13, disability=DQ14, # health measures,
                survey_weight=F_WT

               ) # %>% 
  # dplyr::select(c(area,
  #                 sex1))


# add covid data to the survey data
mydata$covid_cases <- NA
mydata$population <- NA

for (i in 1:17) {
  mydata$covid_cases[mydata$area==i] <- as.numeric(covid_data$cumulative_cases[covid_data$LOC==i])
  mydata$population[mydata$area==i] <- round(as.numeric(covid_data$population[covid_data$LOC==i]))
}


### modify variables ###

# levels
level_var <- c("id", "census", 'strata', 'area') # specify level variables

mydata[,level_var] <- lapply(mydata[,level_var], as.factor) # categorize level variables

# dependent variable: smoking
mydata$smoking_change_cat <- factor(mydata$smoking_change_cat, levels = c("3", "2", "1"))

mydata$smoking_change_cigarettes <- NA
mydata$smoking_change_cigarettes[mydata$smoking_change_cat=='1' & !is.na(mydata$smoking_change_cat)] <- mydata$cigarettes[mydata$smoking_change_cat=='1' & !is.na(mydata$smoking_change_cat)]
mydata$smoking_change_cigarettes[mydata$smoking_change_cat=='2' & !is.na(mydata$smoking_change_cat)] <- -1 * mydata$cigarettes[mydata$smoking_change_cat=='2' & !is.na(mydata$smoking_change_cat)]
mydata$smoking_change_cigarettes[mydata$smoking_change_cat=='3' & !is.na(mydata$smoking_change_cat)] <- 0

mydata$smoking_increased <- recode_factor(factor(mydata$smoking_change_cat),'1'='yes', 
                                          '2'='no', '3'="no")
mydata$smoking_increased <- factor(mydata$smoking_increased, levels = c("no", "yes"))

mydata$smoking_decreased <- recode_factor(factor(mydata$smoking_change_cat),'1'='no', 
                                          '2'='yes', '3'="no")
mydata$smoking_decreased <- factor(mydata$smoking_decreased, levels = c("no", "yes"))

# # dependent variable: alcohol
# mydata$alcohol_change_days[mydata$alcohol_change_days == 4] <- NA 
# mydata$alcohol_change_days <- factor(mydata$alcohol_change_days, levels = c("3", "2", "1"))
# 
# mydata$alcohol_increased <- recode_factor(factor(mydata$alcohol_change_days),'1'='yes', 
#                                           '2'='no', '3'="no")
# mydata$alcohol_increased <- factor(mydata$alcohol_increased, levels = c("no", "yes"))
# 
# mydata$alcohol_decreased <- recode_factor(factor(mydata$alcohol_change_days),'1'='no', 
#                                           '2'='yes', '3'="no")
# mydata$alcohol_decreased <- factor(mydata$alcohol_decreased, levels = c("no", "yes"))




# cat_dep_var <- c('smoking_change_cat', 'smoking_increased', 'smoking_decreased',
#                  'alcohol_change_days', 'alcohol_increased', 'alcohol_decreased')
# mydata[,cat_dep_var] <- lapply(mydata[,cat_dep_var], as.factor) # categorize level variables
# 
# summary(factor(mydata$alcohol_change_days))

# dependent variable: health 
mydata$health_change_3 <- recode_factor(mydata$health_change, '3'='same',
                                        '1'='improve','2'='improve',
                                        '4'='worsen','5'='worsen')

# change after COVID
mydata$job_change_5 <- recode_factor(mydata$job_change, '5'='same','4'='part_time','3'='temp','2'='pause','1'='unemployed')
# mydata$income_change_individual
# mydata$income_change_household
mydata$family_relationship_change_3 <- recode_factor(mydata$family_relationship_change, '3'='same',
                                                     '4'='improve','5'='improve',
                                                     '1'='worsen','2'='worsen')

# sociodemograhpic variables
mydata$sex <- recode_factor(factor(mydata$sex), '2'="female", '1'='male')
mydata$basicincome_2 <- recode_factor(factor(mydata$basicincome), '3'="no", '1'='yes', '2'='yes')

mydata$home_payment_3 <- recode_factor(factor(mydata$home_payment), '1'='own', '2'='yearly',
                                       '3'='monthly', '4'='monthly', '5'='monthly','6'='monthly')
mydata$family_type_3 <- recode_factor(factor(mydata$family_type), '6'='two_parents','1'='alone',
                                             '2'='one_or_grand_parent',
                                             '3'='one_or_grand_parent',
                                             '5'='one_or_grand_parent')
mydata$edu_self <- recode_factor(mydata$edu, '')
# phd, master, college4_grad, college2, highschool_grad, highschool_




mydata$income_household_rev <- factor(factor(mydata$income_household), levels = rev(levels(factor(mydata$income_household))))
mydata$income_individual_rev <- factor(factor(mydata$income_individual), levels = rev(levels(factor(mydata$income_individual))))

mydata$employment_5 <- NA
mydata$employment_5[mydata$employment_type2 == 1] <- 'regular'
mydata$employment_5[mydata$employment_type2 == 2 | mydata$employment_type2 == 3] <- 'temporary'
mydata$employment_5[mydata$employment_type1 == 2 | mydata$employment_type1 == 3] <- 'self'
mydata$employment_5[mydata$employment_type1 == 4] <- 'family'
mydata$employment_5[mydata$employed == 2] <- 'unemployed'
mydata$employment_5 <- factor(mydata$employment_5, levels = c('regular','self','temporary','family','unemployed'))

cont_var <- c('work_hour')
mydata[,cont_var] <- lapply(mydata[,cont_var], as.numeric)

sociodemo_var <- c('sex','basicincome_2','home_payment_3','family_tpe_3',
                   'income_household_rev','income_individual_rev','employment_5','work_hour')

# psychological wellbeing variables

mydata$anxiety_4 <- recode_factor(mydata$anxiety, '1'='never','2'='less_week','3'='more_week','4'='daily')
mydata$worry_4 <- recode_factor(mydata$worry, '1'='never','2'='less_week','3'='more_week','4'='daily')
mydata$depression_4 <- recode_factor(mydata$depression, '1'='never','2'='less_week','3'='more_week','4'='daily')
mydata$apathy_4 <- recode_factor(mydata$apathy, '1'='never','2'='less_week','3'='more_week','4'='daily')
mydata$loneliness_4 <- recode_factor(mydata$loneliness, '1'='never','2'='less_week','3'='more_week','4'='daily')

psych_var <- c('happiness','life_satisfaction','anxiety_4','worry_4','depression_4','apathy_4','loneliness_4')


# health variables

mydata$physical_health_5 <- recode_factor(mydata$physical_health, '1'='best','2'='good','3'='normal','4'='bad','5'='worst')
mydata$chronic_disease_4 <- recode_factor(mydata$physical_health, '0'='no','1'='less_3','2'='3-6','3'='more_6')
mydata$disability_2 <- recode_factor(mydata$disability, '2'='no','1'='yes')

health_var <- c('physical_health_5','chronic_disease_4','disability_2')

# check the distribution
summary(factor(mydata$health_change_3))
summary(mydata$anxiety)

corr_plot <- ggplot(mydata, aes(x=age, y=smoking_change_cigarettes)) + 
  geom_point(color='steelblue1', alpha=1) +
  geom_smooth(method=lm, se=T, color='steelblue') + 
  scale_x_continuous(name = "X", breaks = c(0,20,40,60)) +
  scale_y_continuous(name = "Y")
corr_plot

normal_variable <- mydata$life_satisfaction
hist(normal_variable)
ggplot(mydata, aes(x=normal_variable)) + geom_density() # density curve

CrossTable(mydata$employment_type1, mydata$employment_type2)

chisq.test(mydata$smoking_increased, mydata$basicincome)

dim(mydata[mydata$age<18, ])













