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
library(faraway)
library(caret)
library(car)
library(rms)



# import data
data <- read_excel("C:/Users/USER/Desktop/Backup/Gap year/Korea Happiness Survey/data/raw_data_v3_20210811.xlsx")
covid_data <- read_excel("C:\\Users\\USER\\Desktop\\Backup\\Gap year\\Korea Happiness Survey\\data\\COVID-KR.xlsx")
covid_data <- covid_data[c(1:17),]

mydata <- data %>% 
  dplyr::rename(id=ID, census=census, strata=strataid, area2=eup_myeon_dong, area3=si_gun_gu, area=LOC, # levels 
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
                online_friends=D7, offline_friends=D701, # social capital measures
                happiness=A1, life_satisfaction=A302, # psychological wellbeing measures
                anxiety=CO23, worry=CO2301, depression=CO2302, apathy=CO2303, loneliness=CO2304,
                physical_health=DQ12, chronic_disease=DQ13, disability=DQ14, # health measures,
                survey_weight=F_WT

               ) %>% 
  dplyr::select(c(id, census, strata, area2, area3, area,
                  smoking_change_cat, cigarettes, # smoking outcomes 
                  alcohol_change_days_cat, alcohol_days, # alcohol outcomes
                  alcohol_change_drinks_cat, alcohol_drinks,
                  health_change, # health outcome
                  covid_positive, # covid measures
                  socialladder, home_payment, residence_type, family_type, # sociodemographic measures
                  edu, edu_paternal, edu_maternal,
                  grad, grad_paternal, grad_maternal,
                  employed, job_type, employment_type1, employment_type2, work_hour,
                  job_change, income_change_individual, income_change_household, family_relationship_change,
                  income_individual, income_household, cost_of_living, basicincome,
                  marital_status, age, sex,
                  online_friends, offline_friends, # social capital measures
                  happiness, life_satisfaction, # psychological wellbeing measures
                  anxiety, worry, depression, apathy, loneliness,
                  physical_health, chronic_disease, disability, # health measures,
                  survey_weight
                  ))

## check for missing values
na_count <-sapply(mydata, function(y) sum(is.na(y)))
print(na_count <- data.frame(na_count))

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

# dependent variable: smoking change
mydata$smoking_change_cat <- recode_factor(mydata$smoking_change_cat, "3"='same', "2"='less', "1"='more')

mydata$smoking_change_cigarettes <- NA
mydata$smoking_change_cigarettes[mydata$smoking_change_cat=='1' & !is.na(mydata$smoking_change_cat)] <- mydata$cigarettes[mydata$smoking_change_cat=='1' & !is.na(mydata$smoking_change_cat)]
mydata$smoking_change_cigarettes[mydata$smoking_change_cat=='2' & !is.na(mydata$smoking_change_cat)] <- -1 * mydata$cigarettes[mydata$smoking_change_cat=='2' & !is.na(mydata$smoking_change_cat)]
mydata$smoking_change_cigarettes[mydata$smoking_change_cat=='3' & !is.na(mydata$smoking_change_cat)] <- 0

mydata$smoking_increased <- recode_factor(factor(mydata$smoking_change_cat),'same'='no', 'less'="no",'more'='yes')
mydata$smoking_decreased <- recode_factor(factor(mydata$smoking_change_cat),'2'='yes', '3'="no",'1'='no')

# dependent variable: alcohol change
mydata$alcohol_change_days_cat[mydata$alcohol_change_days_cat == 4] <- NA
mydata$alcohol_change_days_cat <- recode_factor(mydata$alcohol_change_days_cat, "3"='same', "2"='less', "1"='more')

mydata$alcohol_change_drinks_cat <- recode_factor(mydata$alcohol_change_drinks_cat, "3"='same', "2"='less', "1"='more')

mydata$alcohol_change_cat <- NA
mydata$alcohol_change_cat[mydata$alcohol_change_days_cat=='same' & mydata$alcohol_change_drinks_cat=='same'] <- 'same'
mydata$alcohol_change_cat[(mydata$alcohol_change_days_cat=='less' & mydata$alcohol_change_drinks_cat=='same') |
                            (mydata$alcohol_change_days_cat=='same' & mydata$alcohol_change_drinks_cat=='less') |
                            (mydata$alcohol_change_days_cat=='less' & mydata$alcohol_change_drinks_cat=='less')] <- 'less'
mydata$alcohol_change_cat[mydata$alcohol_change_days_cat=='more' | mydata$alcohol_change_drinks_cat=='more'] <- 'more'
mydata$alcohol_change_cat <- factor(mydata$alcohol_change_cat, levels=c('same','less','more'))

# alcohol_days
# alcohol_drinks   

# mydata$alcohol_increased <- recode_factor(factor(mydata$alcohol_change_days),'2'='no', '3'="no",'1'='yes')
# mydata$alcohol_decreased <- recode_factor(factor(mydata$alcohol_change_days),'2'='yes', '3'="no",'1'='no')

# cat_dep_var <- c('smoking_change_cat', 'smoking_increased', 'smoking_decreased',
#                  'alcohol_change_days', 'alcohol_increased', 'alcohol_decreased')
# mydata[,cat_dep_var] <- lapply(mydata[,cat_dep_var], as.factor) # categorize level variables
# 
# summary(factor(mydata$alcohol_change_days))

# dependent variable: health change
mydata$health_change_3 <- recode_factor(mydata$health_change, '3'='same',
                                        '1'='improve','2'='improve',
                                        '4'='worsen','5'='worsen')

# change after COVID
mydata$job_change_6 <- recode_factor(mydata$job_change, '5'='same','4'='part_time','3'='temp','2'='pause','1'='unemployed','6'='no')
# mydata$income_change_individual
# mydata$income_change_household
mydata$family_relationship_change_3 <- recode_factor(mydata$family_relationship_change, '3'='same',
                                                     '4'='improve','5'='improve',
                                                     '1'='worsen','2'='worsen')

change_var <- c('job_change_6', 'family_relationship_change_3')

# sociodemograhpic variables
mydata$sex <- recode_factor(factor(mydata$sex), '2'="female", '1'='male')
mydata$basicincome_2 <- recode_factor(factor(mydata$basicincome), '3'="no", '1'='yes', '2'='yes')

mydata$home_payment_3 <- recode_factor(factor(mydata$home_payment), '1'='own', '2'='yearly',
                                       '3'='monthly', '4'='monthly', '5'='monthly','6'='monthly')
mydata$family_type_2 <- recode_factor(factor(mydata$family_type), '6'='two_parents',
                                      '1'='one_grand_alone','2'='one_grand_alone',
                                      '3'='one_grand_alone','5'='one_grand_alone') # 0 case for 4



# mydata$edu_self <- recode_factor(mydata$edu, '')
# phd, master, college4_grad, college2, highschool_grad, highschool_

mydata$income_household_rev <- factor(factor(mydata$income_household), levels = rev(levels(factor(mydata$income_household))))
# mydata$income_household_rev_3

mydata$income_individual_rev <- factor(factor(mydata$income_individual), levels = rev(levels(factor(mydata$income_individual))))

mydata$employment_4 <- NA
mydata$employment_4[mydata$employment_type2 == 1] <- 'regular'
mydata$employment_4[mydata$employment_type2 == 2 | mydata$employment_type2 == 3] <- 'temporary'
mydata$employment_4[mydata$employment_type1 == 2 | mydata$employment_type1 == 3] <- 'self_family'
mydata$employment_4[mydata$employment_type1 == 4] <- 'self_family'
mydata$employment_4[mydata$employed == 2] <- 'unemployed'
mydata$employment_4 <- factor(mydata$employment_4, levels = c('regular','self_family','temporary','unemployed'))

cont_var <- c('work_hour')
mydata[,cont_var] <- lapply(mydata[,cont_var], as.numeric)

sociodemo_var <- c('age','sex','basicincome_2','home_payment_3','family_type_2',
                   'income_household_rev','income_household','income_individual_rev','employment_4','work_hour')

# social capital variables
mydata$friends <- mydata$online_friends + mydata$offline_friends

social_var <- c('online_friends','offline_friends', 'friends')

# psychological wellbeing variables

mydata$anxiety_4 <- recode_factor(mydata$anxiety, '1'='never','2'='less_week','3'='more_week','4'='daily')
mydata$worry_4 <- recode_factor(mydata$worry, '1'='never','2'='less_week','3'='more_week','4'='daily')
mydata$depression_4 <- recode_factor(mydata$depression, '1'='never','2'='less_week','3'='more_week','4'='daily')
mydata$apathy_4 <- recode_factor(mydata$apathy, '1'='never','2'='less_week','3'='more_week','4'='daily')
mydata$loneliness_4 <- recode_factor(mydata$loneliness, '1'='never','2'='less_week','3'='more_week','4'='daily')

mydata$anxiety_2 <- recode_factor(mydata$anxiety, '1'='no','2'='yes','3'='yes','4'='yes')
mydata$worry_2 <- recode_factor(mydata$worry, '1'='no','2'='yes','3'='yes','4'='yes')
mydata$depression_2 <- recode_factor(mydata$depression, '1'='no','2'='yes','3'='yes','4'='yes')
mydata$apathy_2 <- recode_factor(mydata$apathy, '1'='no','2'='yes','3'='yes','4'='yes')
mydata$loneliness_2 <- recode_factor(mydata$loneliness, '1'='no','2'='yes','3'='yes','4'='yes')

psych_var <- c('happiness','life_satisfaction','anxiety_4','worry_4','depression_4','apathy_4','loneliness_4')


# health variables

mydata$physical_health_3 <- recode_factor(mydata$physical_health, '1'='best_good','2'='best_good','3'='normal','4'='bad_worst','5'='bad_worst')
mydata$chronic_disease_3 <- recode_factor(mydata$chronic_disease, '0'='no','1'='less_6','2'='less_6','3'='more_6')
mydata$disability_2 <- recode_factor(mydata$disability, '2'='no','1'='yes')

health_var <- c('physical_health_3','chronic_disease_3','disability_2')

## check for missing values
na_count <-sapply(mydata, function(y) sum(is.na(y)))
print(na_count <- data.frame(na_count))


# check the distribution
summary(factor(mydata$chronic_disease))
summary(mydata$smoking_increased)
unique(mydata$friends)

corr_plot <- ggplot(mydata, aes(x=offline_friends, y=as.numeric(smoking_increased))) + 
  geom_point(color='steelblue1', alpha=1) +
  geom_smooth(method=lm, se=T, color='steelblue') + 
  scale_x_continuous(name = "X", breaks = c(0,20,40,60)) +
  scale_y_continuous(name = "Y")
corr_plot

normal_variable <- mydata$friends
hist(normal_variable, breaks=80)
ggplot(mydata, aes(x=normal_variable)) + geom_density() # density curve

CrossTable(mydata$employment_type1, mydata$employment_type2)

chisq.test(mydata$smoking_increased, mydata$basicincome)

summary(mydata[mydata$online_friends>20, ]$age)

summary(factor(mydata$employment_4))

CrossTable(mydata$worry_2, mydata$anxiety_2)

mydata %>%
  filter(!is.na(smoking_change_cat)) %>%
  filter(employment_4=='self_family') %>%
  dplyr::select(age)


# check for multicollinearity
cor(mydata[c('age','online_friends','offline_friends', 'happiness','life_satisfaction',
             'income_household')], use='pairwise.complete.obs')



# # box cox transformation
# Box <-  boxcox(mydata$age ~ 1,            # Transform Turbidity as a single vector
#              lambda = seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
# Cox <-  data.frame(Box$x, Box$y)          # Create a data frame with the results
# Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
# Cox2[1,]                                  # Display the lambda with the greatest log likelihood
# lambda <-  Cox2[1, "Box.x"]               # Extract that lambda
# mywvs6$age_box <-  (mywvs6$age ^ lambda - 1)/lambda # Transform the original data









