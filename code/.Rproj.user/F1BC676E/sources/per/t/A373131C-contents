### title: World Values Survey Data Preparation
### authors: Sun Y. Lee
### dates: 3/10/20

# packages
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

# import the data 
wvs6 <- readRDS("C:\\Users\\USER\\Desktop\\Backup\\Harvard\\1_Harvard School Works\\Research\\WVS\\F00007762-WV6_Data_R_v20180912.rds")

# create my dataset and renames columns
mywvs6 <- wvs6 %>% 
  dplyr::rename(country=V2, id=V3, # levels 
                health=V11, happiness=V10, life_satisfaction=V23, # outcomes
                income=V239, sex=V240, socialclass=V238, age=V242, # main covariates
                education=V248, maritalstatus=V57, 
                year=V262,  # auxillary variables for imputation and other manipulation
                imp_religion=V9, imp_family=V4, trust=V24, 
                financial_satisfaction=V59, meaning_purpose=V143,
                weight=V258) %>% 
  dplyr::select(c(country, id, 
           health, happiness, life_satisfaction, 
           income, sex, socialclass, age,
           education, maritalstatus, 
           year,
           imp_religion, imp_family, trust,
           financial_satisfaction, meaning_purpose,
           weight))
  
# categorical variables
level_var <- c("country", "id")
mywvs6[,level_var] <- lapply(mywvs6[,level_var], as.factor)
cat_var <- c("health", "happiness", "income", "sex", "socialclass", "education",
             "maritalstatus", "year", 'imp_religion',
             "imp_family", "trust", "financial_satisfaction", "meaning_purpose")
mywvs6[,cat_var] <- lapply(mywvs6[,cat_var], as.factor)

lapply(mywvs6[,cat_var], summary)

# continuous variable
cont_var <- c("age","life_satisfaction","weight")
mywvs6[,cont_var] <- lapply(mywvs6[,cont_var], as.numeric)

lapply(mywvs6[,cont_var], summary)

# all variables
all_var <- c(level_var, cont_var, cat_var)

# drop missing values (-5=missing, -4=not asked, -3=not applicable, -2=no answer, -1=don't know)
mywvs6[,all_var] <- lapply(mywvs6[,all_var], as_numeric)
mywvs6 <- mywvs6 %>%
  mutate_at(all_var, list(~ifelse(.==-5 | .==-4 | .==-3 | .==-2 | .==-1, NA, .)))
## check for missing values
na_count <-sapply(mywvs6[,all_var], function(y) sum(is.na(y)))
print(na_count <- data.frame(na_count))
## countries did not collect information on some variables. Insert each variable to check.
mywvs6 %>% 
  group_by(country) %>% 
  summarize(missing=sum(is.na(age)), N=n() ) %>% 
  as_tibble  # %>% View
## checking balance across countries after removing missing values
mywvs6$country <- as.factor(mywvs6$country)
wvs_missingcheck <- mywvs6[complete.cases(mywvs6), ]
summary(as.factor(wvs_missingcheck$country))

# make categorical variables
mywvs6$age_cat <- cut(mywvs6$age, c(15,20,25,30,35,40,45,50,55,60,65,70,75,80,103),
                      labels = c("16-20","21-25","26-30","31-35","36-40","41-45",
                                 "46-50","51-55","56-60","61-65","66-70","71-75",
                                 "76-80","81-103"))
mywvs6$happiness2 <- as.factor(ifelse(mywvs6$happiness == 1 | mywvs6$happiness == 2, 0, 1))
mywvs6$health2 <- as.factor(ifelse(mywvs6$health == 1 | mywvs6$health == 2, 0, 1))

cat_var <- c(cat_var, "age_cat", "happiness2", "health2") 
mywvs6[,cat_var] <- lapply(mywvs6[,cat_var], as.factor) # make them categorical before using fct_recode

# modify categorical levels
mywvs6$sex_m <- fct_recode(mywvs6$sex, "male"="1", "female"="2")
mywvs6$maritalstatus_m <- fct_recode(mywvs6$maritalstatus, "married"="1",
                                   "couple"="2", "SWD"="3", "SWD"="4", 
                                   "SWD"="5", "single"="6")
mywvs6$education_m <- fct_recode(mywvs6$education, "none"="1", "none"="2",
                               "primary"="3","primary"="4","primary"="6",
                               "secondary"="5","secondary"="7","someuni"="8",
                               "uni"="9")
mywvs6$income_m <- fct_recode(mywvs6$income, "1st"="1", "1st"="2",
                            "2nd"="3","2nd"="4","3rd"="5",
                            "3rd"="6","4th"="7","4th"="8",
                            "5th"="9", "5th"="10")
mywvs6$socialclass_m <- fct_recode(mywvs6$socialclass,"upper"="1", "upmid"="2",
                                 "lowmid"="3","working"="4","lower"="5" )
lapply(mywvs6[,cat_var], summary)

# name each country
levels(mywvs6$country)
levels(mywvs6$country) <- list(Algeria="12", Azerbaijan="31", Argentina="32", Australia="36",
                               Armenia="51", Brazil="76", Belarus="112", Chile="152",
                               China="156", Taiwan="158", Colombia="170", Cyprus="196",
                               Ecuador="218", Estonia="233", Georgia="268", Palestine="275",
                               Germany="276", Ghana="288", Haiti="332", Hong_Kong="344",
                               India="356", Iraq="368", Japan="392", Kazakhstan="398",
                               Jordan="400", South_Korea="410", Kuwait="414", Kyrgyzstan="417",
                               Lebanon="422", Libya="434", Malaysia="458", Mexico="484",
                               Morocco="504", Netherlands="528", New_Zealand="554", Nigeria="566",
                               Pakistan="586", Peru="604", Philippines="608", Poland="616",
                               Qatar="634", Romania="642", Russia="643", Rwanda="646",
                               Singapore="702", Slovenia="705", South_Africa="710", Zimbabwe="716",
                               Spain="724", Sweden="752", Thailand="764", Trinidad_and_Tobago="780",
                               Tunisia="788", Turkey="792", Ukraine="804", Egypt="818",
                               United_States="840", Uruguay="858", Uzbekistan="860", Yemen="887")

# not included in the codebook: Netherlands=528, Qatar=634, Slovenia=705, Spain=724,
#                               Trinidad_and_Tobago=780. Confirmed by sample size

# Load the SWIID
load("C:\\Users\\USER\\Desktop\\Backup\\Harvard\\1_Harvard School Works\\Research\\WVS\\Income inequality\\swiid8_2\\swiid8_2.rda")

# add gini from swiid_summary (mean of 100 swiid datasets) to mywvs6 (main dataset). This does not take uncertainty in gini meausre into account.
swiid_summary$country <- swiid_summary$country %>%
  as.factor %>%
  fct_recode("Palestine" = "Palestinian Territories", "Hong_Kong" = "Hong Kong",
             "South_Korea" = "Korea", "New_Zealand" = "New Zealand",
             "South_Africa" = "South Africa", "Trinidad_and_Tobago" = "Trinidad and Tobago",
             "United_States" = "United States")

mywvs6$gini <- NA
for (i in levels(mywvs6$country)) {
  data_subset <- mywvs6[mywvs6$country==i,]
  swiid_subset <- swiid_summary[swiid_summary$country==i,]
  year_list <- unique(as_numeric(swiid_subset$year))
  giniyear <- unique(as_numeric(data_subset$year))
  year_location <- which(abs(year_list-giniyear)==min(abs(unique(year_list-giniyear))))
  mywvs6$gini[mywvs6$country==i] <- mean(swiid_subset$gini_disp[swiid_subset$year==swiid_subset$year[year_location]])
}

country_gini <- mywvs6 %>% group_by(country) %>% summarize(gini=mean(gini))
country_gini <- as.data.frame(country_gini)
country_gini$gini_c <- country_gini$gini-mean(country_gini$gini, na.rm=T)
country_gini$gini_tertile <- quantcut(country_gini$gini, q=3)
summary(country_gini$gini_tertile)

test <- country_gini %>% filter(gini_tertile=="[23.9,33.5]")
countries_gini_1st <- unique(test$country)
test <- country_gini %>% filter(gini_tertile=="(33.5,40.2]")
countries_gini_2nd <- unique(test$country)
test <- country_gini %>% filter(gini_tertile=="(40.2,59.4]")
countries_gini_3rd <- unique(test$country)

# add GDP to each country (World Bank GDP per capita, PPP (constant 2011 international $))
mywvs6$gdp <- NA
mywvs6$gdp[mywvs6$country=="Algeria"] <- 13557.7697574639 # Year 2014
mywvs6$gdp[mywvs6$country=="Argentina"] <- 19637.75538 # Year 2013
mywvs6$gdp[mywvs6$country=="Armenia"] <- 7019.767748 # Year 2011
mywvs6$gdp[mywvs6$country=="Australia"] <- 42854.89078 # Year 2012
mywvs6$gdp[mywvs6$country=="Azerbaijan"] <- 15754.15236   # Year 2011
mywvs6$gdp[mywvs6$country=="Belarus"] <- 17166.69564 # Year 2011
mywvs6$gdp[mywvs6$country=="Brazil"] <- 15480.86796 # Year 2014
mywvs6$gdp[mywvs6$country=="Colombia"] <- 11996.21652 # Year 2012
mywvs6$gdp[mywvs6$country=="Cyprus"] <- 33192.3369 # Year 2011
mywvs6$gdp[mywvs6$country=="Chile"] <- 21219.17655 # Year 2012
mywvs6$gdp[mywvs6$country=="China"] <- 11919.6071 # Year 2013
mywvs6$gdp[mywvs6$country=="Ecuador"] <- 10634.27789 # Year 2013
mywvs6$gdp[mywvs6$country=="Egypt"] <- 9970.975766 # Year 2013
mywvs6$gdp[mywvs6$country=="Estonia"] <- 24780.97878 # Year 2011
mywvs6$gdp[mywvs6$country=="Georgia"] <- 9179.19 # Year 2014
mywvs6$gdp[mywvs6$country=="Germany"] <- 42705.79153 # Year 2013
mywvs6$gdp[mywvs6$country=="Ghana"] <- 3595.641184 # Year 2012
mywvs6$gdp[mywvs6$country=="Haiti"] <- 1655.294377 # Year 2016
mywvs6$gdp[mywvs6$country=="Hong_Kong"] <- 52789.43908 # Year 2014
mywvs6$gdp[mywvs6$country=="India"] <- 4817.197501 # Year 2012
mywvs6$gdp[mywvs6$country=="Iraq"] <- 15556.95879 # Year 2013
mywvs6$gdp[mywvs6$country=="Japan"] <- 35749.75668 # Year 2010
mywvs6$gdp[mywvs6$country=="Jordan"] <- 8634.535737 # Year 2014
mywvs6$gdp[mywvs6$country=="Kazakhstan"] <- 21276.93402   # Year 2011
mywvs6$gdp[mywvs6$country=="Kuwait"] <- 72588.67068 # Year 2014
mywvs6$gdp[mywvs6$country=="Kyrgyzstan"] <- 2920.60321 # Year 2011
mywvs6$gdp[mywvs6$country=="Lebanon"] <- 12833.3615 # Year 2013
mywvs6$gdp[mywvs6$country=="Libya"] <- 15965.49175 # Year 2014
mywvs6$gdp[mywvs6$country=="Malaysia"] <- 22670.2933 # Year 2012
mywvs6$gdp[mywvs6$country=="Mexico"] <- 16891.49269 # Year 2012
mywvs6$gdp[mywvs6$country=="Morocco"] <- 6703.876831 # Year 2011
mywvs6$gdp[mywvs6$country=="Netherlands"] <- 45948.53628 # Year 2012
mywvs6$gdp[mywvs6$country=="New_Zealand"] <- 32734.53667 # Year 2011
mywvs6$gdp[mywvs6$country=="Nigeria"] <- 5290.630106 # Year 2012
mywvs6$gdp[mywvs6$country=="Pakistan"] <- 4148.942547 # Year 2012
mywvs6$gdp[mywvs6$country=="Palestine"] <-  2727 # Year 2013; GDP from GapMinder (constant 2011 international $)
mywvs6$gdp[mywvs6$country=="Peru"] <- 11185.80881 # Year 2012
mywvs6$gdp[mywvs6$country=="Philippines"] <- 5967.489234 # Year 2012
mywvs6$gdp[mywvs6$country=="Poland"] <- 23218.1113 # Year 2012
mywvs6$gdp[mywvs6$country=="Qatar"] <- 119973.5535 # Year 2010
mywvs6$gdp[mywvs6$country=="Romania"] <- 18361.20494 # Year 2012
mywvs6$gdp[mywvs6$country=="Russia"] <- 24310.0439699999 # Year 2011
mywvs6$gdp[mywvs6$country=="Rwanda"] <- 1575.879672 # Year 2012
mywvs6$gdp[mywvs6$country=="Singapore"] <- 77492.6312 # Year 2012
mywvs6$gdp[mywvs6$country=="Slovenia"] <- 28931.38371 # Year 2011
mywvs6$gdp[mywvs6$country=="South_Korea"] <- 30352.10482 # Year 2010
mywvs6$gdp[mywvs6$country=="South_Africa"] <- 12357.69869 # Year 2013
mywvs6$gdp[mywvs6$country=="Spain"] <- 31867.97324 # Year 2011
mywvs6$gdp[mywvs6$country=="Sweden"] <- 44503.67324   # Year 2011
mywvs6$gdp[mywvs6$country=="Taiwan"] <-  39482 # Year 2012; GDP from GapMinder (constant 2011 international $)
mywvs6$gdp[mywvs6$country=="Thailand"] <- 14771.15548 # Year 2013
mywvs6$gdp[mywvs6$country=="Trinidad_and_Tobago"] <- 31259.80079 # Year 2010
mywvs6$gdp[mywvs6$country=="Tunisia"] <- 10638.62722 # Year 2013
mywvs6$gdp[mywvs6$country=="Turkey"] <- 20259.43813 # Year 2012
mywvs6$gdp[mywvs6$country=="Ukraine"] <- 8281.867126 # Year 2011
mywvs6$gdp[mywvs6$country=="United_States"] <- 49883.11398 # Year 2011
mywvs6$gdp[mywvs6$country=="Uruguay"] <- 17993.44754 # Year 2011
mywvs6$gdp[mywvs6$country=="Uzbekistan"] <- 5502.049022 # Year 2011
mywvs6$gdp[mywvs6$country=="Yemen"] <- 3828.484887 # Year 2014
mywvs6$gdp[mywvs6$country=="Zimbabwe"] <- 2486.84793 # Year 2012


mywvs6$gdp <- mywvs6$gdp/10000
# mywvs6$gdp <- log(mywvs6$gdp)
cont_var <- c(cont_var, "gdp")

country_gdp <- mywvs6 %>% group_by(country) %>% summarize(gdp=mean(gdp))
country_gdp <- as.data.frame(country_gdp)
country_gdp$gdp_c <- country_gdp$gdp-mean(country_gdp$gdp, na.rm=T)
country_gdp$gdp_tertile <- quantcut(country_gdp$gdp, q=3)
summary(country_gdp$gdp_tertile)
country_gdp$gdp_lowmid <- as.factor(ifelse(country_gdp$country %in% countries_gdp_low |country_gdp$country %in% countries_gdp_lmid , 1, 0))
country_gdp$gdp_umid <- as.factor(ifelse(country_gdp$country %in% countries_gdp_umid, 1, 0))
country_gdp$gdp_high <- as.factor(ifelse(country_gdp$country %in% countries_gdp_high, 1, 0))
country_gdp$gdp_cat[country_gdp$gdp_lowmid=="1"] <- 1
country_gdp$gdp_cat[country_gdp$gdp_umid=="1"] <- 2
country_gdp$gdp_cat[country_gdp$gdp_high=="1"] <- 3
country_gdp$gdp_cat <- as.factor(country_gdp$gdp_cat)
summary(country_gdp$gdp_cat)


# add GNI to each country (World Bank GNI per capita, PPP (constant 2011 international $))
mywvs6$gni <- NA
mywvs6$gni[mywvs6$country=="Algeria"] <- 13163.154915275 # Year 2014
mywvs6$gni[mywvs6$country=="Argentina"] <- 19227.7418081629 # Year 2013
mywvs6$gni[mywvs6$country=="Armenia"] <- 7268.98932583388 # Year 2011
mywvs6$gni[mywvs6$country=="Australia"] <- 41570.5577589425 # Year 2012
mywvs6$gni[mywvs6$country=="Azerbaijan"] <- 14593.2795658638   # Year 2011
mywvs6$gni[mywvs6$country=="Belarus"] <- 16815.0091661502 # Year 2011
mywvs6$gni[mywvs6$country=="Brazil"] <- 15176.8884224417 # Year 2014
mywvs6$gni[mywvs6$country=="Colombia"] <- 11538.4697258698 # Year 2012
mywvs6$gni[mywvs6$country=="Cyprus"] <- 34191.0344959577 # Year 2011
mywvs6$gni[mywvs6$country=="Chile"] <- 20247.1384242747 # Year 2012
mywvs6$gni[mywvs6$country=="China"] <- 11823.4892401472 # Year 2013
mywvs6$gni[mywvs6$country=="Ecuador"] <- 10482.8080934178 # Year 2013
mywvs6$gni[mywvs6$country=="Egypt"] <- 9711.96221899217 # Year 2013
mywvs6$gni[mywvs6$country=="Estonia"] <- 23313.0396187217 # Year 2011
mywvs6$gni[mywvs6$country=="Georgia"] <- 9100.69334643431 # Year 2014
mywvs6$gni[mywvs6$country=="Germany"] <- 43698.5996885001 # Year 2013
mywvs6$gni[mywvs6$country=="Ghana"] <- 3407.56779205057 # Year 2012
mywvs6$gni[mywvs6$country=="Haiti"] <- 1664.52300469439 # Year 2016
mywvs6$gni[mywvs6$country=="Hong_Kong"] <- 53885.8577728435 # Year 2014
mywvs6$gni[mywvs6$country=="India"] <- 4760.39162184495 # Year 2012
mywvs6$gni[mywvs6$country=="Iraq"] <- 13679.1138617637 # Year 2011 (Year 2013 unavailable)
mywvs6$gni[mywvs6$country=="Japan"] <- 36684.8782525601 # Year 2010
mywvs6$gni[mywvs6$country=="Jordan"] <- 8534.96235426196 # Year 2014
mywvs6$gni[mywvs6$country=="Kazakhstan"] <- 18214.6434295493   # Year 2011
mywvs6$gni[mywvs6$country=="Kuwait"] <- 78877.8762893428 # Year 2014
mywvs6$gni[mywvs6$country=="Kyrgyzstan"] <- 2610.053585442 # Year 2011
mywvs6$gni[mywvs6$country=="Lebanon"] <- 12771.0607452071 # Year 2013
mywvs6$gni[mywvs6$country=="Libya"] <- 11113.9189903547 # Year 2011 (Year 2014 not available)
mywvs6$gni[mywvs6$country=="Malaysia"] <- 21832.9476185258 # Year 2012
mywvs6$gni[mywvs6$country=="Mexico"] <- 16567.6502943344 # Year 2012
mywvs6$gni[mywvs6$country=="Morocco"] <- 6590.79631781545 # Year 2011
mywvs6$gni[mywvs6$country=="Netherlands"] <- 46735.5722663038 # Year 2012
mywvs6$gni[mywvs6$country=="New_Zealand"] <- 31252.2174305537 # Year 2011
mywvs6$gni[mywvs6$country=="Nigeria"] <- 5017.172110983 # Year 2012
mywvs6$gni[mywvs6$country=="Pakistan"] <- 4359.68085088021 # Year 2012
mywvs6$gni[mywvs6$country=="Palestine"] <- 5090 # Year 2013; GDP from GapMinder
mywvs6$gni[mywvs6$country=="Peru"] <- 10496.6926560657 # Year 2012
mywvs6$gni[mywvs6$country=="Philippines"] <- 7178.8737437091 # Year 2012
mywvs6$gni[mywvs6$country=="Poland"] <- 22357.5415079635 # Year 2012
mywvs6$gni[mywvs6$country=="Qatar"] <- 109354.336890911 # Year 2010
mywvs6$gni[mywvs6$country=="Romania"] <- 18044.0905493013 # Year 2012
mywvs6$gni[mywvs6$country=="Russia"] <- 23594.3737056731 # Year 2011
mywvs6$gni[mywvs6$country=="Rwanda"] <- 1555.69105614351 # Year 2012
mywvs6$gni[mywvs6$country=="Singapore"] <- 74094.2669297519 # Year 2012
mywvs6$gni[mywvs6$country=="Slovenia"] <- 28538.632826851 # Year 2011
mywvs6$gni[mywvs6$country=="South_Korea"] <- 30386.8360544423 # Year 2010
mywvs6$gni[mywvs6$country=="South_Africa"] <- 12037.7831155003 # Year 2013
mywvs6$gni[mywvs6$country=="Spain"] <- 31305.6058762271 # Year 2011
mywvs6$gni[mywvs6$country=="Sweden"] <- 45415.8988719449 # Year 2011
mywvs6$gni[mywvs6$country=="Taiwan"] <-  21922 # Year 2012; National Statistics Republic of China (Taiwan)
mywvs6$gni[mywvs6$country=="Thailand"] <- 13839.5536328014 # Year 2013
mywvs6$gni[mywvs6$country=="Trinidad_and_Tobago"] <- 27545.5342744522 # Year 2011 (Year 2010 not available)
mywvs6$gni[mywvs6$country=="Tunisia"] <- 10126.497519297 # Year 2013
mywvs6$gni[mywvs6$country=="Turkey"] <- 20106.0423160928 # Year 2012
mywvs6$gni[mywvs6$country=="Ukraine"] <- 8089.18490698852 # Year 2011
mywvs6$gni[mywvs6$country=="United_States"] <- 50812.6533881704 # Year 2011
mywvs6$gni[mywvs6$country=="Uruguay"] <- 17381.935761536 # Year 2011
mywvs6$gni[mywvs6$country=="Uzbekistan"] <- 5519.59663533872 # Year 2011
mywvs6$gni[mywvs6$country=="Yemen"] <- 3599.44479380964 # Year 2011 (Year 2014 not available)
mywvs6$gni[mywvs6$country=="Zimbabwe"] <- 2157.64929058614 # Year 2012

# mywvs6$gni <- log(mywvs6$gni)
mywvs6$gni <- mywvs6$gni/10000
cont_var <- c(cont_var, "gni")

country_gni <- mywvs6 %>% group_by(country) %>% summarize(gni=mean(gni))
country_gni <- as.data.frame(country_gni)
country_gni$gni_c <- country_gni$gni-mean(country_gni$gni, na.rm=T)
country_gni$gni_tertile <- quantcut(country_gni$gni, q=3)
country_gni$gdp_lowmid <- as.factor(ifelse(country_gni$country %in% countries_gdp_low |country_gni$country %in% countries_gdp_lmid , 1, 0))
country_gni$gdp_umid <- as.factor(ifelse(country_gni$country %in% countries_gdp_umid, 1, 0))
country_gni$gdp_high <- as.factor(ifelse(country_gni$country %in% countries_gdp_high, 1, 0))

summary(country_gni$gni_tertile)

# World Bank classification of countries by GNI
countries_gdp_low <- c("Haiti","Kyrgyzstan","Rwanda","Zimbabwe")
countries_gdp_lmid <- c("Armenia","Egypt","Georgia","Ghana","India","Morocco",
                        "Nigeria","Pakistan","Palestine","Philippines","Ukraine",
                        "Uzbekistan","Yemen")
countries_gdp_umid <- c("Algeria","Argentina","Azerbaijan","Belarus","Brazil",
                        "Colombia","China","Ecuador","Iraq","Jordan","Kazakhstan",
                        "Lebanon","Libya","Malaysia","Mexico","Peru","Romania",
                        "Russia","South_Africa","Thailand","Tunisia","Turkey",
                        "Uruguay")
countries_gdp_high <- c("Australia","Cyprus","Chile","Estonia","Germany","Hong_Kong",
                        "Japan","Kuwait","Netherlands","New_Zealand","Poland",
                        "Qatar","Singapore","Slovenia","South_Korea","Spain",
                        "Sweden","Taiwan","Trinidad_and_Tobago","United_States")

# World Bank classification of countries by region
countries_ea_p <- c("Australia","China","Hong_Kong","Japan","South_Korea","Malaysia",
                    "New_Zealand","Philippines","Singapore","Taiwan","Thailand")
countries_eu_ca <- c("Armenia","Azerbaijan","Belarus","Cyprus","Georgia","Germany",
                     "Kazakhstan","Kyrgyzstan","Netherlands","Poland","Romania","Russia",
                     "Slovenia","Spain","Sweden","Turkey","Ukraine","Estonia","Uzbekistan",
                     "United_States") # US included here because it is the only North American country.
countries_sa<- c("India","Pakistan")
countries_la_cb <- c("Argentina","Brazil","Chile","Colombia","Ecuador","Haiti","Mexico",
                     "Peru","Trinidad_and_Tobago","Uruguay")
countries_me_na <- c("Algeria","Egypt","Iraq","Jordan","Kuwait","Lebanon","Libya","Morocco",
                     "Qatar","Tunisia","Yemen","Palestine") # included Palestine here. Palestine is not classified by World Bank.
countries_ssa <- c("Ghana","Nigeria","South_Africa","Rwanda","Zimbabwe")


mywvs6$region[mywvs6$country %in% countries_ea_p] <- 1
mywvs6$region[mywvs6$country %in% countries_eu_ca] <- 2
mywvs6$region[mywvs6$country %in% countries_sa] <- 3
mywvs6$region[mywvs6$country %in% countries_la_cb] <- 4
mywvs6$region[mywvs6$country %in% countries_me_na] <- 5
mywvs6$region[mywvs6$country %in% countries_ssa] <- 6
mywvs6$region <- as.factor(mywvs6$region)

mywvs6$region <- fct_recode(mywvs6$region, "EAsia/Pacific"="1", "Eu/CAsia"="2",
                                 "SAsia"="3","Latin"="4","MEast/NAfrica"="5",
                                 "SSAfrica"="6")

# derived continuous variables
mywvs6$gini_c <- mywvs6$gini-mean(mywvs6$gini, na.rm=T)
mywvs6$gini_1st <- as.factor(ifelse(mywvs6$gini>=23.9 & mywvs6$gini<=33.5, 1, 0))
mywvs6$gini_2nd <- as.factor(ifelse(mywvs6$gini>33.5 & mywvs6$gini<=40.2, 1, 0))
mywvs6$gini_3rd <- as.factor(ifelse(mywvs6$gini>40.2 & mywvs6$gini<=59.4, 1, 0))
mywvs6$gini_tertile <- quantcut(mywvs6$gini, q=3)

mywvs6$lngdp <- log(mywvs6$gdp)
mywvs6$lngdp_c <- mywvs6$lngdp-mean(mywvs6$lngdp, na.rm=T)
mywvs6$gdp_c <- mywvs6$gdp-mean(mywvs6$gdp, na.rm=T)
mywvs6$gdp_c_2 <- mywvs6$gdp_c^2
mywvs6$gdp_1st <- as.factor(ifelse(mywvs6$gdp>=0.157 & mywvs6$gdp<=1.1, 1, 0))
mywvs6$gdp_2nd <- as.factor(ifelse(mywvs6$gdp>1.1 & mywvs6$gdp<=2.29, 1, 0))
mywvs6$gdp_3rd <- as.factor(ifelse(mywvs6$gdp>2.29 & mywvs6$gdp<=12, 1, 0))
mywvs6$gdp_low <- as.factor(ifelse(mywvs6$country %in% countries_gdp_low, 1, 0))
mywvs6$gdp_lmid <- as.factor(ifelse(mywvs6$country %in% countries_gdp_lmid, 1, 0))
mywvs6$gdp_umid <- as.factor(ifelse(mywvs6$country %in% countries_gdp_umid, 1, 0))
mywvs6$gdp_high <- as.factor(ifelse(mywvs6$country %in% countries_gdp_high, 1, 0))
mywvs6$gdp_lowmid <- as.factor(ifelse(mywvs6$country %in% countries_gdp_low |mywvs6$country %in% countries_gdp_lmid , 1, 0)) # combine the low and lower middle categories because the low income category only have two countries.

mywvs6$gni_c <- mywvs6$gni-mean(mywvs6$gni, na.rm=T)
mywvs6$gni_1st <- as.factor(ifelse(mywvs6$gni>=0.156 & mywvs6$gni<=1.05, 1, 0))
mywvs6$gni_2nd <- as.factor(ifelse(mywvs6$gni>1.05 & mywvs6$gni<=2.19, 1, 0))
mywvs6$gni_3rd <- as.factor(ifelse(mywvs6$gni>2.19 & mywvs6$gni<=10.9, 1, 0))
mywvs6$lngni <- log(mywvs6$gni)
mywvs6$lngni_c <- mywvs6$lngdp-mean(mywvs6$lngni, na.rm=T)

mywvs6$age_c <- mywvs6$age-mean(mywvs6$age, na.rm=T)
mywvs6$age_c_2 <- mywvs6$age_c^2

mywvs6$gdp_low <- as.factor(ifelse(mywvs6$country %in% countries_gdp_low, 1, 0))
mywvs6$gdp_lmid <- as.factor(ifelse(mywvs6$country %in% countries_gdp_lmid, 1, 0))
mywvs6$gdp_umid <- as.factor(ifelse(mywvs6$country %in% countries_gdp_umid, 1, 0))
mywvs6$gdp_high <- as.factor(ifelse(mywvs6$country %in% countries_gdp_high, 1, 0))
mywvs6$gdp_lowmid <- as.factor(ifelse(mywvs6$country %in% countries_gdp_low |mywvs6$country %in% countries_gdp_lmid , 1, 0))
mywvs6$gdp_cat[mywvs6$gdp_lowmid=="1"] <- 1
mywvs6$gdp_cat[mywvs6$gdp_umid=="1"] <- 2
mywvs6$gdp_cat[mywvs6$gdp_high=="1"] <- 3

# create a country level dataset
country_life_satisfaction <- mywvs6 %>% group_by(country) %>% summarize(life_satisfaction=mean(life_satisfaction, na.rm=T))
country_health <- mywvs6 %>% group_by(country) %>% summarize(n=n(),
                                                             health=sum(as_numeric(health2)/n, na.rm=T))

mywvs6_country <- tibble(country=country_gdp$country, gdp=country_gdp$gdp, 
                         lngdp=log(country_gdp$gdp), gni=country_gni$gni,
                         lngni=log(country_gni$gni), gini=country_gini$gini, 
                         health=country_health$health,
                         life_satisfaction=country_life_satisfaction$life_satisfaction,)

# reorganize variable lists and check
cont_var <- c(cont_var, "gini_c", "gdp_c", "gni_c", "age_c", "age_c_2")
cat_var <- c(cat_var, 'gini_1st','gini_2nd','gini_3rd','gdp_1st','gdp_2nd','gdp_3rd',
             'gdp_low','gdp_lmid','gdp_umid','gdp_high','gdp_cat','gini_tertile')
mywvs6[,level_var] <- lapply(mywvs6[,level_var], as.factor)
mywvs6[,cont_var] <- lapply(mywvs6[,cont_var], as.numeric)
mywvs6[,cat_var] <- lapply(mywvs6[,cat_var], as.factor)

all_var <- c(level_var, cont_var, cat_var)
lapply(mywvs6[,all_var], summary)






### check normality of variables and transform ###
# skim
# skim(select(mywvs6, c(cat_var, cont_var)))

# histogram
# ggplot(mywvs6, aes(x=mywvs6$life_satisfaction)) + geom_histogram() 

# # Box-Cox transformation of age
# Box <-  boxcox(mywvs6$age ~ 1,            # Transform Turbidity as a single vector
#              lambda = seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
# Cox <-  data.frame(Box$x, Box$y)          # Create a data frame with the results
# Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
# Cox2[1,]                                  # Display the lambda with the greatest log likelihood
# lambda <-  Cox2[1, "Box.x"]               # Extract that lambda
# mywvs6$age_box <-  (mywvs6$age ^ lambda - 1)/lambda # Transform the original data
# 
# # Box-Cox transformation of life_satisfaction
# Box <-  boxcox(mywvs6$life_satisfaction ~ 1,            # Transform Turbidity as a single vector
#                lambda = seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
# Cox <-  data.frame(Box$x, Box$y)          # Create a data frame with the results
# Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
# Cox2[1,]                                  # Display the lambda with the greatest log likelihood
# lambda <-  Cox2[1, "Box.x"]               # Extract that lambda
# mywvs6$life_satisfaction_box <-  (mywvs6$life_satisfaction ^ lambda - 1)/lambda # Transform the original data
# 
# # Box-Cox transformation of income
# Box <-  boxcox(mywvs6$income ~ 1,            # Transform Turbidity as a single vector
#                lambda = seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
# Cox <-  data.frame(Box$x, Box$y)          # Create a data frame with the results
# Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
# Cox2[1,]                                  # Display the lambda with the greatest log likelihood
# lambda <-  Cox2[1, "Box.x"]               # Extract that lambda
# mywvs6$income_box <-  (mywvs6$income ^ lambda - 1)/lambda # Transform the original data

# sort by country and individuals
mywvs6 <- mywvs6 %>% arrange(country, id)

# table one
## unweighted
tableone_all_vars <- c('age','sex','income','education','socialclass','maritalstatus','life_satisfaction','gdp',
                   'gini')
tableone_cat_vars <- c('sex','income','education','socialclass','maritalstatus')
table1_gdp <- CreateTableOne(data=mywvs6, vars=tableone_all_vars, factorVars=tableone_cat_vars, strata='gdp_cat',
                             includeNA=T)
write.csv(print(table1_gdp, catDigits=1,contDigits=2,missing=T,test=F,showAllLevels=T,noSpaces=T), file='table1_gdp.csv')
table1_gini <- CreateTableOne(data=mywvs6, vars=tableone_all_vars, factorVars=tableone_cat_vars, strata='gini_tertile',
                              includeNA=T)
write.csv(print(table1_gini, catDigits=1,contDigits=2,missing=T,test=F,showAllLevels=T,noSpaces=T), file='table1_gini.csv')
table1_all <- CreateTableOne(data=mywvs6, vars=tableone_all_vars, factorVars=tableone_cat_vars, includeNA=T)
write.csv(print(table1_all, catDigits=1,contDigits=2,missing=T,test=F,showAllLevels=T,noSpaces=T), file='table1_all.csv')
## weighted
mywvs6_svy <- svydesign(id=~1, strata=~country,weights=~weight,data=mywvs6)
svytable1_gdp <- svyCreateTableOne(data=mywvs6_svy, vars=tableone_all_vars, factorVars=tableone_cat_vars, strata='gdp_cat',
                             includeNA=T)
write.csv(print(svytable1_gdp, catDigits=1,contDigits=2,missing=T,test=F,showAllLevels=T,noSpaces=T), file='svytable1_gdp.csv')
svytable1_gini <- svyCreateTableOne(data=mywvs6_svy, vars=tableone_all_vars, factorVars=tableone_cat_vars, strata='gini_tertile',
                              includeNA=T)
write.csv(print(svytable1_gini, catDigits=1,contDigits=2,missing=T,test=F,showAllLevels=T,noSpaces=T), file='svytable1_gini.csv')
svytable1_all <- svyCreateTableOne(data=mywvs6_svy, vars=tableone_all_vars, factorVars=tableone_cat_vars, includeNA=T)
write.csv(print(svytable1_all, catDigits=1,contDigits=2,missing=T,test=F,showAllLevels=T,noSpaces=T), file='svytable1_all.csv')

# create my own functions
## fix R2MLwiN::sixway so that it presents all plots in the rmarkdown knitted document. Using this does not pick up MCSE and BD functions so these are manually added.
sixway2 <- function(chain, name = NULL, acf.maxlag = 100, pacf.maxlag = 10, ...) {
  args <- list(...)
  
  isMCMC <- TRUE
  if (!coda::is.mcmc(chain) && !coda::is.mcmc.list(chain)) {
    isMCMC <- FALSE
    chain <- coda::mcmc(chain)
  }
  
  if (coda::nvar(chain) > 1) {
    stop("Cannot plot more than one parameter at a time")
  }
  
  if (coda::is.mcmc.list(chain) && coda::nchain(chain) > 1) {
    warning("Sixway does not fully support multiple chains - diagnostics will be on concatenated chains")
  }
  
  if (length(args) > 0 && "mar" %in% names(args)) {
    mar <- args[["mar"]]
  } else {
    mar <- c(4, 4, 2, 1)/2
  }
  
  if (length(args) > 0 && "mgp" %in% names(args)) {
    mgp <- args[["mgp"]]
  } else {
    mgp <- c(1, 0.25, 0)
  }
  
  if (is.null(name)) {
    if (!is.null(coda::varnames(chain))) {
      name <- coda::varnames(chain)[1]
    } else {
      name <- "x"
    }
  }
  
  if (.Platform$GUI == "RStudio") {
    o = tolower(Sys.info()["sysname"])
    a = switch(o,
               "darwin"  = "quartz",
               "linux"   = "x11",
               "windows" = "windows")
    options("device" = a)
  }
  mypar <- graphics::par(mar = mar, mgp = mgp, bg="white", no.readonly=TRUE, ...)
  on.exit(graphics::par(mypar))
  graphics::split.screen(figs = c(4, 1))
  graphics::split.screen(figs = c(1, 2), screen = 1)
  graphics::split.screen(figs = c(1, 2), screen = 2)
  graphics::split.screen(figs = c(1, 2), screen = 3)
  graphics::split.screen(figs = c(1, 1), screen = 4)
  
  graphics::screen(5)
  coda::traceplot(chain, xlab = "Iterations", ylab = "parameter", type = "l", tcl = -0.1, cex.axis = 0.8, main = "")
  
  flatchain <- as.matrix(chain)
  
  graphics::screen(6)
  dens <- stats::density(flatchain)
  graphics::plot(dens, xlab = "parameter value", ylab = "kernel density", main = "", tcl = -0.1, cex.axis = 0.8)
  
  graphics::screen(7)
  aa <- stats::acf(flatchain, acf.maxlag, main = "", mgp = c(1, 0.25, 0), tcl = -0.1, cex.axis = 0.8, ylim = c(0, 1))
  rho <- aa$acf[2]
  
  graphics::screen(8)
  stats::pacf(flatchain, pacf.maxlag, main = "", mgp = c(1, 0.25, 0), tcl = -0.1, cex.axis = 0.8, ylim = c(0, 1))
  
  graphics::screen(9)
  mcse <- MCSE(flatchain, rho, ll = 0.5, ul = 20)
  graphics::plot(mcse[, 1], mcse[, 2], type = "l", xlab = "updates", ylab = "MCSE", tcl = -0.1, cex.axis = 0.8)
  
  RL1 <- coda::raftery.diag(flatchain, q = 0.025, r = 0.005, s = 0.95, converge.eps = 0.001)
  RL2 <- coda::raftery.diag(flatchain, q = 0.975, r = 0.005, s = 0.95, converge.eps = 0.001)
  Ndb <- BD(mean(flatchain), stats::var(flatchain), rho, k = 2, alpha = 0.05)
  
  graphics::screen(10)
  graphics::plot(1, xlim = c(1, 10), ylim = c(1, 5), type = "n", axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)
  graphics::text(5, 4.8, "Accuracy Diagnostics", cex = 1.2)
  if (RL1$resmatrix[1] == "Error") {
    graphics::text(5, 4, paste("RL diagnostic only available after ", RL1$resmatrix[2], " updates.", sep = ""), cex = 0.8)
  } else {
    graphics::text(5, 4, paste("Raftery-Lewis (quantile) : Nhat =(", RL1$resmatrix[1, "N"], ",", RL2$resmatrix[1, "N"],
                               ")", sep = ""), cex = 0.8)
  }
  graphics::text(5, 3, "when q=(0.025,0.975), r=0.005 and s=0.95", cex = 0.8)
  graphics::text(5, 2.1, paste("Brooks-Draper (mean) : Nhat =", Ndb), cex = 0.8)
  graphics::text(5, 1.2, "when k=2 sigfigs and alpha=0.05", cex = 0.8)
  graphics::screen(11)
  graphics::plot(1, xlim = c(1, 22), ylim = c(1, 4), type = "n", axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)
  graphics::text(10, 3.8, "Summary Statistics", cex = 1.2)
  quants <- round(stats::quantile(flatchain, c(0.025, 0.05, 0.5, 0.95, 0.975)), 3)
  graphics::text(10, 2.9, paste("param name :", name, "posterior mean =", round(mean(flatchain), 3), "SD = ", round(stats::sd(flatchain),
                                                                                                                    3), "mode =", round(dens$x[which.max(dens$y)], 3)), cex = 0.8)
  graphics::text(10, 2, paste("quantiles : 2.5% =", quants[1], "5% =", quants[2], "50% =", quants[3], "95% =", quants[4],
                              "97.5% =", quants[5]), cex = 0.8)
  if (isMCMC) {
    graphics::text(10, 1.2, paste(coda::niter(chain) * coda::thin(chain), "actual iterations storing every ", paste(coda::thin(chain), "th",
                                                                                                                    sep = ""), " iteration. Effective Sample Size (ESS) =", round(coda::effectiveSize(chain))), cex = 0.8)
  } else {
    graphics::text(10, 1.2, paste(length(chain), "actual iterations. Diagnostics assume storing every 1th iteration. Effective Sample Size (ESS) =",
                                  round(coda::effectiveSize(chain))), cex = 0.8)
  }
  graphics::close.screen(all.screens = TRUE)
}
MCSE <- function(chain, rho, ll = 0.5, ul = 20) {
  chain_var <- stats::var(chain)
  if (coda::is.mcmc(chain)) {
    runlength <- stats::end(chain) - (stats::start(chain) - 1)
  } else {
    runlength <- length(chain)
  }
  if (ul < ll) {
    temp <- ll
    ll <- ul
    ul <- temp
  }
  if (ul - ll < 1e-04) {
    ul <- 20
    ll <- 0.5
  }
  ll <- ll * runlength
  ul <- ul * runlength
  mult <- (sqrt((1 + rho)/(1 - rho))) * sqrt(chain_var)
  
  mcsepoints <- 1000
  mcse <- ll + ((ul - ll) * ((0:(mcsepoints - 1))/mcsepoints))
  updates <- matrix(mult, mcsepoints, 1)/sqrt(mcse)
  cbind(mcse, updates)
}
BD <- function(est, var, rho, k = 2, alpha = 0.05) {
  # Based on an upublished paper by David Draper
  ceiling(4 * stats::qnorm(1 - alpha/2)^2 * (sqrt(var)/(10^(floor(log10(abs(est))) - k + 1)))^2 * (1 + rho)/(1 - rho))
} 
## create a glance function to store coeff and vcov for mice function
# tidy.mlwinfitIGLS <- function(x, conf.int = FALSE, conf.level = .95, ...) 
{
#   est <- coef(x)
#   term <- names(est)
#   sd <- sqrt(diag(vcov(x)))
#   zscore <- est / sd
#   pval <- 2 * stats::pnorm(abs(zscore), lower.tail = FALSE)
#   ret <- tibble::tibble(term=term, estimate=est, std.error=sd, statistic=zscore, p.value=pval)
#   
#   if (conf.int) {
#     conf <- plyr::unrowname(confint(x, level = conf.level))
#     colnames(conf) <- c("conf.low", "conf.high")
#     ret <- cbind(ret, conf)
#   }
#   ret
}
# glance.mlwinfitIGLS <- function(x, ...) 
{
  # tibble::tibble(
  #   logLik = stats::logLik(x),
  #   AIC = stats::AIC(x),
  #   BIC = stats::BIC(x),
  #   df.residual = stats::df.residual(x), 
  #   nobs = stats::nobs(x)
  # )
}


