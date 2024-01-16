library(dplyr)
library(tidyverse)
library(openintro)
install.packages("pacman")

pacman::p_load(tidyverse, rmarkdown, 
               tinytex, ISLR, ISLR2, 
               openintro, opendatatoronto,
               causaldata)

#Load the dataset smoking from the openintro package
smoking_data<- openintro::smoking

#Select only the gender, age and smoke variables
select(smoking_data,gender,age,smoke)

#Select only the numerical variables and arrange the age variable in the descending order
select(smoking_data,where(is.numeric))

#Select all variables except the ethnicity and nationality variables
select(smoking_data,-ethnicity,-nationality)

#With the smoking dataset, rename the highest_qualification variable as education
rename(smoking_data,education=highest_qualification)

#Create a new variable to calculate the total number of cigarettes in a week (adding weekends and weekdays)
smoking_data<-mutate(smoking_data,total_cigs=amt_weekdays+amt_weekends)

#In the smoking dataset, filter males
filter(smoking_data,gender=="Male")

#From the people who do not smoke, how many are 35 years old?
filter(smoking_data,age==35 & smoke=="No")

#Filter cases where the respondents is neither Divorced nor English
filter(smoking_data, marital_status!="Divorced"& nationality!="English")

#Filter only cases where age is below 20 and if the person smokes
filter(smoking_data, age<=20 & smoke=="Yes")

#How many people who report that they smoke do not have any qualifications?
filter(smoking_data, smoke=="Yes" & highest_qualification=="No Qualification")

#Filter cases where the respondents is either from London or Wales
filter(smoking_data,region=="London" | region=="Wales")

#In the smoking dataset, select only the highest_qualification and smoke_weekdays variables, rename the variables appropriately and calculate the total number of cigarettes smoke by people in each qualification
smoking_data|>
  select(highest_qualification,amt_weekdays)|>
  rename(education=highest_qualification,cigs_weekdays=amt_weekdays)|>
  group_by(education)|>
  summarise(total_cigs_education=sum(cigs_weekdays,na.rm=T))

#Find the number of people who smoke below the age of thirty and arrange the results in decreasing order of age
filter(smoking_data,smoke=="Yes", age <=30)

#Select the income and smoke_weekend variables and calculate the total number of cigarettes smoke in each income category
smoking_data|>
  select(gross_income,amt_weekdays)|>
  group_by(gross_income)|>
  summarise(total_cigs_inc=sum(amt_weekdays,na.rm=T))

#What is the mean age of males and females?
smoking_data|>
  group_by(gender)|>
  summarise(mean_age=mean(age))

#Create age categories for the smoking dataset like 15-25, 26-40, 40-59 and 59+
smoking_data|>
  mutate(age_group=case_when(age>=15&
                               age<=25~"15-25",
                             age>=26&
                               age<=40~"26-40",
                             age>=41&
                               age<=59~"40-59",
                             age>59~"59+")
         )

#Convert the gender variable into a numeric one, with female as 1 and male as 0
smoking_data|>
  mutate(gender_code=case_when(gender=="Male"~"0",
                            gender=="Female"~"1")
         )|>view()

#Create smoking categories as ‘High’ (for more than 30), ‘Medium’(11-30) and ‘Low’ (0-10) with the smoke_weekends variable
smoking_data|>
  mutate(smoker_level=case_when(amt_weekends>=0&amt_weekends<=10~"Low",
                                amt_weekends>=11&amt_weekends<=30~"Medium",
                                amt_weekends>30~"High")
         )|>view()
