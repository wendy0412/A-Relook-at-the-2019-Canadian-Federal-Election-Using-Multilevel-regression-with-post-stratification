library(tidyverse)
library(labelled)
install.packages("devtools")
devtools::install_github("hodgettsp/cesR",force = TRUE)
library(cesR)
get_ces("ces2019_web")
ces2019<- to_factor(ces2019_web)
head(ces2019)

survey_data<-ces2019%>%
  select(cps19_votechoice,
         cps19_gender,
         cps19_province,
         cps19_education,
         cps19_age)

#change variable name 
survey_data<-rename(survey_data,sex=cps19_gender)
survey_data<-rename(survey_data,province=cps19_province)
survey_data<-rename(survey_data,education=cps19_education)
survey_data<-rename(survey_data,age=cps19_age)

#remove another party, dk, na
#remove non-binary gender
survey_data<-na.omit(survey_data)
survey_reduced<-survey_data%>%
  filter((cps19_votechoice== "Green Party"|
           cps19_votechoice=="Conservative Party"|
            cps19_votechoice=="Liberal Party"|
            cps19_votechoice=="ndp"|
            cps19_votechoice=="People's Party"|
            cps19_votechoice=="Bloc Québécois"),
         sex!= "Other (e.g. Trans, non-binary, two-spirit, gender-queer)")

survey_reduced$sex<-ifelse(survey_reduced$sex=="A woman","Female","Male")

survey_reduced<-survey_reduced %>% 
  mutate(age = case_when(age >= 25  & age <= 34 ~ '25 to 34',
                             age >=35  & age <= 44 ~ '35 to 44',
                             age >=45  & age <= 54 ~ '45 to 54',
                             age >=55  & age <= 64 ~ '55 to 64'
                             )) 
#remove age not 25-64
survey_reduced<-na.omit(survey_reduced)

#education
survey_reduced<-survey_reduced %>% 
  filter(education!="Don't know/ Prefer not to answer")

survey_reduced$education<-ifelse(survey_reduced$education=="Completed elementary school"|
                                   survey_reduced$education=="Some elementary school"|
                                   survey_reduced$education=="No schooling","no degree",
                                 ifelse(survey_reduced$education=="Completed secondary/ high school"|
                                          survey_reduced$education=="Some secondary/ high school","high school",
                                        ifelse(survey_reduced$education=="Completed technical, community college, CEGEP, College Classique"|
                                                 survey_reduced$education=="Some technical, community college, CEGEP, College Classique",
                                               "technical,community college","university")))

#create cells
survey_reduced$cell<-paste(survey_reduced$sex,survey_reduced$age)

#factor
survey_reduced[c("age","sex","province","cell","education","cps19_votechoice")] <- lapply(survey_reduced[c("age","sex","province","cell","education","cps19_votechoice")], factor) 

survey_reduced$cps19_votechoice <- relevel(survey_reduced$cps19_votechoice, ref = "Conservative Party") 



write_csv(survey_reduced, "/Users/wendyzeng/Desktop/304 final project/survey_data.csv")
