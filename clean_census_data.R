library(tidyverse)
census_data2016<-read_csv('/Users/wendyzeng/Desktop/304 final project/rawcensus.csv')

educ_count<-c("Total - Highest certificate, diploma or degree (2016 counts)",
                   "No certificate, diploma or degree (2016 counts)",
                   "Secondary (high) school diploma or equivalency certificate (2016 counts)",
                   "Apprenticeship or trades certificate or diploma (2016 counts)",
                   "College, CEGEP or other non-university certificate or diploma (2016 counts)",
                   "University certificate or diploma below bachelor level (2016 counts)")
census_data<-census_data2016 %>% select(c("Age","Sex","Geographic name",educ_count))%>%
  pivot_longer(cols=educ_cols_count,
               names_to='education',values_to="total_count")

census_data<-rename(census_data,age=Age)
census_data<-rename(census_data,sex=Sex)
census_data<-rename(census_data,province='Geographic name')

census_reduced<-census_data%>%
  filter(province != "Canada",sex!="Both sexes",(age=="25 to 34"| age=="35 to 44"|
                                                   age=="45 to 54"| age=="55 to 64"))

#education
census_reduced<-census_reduced%>%
  filter(education!="Total - Highest certificate, diploma or degree (2016 counts)" )
census_reduced$education<-ifelse(census_reduced$education=="No certificate, diploma or degree (2016 counts)","no degree",
                                 ifelse(census_reduced$education=="Secondary (high) school diploma or equivalency certificate (2016 counts)","high school",
                                        ifelse(census_reduced$education=="University certificate or diploma below bachelor level (2016 counts)",
                                               "university","technical,community college")))

#create cells
census_reduced$cell<-paste(census_reduced$sex,census_reduced$age)

census_reduced[c("age","sex","province","cell","education")] <- lapply(census_reduced[c("age","sex","province","cell","education")], factor) 


write_csv(census_reduced, "/Users/wendyzeng/Desktop/304 final project/census_data.csv")
