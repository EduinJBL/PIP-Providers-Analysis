#4_Match authorities to different providers
library(tidyverse)
library(stringr)
postcodelookup<-as_tibble(read_csv("Inputs\\NSPL_NOV_2019_UK\\Data\\NSPL_NOV_2019_UK.csv"))
setwd("C:\\Users\\eblat\\OneDrive\\Documents\\Pro Bono Economics\\PBE-Project-Analysis\\Providers Analysis")
providers<-as_tibble(read_csv("Capita Provider Postcodes.csv"))
#create version of prefixes only with letters
providers<-providers%>%mutate(letters_prefix=gsub('[[:digit:]]+', '', 
                                                  postcode_prefix),
                              prefix_length=nchar(postcode_prefix))
#find similar letter prefixes in postcode lookup data
postcodelookup<-postcodelookup%>%mutate(letters_prefix=gsub("^(.+?)(?=\\d).*", "\\1", pcds
                                                            , perl = TRUE),
                                        postcode_nospace=str_replace_all(pcds, fixed(" "), ""))
#join lookup with providers data
postcodelookup<-postcodelookup%>%left_join(providers,by="letters_prefix")

#Seperate the difficult cases where the postcode prefix i
#includes letters as well. Deal with seperately and bind
postcodelookup<-postcodelookup%>%mutate(prefix_in_LU_data=substr(postcode_nospace,1,prefix_length))
diffcases<-postcodelookup%>%filter(only_letters==FALSE)
diffcases<-diffcases%>%mutate(provider=ifelse(prefix_in_LU_data==postcode_prefix,"Capita","ATOS"))
#bind diffcases data back to lookup
postcodelookup<-postcodelookup%>%filter(only_letters==TRUE|
                                        is.na(only_letters))%>%
                bind_rows(diffcases)
#For cases not matched set ATOS as provider
postcodelookup<-postcodelookup%>%mutate(provider=ifelse(is.na(provider),"ATOS",
                                                        provider))%>%
  select(provider,pcds,laua)

df<-postcodelookup%>%mutate(count=1)%>%spread(provider,count)

df<-postcodelookup%>%group_by(laua)%>%summarise(numberofprovs=n_distinct(provider))

df2<-postcodelookup%>%count(laua,provider)%>%
  group_by(laua)%>%slice(which.max(n))%>%select(laua,provider)
df<-df%>%left_join(df2,by="laua")%>%rename(main_provider=provider)
df<-df%>%drop_na(laua)%>%rename(ons_code=laua)

write_csv(df,"1_Providers_by_LA.csv")                                        
                                        