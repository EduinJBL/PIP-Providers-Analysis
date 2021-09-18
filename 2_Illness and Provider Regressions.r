library(statxplorer)
library(tidyverse)
library(readxl)
library(jtools)

#Compile as much detail about clearances as we can from StatXplore
# Set API key from a file

setwd("C:\\Users\\eblat\\OneDrive\\Documents\\Pro Bono Economics")
#ADD YOUR OWN STATXPLORE KEY
load_api_key("Inputs//statxplore_api_key.txt")

setwd("C:\\Users\\eblat\\OneDrive\\Documents\\Pro Bono Economics\\PBE-Project-Analysis\\Providers Analysis")

codes<-read_csv("LA Code Lookup.csv")%>%as_tibble()%>%
  select(LAD19CD,LAD19NM)%>%rename(ons_code=LAD19CD,
                                   authority=LAD19NM)

#Do full analysis three different times: once with reassessments,
#once with new cases, once with both.

#Read Disability Data
results<-fetch_table(filename="DisabilityandReassessmentbyLA2.json")
disdf<-results$dfs$`PIP Clearances`
disdf<-disdf%>%rename(authority=1,reassess=2)

#Read Award Status
results<-fetch_table(filename="TypeofAwardbyReassessment2.json")
awards<-results$dfs$`PIP Clearances`
awards<-awards%>%rename(authority=1,reassess=2)

processing<-function(disdf,awards){
  disdf<-disdf%>%spread(Disability,`PIP Clearances`)
  disdf<-disdf%>%mutate(across(where(is.double)&!c("Total"),~.x/Total))
  #disdf<-disdf%>%select(-starts_with("Unknown"))
  disdf<-disdf%>%left_join(codes,by="authority")%>%relocate(ons_code)
  disdf<-disdf%>%rename(autoimmune=`Autoimmune disease (connective tissue disorders)`,
                        cardio=`Cardiovascular disease`,
                        immune=`Diseases of the immune system`,
                        liver=`Diseases of the liver, gallbladder, biliary tract`,
                        endo=`Endocrine disease`,
                        gastro=`Gastrointestinal disease`,
                        genit=`Genitourinary disease`,
                        haem=`Haematological Disease`,
                        hearing=`Hearing disorders`,
                        infect=`Infectious disease`,
                        malignant=`Malignant disease`,
                        metabolic=`Metabolic disease`,
                        musc_gen=`Musculoskeletal disease (general)`,
                        musc_reg=`Musculoskeletal disease (regional)`,
                        neuro=`Neurological disease`,
                        psych=`Psychiatric disorders`,
                        resp=`Respiratory disease`,
                        visual=`Visual disease`,
                        skin=`Skin disease`)
  
  
  awards<-awards%>%spread(`Clearance Type Detail`,`PIP Clearances`)
  awards<-awards%>%mutate(across(where(is.double)&!c("Total"),~.x/Total))
  
  
  #Join together into df
  df<-disdf%>%left_join(awards,by="authority")
  
  #Join data on provider
  providers<-read_csv("1_Providers_by_LA.csv")
  df<-df%>%left_join(providers,by="ons_code")
  
  #join IMD data
  IMD_data<-read_excel("IMD_LTLA Data.xlsx",
                       sheet="IMD")%>%as_tibble()
  IMD_data<-IMD_data%>%rename(ons_code=1,
                              authority=2,
                              IMD_score=5,
                              IMD_score_rank=6)%>%
    select(ons_code,
           contains("IMD_"))
  
  df<-df%>%left_join(IMD_data,by="ons_code")
  
  return(df)
}


#Now clean disability data taking only one reassessment indicator
for (var in c("Reassessment","Not Reassessment","Total")){
  #filter two dfs so only include relevant cases
  disdf2<-disdf%>%filter(reassess=={{var}})
  awards2<-awards%>%filter(reassess=={{var}})

  #Process both, joining IMD and providers data
  df<-processing(disdf2,awards2)
  #Run Regressions
  #Just explain accepted pct by disease cat
  m1<-lm(Awarded~cardio+autoimmune+liver+endo+gastro+genit+haem+hearing+infect+malignant+
           metabolic+musc_gen+musc_reg+neuro+psych+resp+visual+skin,data=df)
  #now just provider
  m2<-lm(Awarded~main_provider,data=df)
  #print(summary(m2))
  
  m3<-lm(Awarded~main_provider+cardio+autoimmune+liver+endo+gastro+genit+haem+hearing+infect+malignant+
           metabolic+musc_gen+musc_reg+neuro+psych+resp+visual+skin,data=df)
  #print(summary(m3))
  
  m4<-lm(Awarded~main_provider+IMD_score+cardio+autoimmune+liver+endo+gastro+genit+haem+hearing+infect+malignant+
           metabolic+musc_gen+musc_reg+neuro+psych+resp+visual+skin,data=df)
  
  export_summs(m2,m3,m4,
                     model.names=c("No Disability Controls","Disability controls",
                                  "Disability Controls and IMD"),to.file="docx",
                     file.name=paste(var,"Regression Table.docx"))
}



#Exploration of whether unsuccessful cases are more likely to have unknown diseases
dfCap<-df%>%filter(main_provider=="Capita")
dfATOS<-df%>%filter(main_provider!="Capita")
m7<-lm(Awarded~`Unknown or missing.x`,data=dfCap)
summ(m7)

m8<-lm(Awarded~`Unknown or missing.x`,data=dfATOS)
summ(m8)
#check whether capita is more likely to report disability as unknown
#which could bias their figure down. Turns out Capita is less likely 
#to report disability status as unknown than ATOS, so this bias is unlikely
#There could still be biases about how they report illnesses

m9<-lm(`Unknown or missing.x`~main_provider,data=df)
summ(m9)
