library(dplyr)
library(readxl)
library(survey)
library(haven)
library(kableExtra)

setwd("XX SP conflict manuscript data")
#Time spent on sociopolitical news via traditional and social media/online during the 2014 Occupy Central
#weights file
sp_merged<-read_excel('wave 1 to cs19.xlsx')%>%rename(wave=Wave)
#Wave 3
sp1_rawA<-read.csv('Wave 3/SP1_ListA_mainsample.csv')
sp1_rawB<-read.csv('Wave 3/SP1_ListB_subsample.csv')
sp1_raw<-rbind(sp1_rawA,sp1_rawB)
sp_merged1<-sp_merged%>%filter(wave=="oc")
sp_merged1<-sp_merged1%>%left_join(.,sp1_raw%>%select(member_id,Q1.1,Q1.2,Q1.3),by="member_id")
sp_merged1<-sp_merged1%>%mutate(Q1.1=ifelse(Q1.1>1440,NA,Q1.1))
sp_merged1<-sp_merged1%>%mutate(Q1.2=ifelse(Q1.2>1440,NA,Q1.2))
sp_merged1<-sp_merged1%>%mutate(Q1.3=ifelse(Q1.3>1440,NA,Q1.3))
sp_merged1<-sp_merged1%>%mutate(group="Total")
sp_merged1<-sp_merged1%>%mutate(Q1.1_cat=case_when(Q1.1<60 ~ 0, 
                                                   Q1.1>=60 ~ 1))
sp_merged1<-sp_merged1%>%mutate(Q1.2_cat=case_when(Q1.2<60 ~ 0, 
                                                   Q1.2>=60 ~ 1))
sp_merged1<-sp_merged1%>%mutate(Q1.3_cat=case_when(Q1.3<60 ~ 0, 
                                                   Q1.3>=60 ~ 1))
#Wave 4
sp2_raw_fu<-read.csv('Wave 4/SP2_ListAB_FU_932case.csv')%>%mutate(grp="fu")
sp2_raw_pu<-read.csv('Wave 4/SP2_PatchUp_302case.csv')%>%mutate(Q8.3=NA)%>%mutate(grp="pu")
sp2_raw<-rbind(sp2_raw_fu%>%select(member_id,Ref_No,Q8.1,Q8.2,Q8.3,grp),
               sp2_raw_pu%>%select(member_id,Ref_No,Q9.1,Q9.2,Q9.3,grp)%>%rename(Q8.1=Q9.1,Q8.2=Q9.2,Q8.3=Q9.3))
sp_merged2<-sp_merged%>%filter(wave=="oc2")
sp_merged2<-sp_merged2%>%left_join(.,sp2_raw%>%select(member_id,Q8.1,Q8.2,Q8.3,grp),by="member_id")
sp_merged2<-sp_merged2%>%mutate(Q8.1=ifelse(Q8.1>1440,NA,Q8.1))
sp_merged2<-sp_merged2%>%mutate(Q8.2=ifelse(Q8.2>1440,NA,Q8.2))
sp_merged2<-sp_merged2%>%mutate(Q8.3=ifelse(Q8.3>1440,NA,Q8.3))
sp_merged2<-sp_merged2%>%mutate(group="Total")
sp_merged2<-sp_merged2%>%mutate(Q8.1_cat=case_when(Q8.1<60 ~ 0, 
                                                   Q8.1>=60 ~ 1))
sp_merged2<-sp_merged2%>%mutate(Q8.2_cat=case_when(Q8.2<60 ~ 0, 
                                                   Q8.2>=60 ~ 1))
sp_merged2<-sp_merged2%>%mutate(Q8.3_cat=case_when(Q8.3<60 ~ 0, 
                                                   Q8.3>=60 ~ 1))

wtd_prev <- function(data,outcome){
  out <- questionr::wtd.table(data[outcome],weights = data['weights']) %>% data.frame()
  out <- binom.test(round(out[out$Var1==1,]$Freq,0),round(sum(out$Freq),0))
  est <- out$estimate[1]
  ll <- out$conf.int[1]
  ul <- out$conf.int[2]
  prev = paste0(sprintf("%.1f",(round(est*100,1)))," (",sprintf("%.1f",round(ll*100,1)),", ",sprintf("%.1f",round(ul*100,1)),")")
  data.frame(est = est, ll = ll, ul = ul, prev = prev)
}


rbind(
  cbind( c("Wave 3","","",""),
         c("Overall","18-39","40-59","≥60"),
    c((svyby(~Q1.3, by = ~group, design = svydesign(ids = ~1, data = sp_merged1%>%filter(!is.na(Q1.3)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
  mutate(CI=paste(round(Q1.3,1)," (",round(Q1.3-1.96*se,1),", ",round(Q1.3+1.96*se,1), ")", sep = "")))$CI,
(svyby(~Q1.3, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged1%>%filter(!is.na(Q1.3)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
  mutate(CI=paste(round(Q1.3,1)," (",round(Q1.3-1.96*se,1),", ",round(Q1.3+1.96*se,1), ")", sep = ""))%>%
  rename(Var=agegroup))$CI),

c(wtd_prev(sp_merged1,'Q1.3_cat')$prev,
wtd_prev(sp_merged1%>%filter(agegroup=="[0,40)"),'Q1.3_cat')$prev,
wtd_prev(sp_merged1%>%filter(agegroup=="[40,60)"),'Q1.3_cat')$prev,
wtd_prev(sp_merged1%>%filter(agegroup=="[60,150)"),'Q1.3_cat')$prev),

c((svyby(~Q1.1, by = ~group, design = svydesign(ids = ~1, data = sp_merged1%>%filter(!is.na(Q1.1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
     mutate(CI=paste(round(Q1.1,1)," (",round(Q1.1-1.96*se,1),", ",round(Q1.1+1.96*se,1), ")", sep = "")))$CI,
  (svyby(~Q1.1, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged1%>%filter(!is.na(Q1.1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
     mutate(CI=paste(round(Q1.1,1)," (",round(Q1.1-1.96*se,1),", ",round(Q1.1+1.96*se,1), ")", sep = ""))%>%
     rename(Var=agegroup))$CI),

c(wtd_prev(sp_merged1,'Q1.1_cat')$prev,
  wtd_prev(sp_merged1%>%filter(agegroup=="[0,40)"),'Q1.1_cat')$prev,
  wtd_prev(sp_merged1%>%filter(agegroup=="[40,60)"),'Q1.1_cat')$prev,
  wtd_prev(sp_merged1%>%filter(agegroup=="[60,150)"),'Q1.1_cat')$prev),

c((svyby(~Q1.2, by = ~group, design = svydesign(ids = ~1, data = sp_merged1%>%filter(!is.na(Q1.2)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
     mutate(CI=paste(round(Q1.2,1)," (",round(Q1.2-1.96*se,1),", ",round(Q1.2+1.96*se,1), ")", sep = "")))$CI,
  (svyby(~Q1.2, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged1%>%filter(!is.na(Q1.2)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
     mutate(CI=paste(round(Q1.2,1)," (",round(Q1.2-1.96*se,1),", ",round(Q1.2+1.96*se,1), ")", sep = ""))%>%
     rename(Var=agegroup))$CI),

c(wtd_prev(sp_merged1,'Q1.2_cat')$prev,
  wtd_prev(sp_merged1%>%filter(agegroup=="[0,40)"),'Q1.2_cat')$prev,
  wtd_prev(sp_merged1%>%filter(agegroup=="[40,60)"),'Q1.2_cat')$prev,
  wtd_prev(sp_merged1%>%filter(agegroup=="[60,150)"),'Q1.2_cat')$prev)),

cbind( c("Wave 4","","",""),
       c("Overall","18-39","40-59","≥60"),
  c((svyby(~Q8.3, by = ~group, design = svydesign(ids = ~1, data = sp_merged2%>%filter(!is.na(Q8.3)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
            mutate(CI=paste(round(Q8.3,1)," (",round(Q8.3-1.96*se,1),", ",round(Q8.3+1.96*se,1), ")", sep = "")))$CI,
         (svyby(~Q8.3, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged2%>%filter(!is.na(Q8.3)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
            mutate(CI=paste(round(Q8.3,1)," (",round(Q8.3-1.96*se,1),", ",round(Q8.3+1.96*se,1), ")", sep = ""))%>%
            rename(Var=agegroup))$CI),
       
       c(wtd_prev(sp_merged2,'Q8.3_cat')$prev,
         wtd_prev(sp_merged2%>%filter(agegroup=="[0,40)"),'Q8.3_cat')$prev,
         wtd_prev(sp_merged2%>%filter(agegroup=="[40,60)"),'Q8.3_cat')$prev,
         wtd_prev(sp_merged2%>%filter(agegroup=="[60,150)"),'Q8.3_cat')$prev),
       
       c((svyby(~Q8.1, by = ~group, design = svydesign(ids = ~1, data = sp_merged2%>%filter(!is.na(Q8.1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
            mutate(CI=paste(round(Q8.1,1)," (",round(Q8.1-1.96*se,1),", ",round(Q8.1+1.96*se,1), ")", sep = "")))$CI,
         (svyby(~Q8.1, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged2%>%filter(!is.na(Q8.1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
            mutate(CI=paste(round(Q8.1,1)," (",round(Q8.1-1.96*se,1),", ",round(Q8.1+1.96*se,1), ")", sep = ""))%>%
            rename(Var=agegroup))$CI),
       
       c(wtd_prev(sp_merged2,'Q8.1_cat')$prev,
         wtd_prev(sp_merged2%>%filter(agegroup=="[0,40)"),'Q8.1_cat')$prev,
         wtd_prev(sp_merged2%>%filter(agegroup=="[40,60)"),'Q8.1_cat')$prev,
         wtd_prev(sp_merged2%>%filter(agegroup=="[60,150)"),'Q8.1_cat')$prev),
       
       c((svyby(~Q8.2, by = ~group, design = svydesign(ids = ~1, data = sp_merged2%>%filter(!is.na(Q8.2)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
            mutate(CI=paste(round(Q8.2,1)," (",round(Q8.2-1.96*se,1),", ",round(Q8.2+1.96*se,1), ")", sep = "")))$CI,
         (svyby(~Q8.2, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged2%>%filter(!is.na(Q8.2)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
            mutate(CI=paste(round(Q8.2,1)," (",round(Q8.2-1.96*se,1),", ",round(Q8.2+1.96*se,1), ")", sep = ""))%>%
            rename(Var=agegroup))$CI),
       
       c(wtd_prev(sp_merged2,'Q8.2_cat')$prev,
         wtd_prev(sp_merged2%>%filter(agegroup=="[0,40)"),'Q8.2_cat')$prev,
         wtd_prev(sp_merged2%>%filter(agegroup=="[40,60)"),'Q8.2_cat')$prev,
         wtd_prev(sp_merged2%>%filter(agegroup=="[60,150)"),'Q8.2_cat')$prev)))%>%data.frame()%>%
  kable(digits =1,col.names = c('','Age group (years)','Minutes per day (95% CI)','Proportion of spending ≥1 h per day (95% CI)','Minutes per day (95% CI)','Proportion of spending ≥1 h per day (95% CI)',
                                'Minutes per day (95% CI)','Proportion of spending ≥1 h per day (95% CI)'))%>%kable_classic()%>%
  add_header_above(c(" " = 2, "Social media/online" = 2, "Television" = 2, "Newspapers/radio"=2))

####################################################################################################
#Time spent on sociopolitical news via traditional and social media during the 2019 social unrest
#Wave 8
sp8_raw<-read_dta('Wave 8/sp8.dta')
sp_merged8<-sp_merged%>%filter(wave=="oc8")
sp_merged8<-sp_merged8%>%left_join(.,sp8_raw%>%select(member_id,sp1631_1,q13_hr,q13_min,q11_hr,q11_min,
                                                      q12_1_hr,q12_1_min,q12_2_hr,q12_2_min),by="member_id")
sp_merged8<-sp_merged8 %>% mutate(q13_hr=ifelse(sp1631_1==6 | sp1631_1==7,0,q13_hr),
                                  q13_min=ifelse(sp1631_1==6 | sp1631_1==7,0,q13_min))
sp_merged8<-sp_merged8 %>% mutate(sm = 60*q13_hr+q13_min,sm=ifelse(sm>1440,NA,sm),
                                  tv=60*q11_hr+q11_min,tv=ifelse(tv>1440,NA,tv),
                                  newspaper=60*q12_1_hr+q12_1_min,newspaper=ifelse(newspaper>1440,NA,newspaper),
                                  radio=60*q12_2_hr+q12_2_min,radio=ifelse(radio>1440,NA,radio),
                                  newspaper_radio=newspaper+radio,newspaper_radio=ifelse(newspaper_radio>1440,NA,newspaper_radio))
sp_merged8<-sp_merged8%>%mutate(group="Total")
sp_merged8<-sp_merged8%>%mutate(tv_cat=case_when(tv<60 ~ 0, 
                                                 tv>=60 ~ 1))
sp_merged8<-sp_merged8%>%mutate(newspaper_radio_cat=case_when(newspaper_radio<60 ~ 0, 
                                                              newspaper_radio>=60 ~ 1))
sp_merged8<-sp_merged8%>%mutate(sm_cat=case_when(sm<60 ~ 0, 
                                                 sm>=60 ~ 1))
#Wave 9
sp9_raw<-read_dta('Wave 9/sp9.dta')
sp_merged9<-sp_merged%>%filter(wave=="oc9")
sp_merged9<-sp_merged9%>%left_join(.,sp9_raw%>%select(member_id,sp1631a_1,q13_hr,q13_min,q11_hr,q11_min,
                                                      q12_1_hr,q12_1_min,q12_2_hr,q12_2_min),by="member_id")
sp_merged9<-sp_merged9 %>% mutate(q13_hr=ifelse(sp1631a_1==6 | sp1631a_1==7,0,q13_hr),
                                  q13_min=ifelse(sp1631a_1==6 | sp1631a_1==7,0,q13_min))
sp_merged9<-sp_merged9 %>% mutate(sm = 60*q13_hr+q13_min,sm=ifelse(sm>1440,NA,sm),
                                  tv=60*q11_hr+q11_min,tv=ifelse(tv>1440,NA,tv),
                                  newspaper=60*q12_1_hr+q12_1_min,newspaper=ifelse(newspaper>1440,NA,newspaper),
                                  radio=60*q12_2_hr+q12_2_min,radio=ifelse(radio>1440,NA,radio),
                                  newspaper_radio=newspaper+radio,newspaper_radio=ifelse(newspaper_radio>1440,NA,newspaper_radio))
sp_merged9<-sp_merged9%>%mutate(group="Total")
sp_merged9<-sp_merged9%>%mutate(tv_cat=case_when(tv<60 ~ 0, 
                                                 tv>=60 ~ 1))
sp_merged9<-sp_merged9%>%mutate(newspaper_radio_cat=case_when(newspaper_radio<60 ~ 0, 
                                                              newspaper_radio>=60 ~ 1))
sp_merged9<-sp_merged9%>%mutate(sm_cat=case_when(sm<60 ~ 0, 
                                                 sm>=60 ~ 1))

rbind(
  cbind( c("Wave 8","","",""),
         c("Overall","18-39","40-59","≥60"),
         c((svyby(~sm, by = ~group, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(sm)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(sm,1)," (",round(sm-1.96*se,1),", ",round(sm+1.96*se,1), ")", sep = "")))$CI,
           (svyby(~sm, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(sm)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(sm,1)," (",round(sm-1.96*se,1),", ",round(sm+1.96*se,1), ")", sep = ""))%>%
              rename(Var=agegroup))$CI),
         
         c(wtd_prev(sp_merged8,'sm_cat')$prev,
           wtd_prev(sp_merged8%>%filter(agegroup=="[0,40)"),'sm_cat')$prev,
           wtd_prev(sp_merged8%>%filter(agegroup=="[40,60)"),'sm_cat')$prev,
           wtd_prev(sp_merged8%>%filter(agegroup=="[60,150)"),'sm_cat')$prev),
         
         c((svyby(~tv, by = ~group, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(tv)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(tv,1)," (",round(tv-1.96*se,1),", ",round(tv+1.96*se,1), ")", sep = "")))$CI,
           (svyby(~tv, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(tv)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(tv,1)," (",round(tv-1.96*se,1),", ",round(tv+1.96*se,1), ")", sep = ""))%>%
              rename(Var=agegroup))$CI),
         
         c(wtd_prev(sp_merged8,'tv_cat')$prev,
           wtd_prev(sp_merged8%>%filter(agegroup=="[0,40)"),'tv_cat')$prev,
           wtd_prev(sp_merged8%>%filter(agegroup=="[40,60)"),'tv_cat')$prev,
           wtd_prev(sp_merged8%>%filter(agegroup=="[60,150)"),'tv_cat')$prev),
         
         c((svyby(~newspaper_radio, by = ~group, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(newspaper_radio)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(newspaper_radio,1)," (",round(newspaper_radio-1.96*se,1),", ",round(newspaper_radio+1.96*se,1), ")", sep = "")))$CI,
           (svyby(~newspaper_radio, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(newspaper_radio)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(newspaper_radio,1)," (",round(newspaper_radio-1.96*se,1),", ",round(newspaper_radio+1.96*se,1), ")", sep = ""))%>%
              rename(Var=agegroup))$CI),
         
         c(wtd_prev(sp_merged8,'newspaper_radio_cat')$prev,
           wtd_prev(sp_merged8%>%filter(agegroup=="[0,40)"),'newspaper_radio_cat')$prev,
           wtd_prev(sp_merged8%>%filter(agegroup=="[40,60)"),'newspaper_radio_cat')$prev,
           wtd_prev(sp_merged8%>%filter(agegroup=="[60,150)"),'newspaper_radio_cat')$prev)),
  
  cbind( c("Wave 9","","",""),
         c("Overall","18-39","40-59","≥60"),
         c((svyby(~sm, by = ~group, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(sm)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(sm,1)," (",round(sm-1.96*se,1),", ",round(sm+1.96*se,1), ")", sep = "")))$CI,
           (svyby(~sm, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(sm)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(sm,1)," (",round(sm-1.96*se,1),", ",round(sm+1.96*se,1), ")", sep = ""))%>%
              rename(Var=agegroup))$CI),
         
         c(wtd_prev(sp_merged9,'sm_cat')$prev,
           wtd_prev(sp_merged9%>%filter(agegroup=="[0,40)"),'sm_cat')$prev,
           wtd_prev(sp_merged9%>%filter(agegroup=="[40,60)"),'sm_cat')$prev,
           wtd_prev(sp_merged9%>%filter(agegroup=="[60,150)"),'sm_cat')$prev),
         
         c((svyby(~tv, by = ~group, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(tv)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(tv,1)," (",round(tv-1.96*se,1),", ",round(tv+1.96*se,1), ")", sep = "")))$CI,
           (svyby(~tv, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(tv)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(tv,1)," (",round(tv-1.96*se,1),", ",round(tv+1.96*se,1), ")", sep = ""))%>%
              rename(Var=agegroup))$CI),
         
         c(wtd_prev(sp_merged9,'tv_cat')$prev,
           wtd_prev(sp_merged9%>%filter(agegroup=="[0,40)"),'tv_cat')$prev,
           wtd_prev(sp_merged9%>%filter(agegroup=="[40,60)"),'tv_cat')$prev,
           wtd_prev(sp_merged9%>%filter(agegroup=="[60,150)"),'tv_cat')$prev),
         
         c((svyby(~newspaper_radio, by = ~group, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(newspaper_radio)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(newspaper_radio,1)," (",round(newspaper_radio-1.96*se,1),", ",round(newspaper_radio+1.96*se,1), ")", sep = "")))$CI,
           (svyby(~newspaper_radio, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(newspaper_radio)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
              mutate(CI=paste(round(newspaper_radio,1)," (",round(newspaper_radio-1.96*se,1),", ",round(newspaper_radio+1.96*se,1), ")", sep = ""))%>%
              rename(Var=agegroup))$CI),
         
         c(wtd_prev(sp_merged9,'newspaper_radio_cat')$prev,
           wtd_prev(sp_merged9%>%filter(agegroup=="[0,40)"),'newspaper_radio_cat')$prev,
           wtd_prev(sp_merged9%>%filter(agegroup=="[40,60)"),'newspaper_radio_cat')$prev,
           wtd_prev(sp_merged9%>%filter(agegroup=="[60,150)"),'newspaper_radio_cat')$prev)))%>%data.frame()%>%
  kable(digits =1,col.names = c('','Age group (years)','Minutes per day (95% CI)','Proportion of spending ≥1 h per day (95% CI)','Minutes per day (95% CI)','Proportion of spending ≥1 h per day (95% CI)',
                                'Minutes per day (95% CI)','Proportion of spending ≥1 h per day (95% CI)'))%>%kable_classic()%>%
  add_header_above(c(" " = 2, "Social media/online" = 2, "Television" = 2, "Newspapers/radio"=2))

####################################################################################################
# Proportion of posts of negative content on social media during 2019-20 social unrest
#Wave 8
sp_merged8<-sp_merged8%>%left_join(.,sp8_raw%>%select(member_id,sp1632_1),by="member_id")
sp_merged8<-sp_merged8%>%mutate(sp1632_1=ifelse(sp1632_1>100,NA,sp1632_1))
#Wave 9
sp_merged9<-sp_merged9%>%left_join(.,sp9_raw%>%select(member_id,sp1632_1),by="member_id")
sp_merged9<-sp_merged9%>%mutate(sp1632_1=ifelse(sp1632_1>100,NA,sp1632_1))


cbind(c("Overall","Age group (years)","18-39","40-59","≥60","Sex","Male","Female","Educational attainment",
        "Primary","Secondary","Tertiary"),
  
  c((svyby(~sp1632_1, by = ~group, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(sp1632_1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
  mutate(CI=paste(round(sp1632_1,1)," (",round(sp1632_1-1.96*se,1),",",round(sp1632_1+1.96*se,1), ")", sep = "")))$CI,
"",
(rbind(svyby(~sp1632_1, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(sp1632_1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
        mutate(CI=paste(round(sp1632_1,1)," (",round(sp1632_1-1.96*se,1),", ",round(sp1632_1+1.96*se,1), ")", sep = ""))%>%
        rename(Var=agegroup),
       "",
      svyby(~sp1632_1, by = ~sex, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(sp1632_1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
        mutate(CI=paste(round(sp1632_1,1)," (",round(sp1632_1-1.96*se,1),", ",round(sp1632_1+1.96*se,1), ")", sep = ""))%>%
        rename(Var=sex),
      "",
      svyby(~sp1632_1, by = ~edulevel, design = svydesign(ids = ~1, data = sp_merged8%>%filter(!is.na(sp1632_1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
        mutate(CI=paste(round(sp1632_1,1)," (",round(sp1632_1-1.96*se,1),", ",round(sp1632_1+1.96*se,1), ")", sep = ""))%>%
        rename(Var=edulevel))%>%filter(Var!="NA"))[["CI"]]),

c((svyby(~sp1632_1, by = ~group, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(sp1632_1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
     mutate(CI=paste(round(sp1632_1,1)," (",round(sp1632_1-1.96*se,1),",",round(sp1632_1+1.96*se,1), ")", sep = "")))$CI,
  "",
  (rbind(svyby(~sp1632_1, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(sp1632_1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
           mutate(CI=paste(round(sp1632_1,1)," (",round(sp1632_1-1.96*se,1),", ",round(sp1632_1+1.96*se,1), ")", sep = ""))%>%
           rename(Var=agegroup),
         "",
         svyby(~sp1632_1, by = ~sex, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(sp1632_1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
           mutate(CI=paste(round(sp1632_1,1)," (",round(sp1632_1-1.96*se,1),", ",round(sp1632_1+1.96*se,1), ")", sep = ""))%>%
           rename(Var=sex),
         "",
         svyby(~sp1632_1, by = ~edulevel, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(sp1632_1)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
           mutate(CI=paste(round(sp1632_1,1)," (",round(sp1632_1-1.96*se,1),", ",round(sp1632_1+1.96*se,1), ")", sep = ""))%>%
           rename(Var=edulevel))%>%filter(Var!="NA"))[["CI"]]))%>%data.frame()%>%
  kable(digits =1,col.names = c('','Wave 8','Wave 9'))%>%kable_classic()%>%
  add_header_above(c(" " = 1, "Prevalence (%) (95% CI)"=2))

####################################################################################################
#Proportion of posts involving negative content on social media during the 2019 social unrest
sp_merged9<-sp_merged9%>%left_join(.,sp9_raw%>%select(member_id,sp16222),by="member_id")
sp_merged9<-sp_merged9%>%mutate(sp16222=ifelse(sp16222>100,NA,sp16222))


cbind(c("Overall","Age group (years)","18-39","40-59","≥60"),
      
c((svyby(~sp16222, by = ~group, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(sp16222)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
    mutate(CI=paste(round(sp16222,1)," (",round(sp16222-1.96*se,1),",",round(sp16222+1.96*se,1), ")", sep = "")))$CI,
 "",
 (svyby(~sp16222, by = ~agegroup, design = svydesign(ids = ~1, data = sp_merged9%>%filter(!is.na(sp16222)), weights = ~weights), FUN = svymean)%>%data.frame()%>%
          mutate(CI=paste(round(sp16222,1)," (",round(sp16222-1.96*se,1),", ",round(sp16222+1.96*se,1), ")", sep = ""))%>%
          rename(Var=agegroup))[["CI"]]))%>%data.frame()%>%
  kable(digits =1,col.names = c('','Prevalence (%) (95% CI)'))%>%kable_classic()



