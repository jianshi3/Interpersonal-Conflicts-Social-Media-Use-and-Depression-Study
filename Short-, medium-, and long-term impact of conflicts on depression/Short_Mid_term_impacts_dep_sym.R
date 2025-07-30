# Load packages
library(readstata13)
library(readxl)
library(survey)
library(mice)
library(dplyr)
rm(list = ls(all.names = TRUE))

setwd("～/RC/data/short_mid_term/")

# Load data
data <- read.csv("msm 1106.csv")
colnames(data)
socio <- read.csv("demographic vars.csv")
colnames(socio)
socio2 <- subset(socio, Wave=="Wave 2")
colnames(socio2) <-  paste0(colnames(socio2),"w2")

data <- merge(data, socio2, by.x = "member_id",by.y="member_idw2",all.x = TRUE)

## merge in conf_st data
sp1to7 <- read.dta13('sp1-7 combined batch-2.dta',convert.factors = F)
sp1to7 <- sp1to7[,c("member_id","q113a","Wave")]
colnames(sp1to7)[2] <- "polconfst"
sp1 <- sp1to7 %>% filter(Wave=='oc') %>% rename("polconfst3"="polconfst")
sp2 <- sp1to7 %>% filter(Wave=='oc2')%>% rename("polconfst4"="polconfst")
sp4 <- sp1to7 %>% filter(Wave=='oc4')%>% rename("polconfst5"="polconfst")
sp5 <- sp1to7 %>% filter(Wave=='oc5')%>% rename("polconfst6"="polconfst")
sp7 <- sp1to7 %>% filter(Wave=='oc7')%>% rename("polconfst7"="polconfst")

sp8 <- read.dta13("sp8.dta",convert.factors = F)
sp9 <- read.dta13("sp9.dta",convert.factors = F)
# Extract relevant data
sp8 <- sp8 %>% select(member_id,age,sex,edulevel,marital,per_inc,emply2,start_y,start_m,start_d,q401:q409,phq,q81:q85,fapgar,sp91,sp92,q13,q11,q12,sp192_1:q113a_2) %>%
  mutate(q112a=ifelse(q112a_1==0,0,q112a_2),q113a=ifelse(q113a_1==0,0,q113a_2),policymax=ifelse(sp192_1==0,0,sp192_2))
sp9 <- sp9 %>% select(member_id,age,sex,edulevel,marital,per_inc,emply2,htype_2,start_y,start_m,start_d,q401:q409,q81:q85,fapgar,sp91,sp91a,sp92,sp92a,q13_hr,q13_min,q11_hr,q11_min,q12_1_hr,q12_1_min,q12_2_hr,q12_2_min,sp192_1a:q113a_2a) %>%
  mutate(phq=q401+q402+q403+q404+q405+q406+q407+q408+q409, sp91 = ifelse(is.na(sp91),sp91a,sp91), sp92 = ifelse(is.na(sp92),sp92a,sp92),
         q112a=ifelse(q112a_1a==0,0,q112a_2a),
         q113a=ifelse(q113a_1a==0,0,q113a_2a),
         policymax=ifelse(sp192_1a==0,0,sp192_2a)
  )

# Data cleaning
sp8 <- sp8 %>% mutate(date = paste0(start_y,'-',start_m,'-',start_d)) %>% select(-start_y,-start_m,-start_d)
sp9 <- sp9 %>% mutate(date = paste0(start_y,'-',start_m,'-',start_d))

sp9 <- sp9 %>% mutate(timespent_socmed = q13_hr*60+q13_min,
                      q11_hr = ifelse(q11_hr==999,NA,q11_hr),q11_min = ifelse(q11_min==999,NA,q11_min),timespent_tv = 60*q11_hr+q11_min,
                      q12_1_hr = ifelse(q12_1_hr==999,NA,q12_1_hr),q12_1_min = ifelse(q12_1_min==999,NA,q12_1_min),q12_1 = 60*q12_1_hr+q12_1_min,
                      q12_2_hr = ifelse(q12_2_hr==999,NA,q12_2_hr),q12_2_min = ifelse(q12_2_min==999,NA,q12_2_min),q12_2 = 60*q12_2_hr+q12_2_min)
sp9 <- sp9 %>% mutate(timespent_newspaper_or_radio = q12_1+q12_2)
sp9 <- sp9 %>% select(-q13_hr,-q13_min,-q11_hr,-q11_min,-q12_1_hr,-q12_1_min,-q12_2_hr,-q12_2_min,-q12_1,-q12_2,-start_y,-start_m,-start_d)

# Rename vars for merging
sp8 <- sp8 %>% rename('conf_family' = 'sp192_1','conf_family_member_a2' = 'sp192_2','q112' = 'q112a_1','q113' = 'q113a_1','timespent_socmed' = 'q13','timespent_tv' = 'q11','timespent_newspaper_or_radio' = 'q12')
sp9 <- sp9 %>% rename('conf_family' = 'sp192_1a','conf_family_member_a2' = 'sp192_2a','q112' = 'q112a_1a','q113' = 'q113a_1a')

# Convert dates
sp8$date <- lubridate::ymd(sp8$date)
sp9$date <- lubridate::ymd(sp9$date)

sp8 <- sp8[,c("member_id","q113a")] %>% rename("polconfst8"="q113a")
sp9 <- sp9[,c("member_id","q113a")] %>% rename("polconfst9"="q113a")

# merge
data <- merge(data, sp1[,1:2], by="member_id",all.x = TRUE)
data <- merge(data, sp2[,1:2], by="member_id",all.x = TRUE)
data <- merge(data, sp4[,1:2], by="member_id",all.x = TRUE)
data <- merge(data, sp5[,1:2], by="member_id",all.x = TRUE)
data <- merge(data, sp7[,1:2], by="member_id",all.x = TRUE)
data <- merge(data, sp8[,1:2], by="member_id",all.x = TRUE)
data <- merge(data, sp9[,1:2], by="member_id",all.x = TRUE)

#### read CS6 outcomes Apr16-2025
cs6 <- read_xlsx("cs6 with weights.xlsx")
cs6clean <- read.dta13("~/RC/data/cs1_19_final2p1.dta")
cs6clean <- subset(cs6clean, cs6clean$wave==15)

summary(cs6clean$phqtotal)
summary(cs6clean$prophq)
table(cs6clean$phqtotal,cs6clean$prophq)
cs6clean <- cs6clean[,c("member_id","phqtotal","prophq")]
names(cs6clean) <- c("member_id","phq14","phq_10yesno14")
data <- merge(data , cs6clean, by="member_id", all.x = TRUE)  

## define target sample for each association
IDlist <- read_xlsx("wave 1 to cs19.xlsx")
w3w3 <- IDlist$member_id[IDlist$Wave=="oc"]
w3w4 <- unique(IDlist$member_id[IDlist$Wave=="oc"|IDlist$Wave=="oc2"])
w4w4 <- IDlist$member_id[IDlist$Wave=="oc2"]
w3w6 <- unique(IDlist$member_id[IDlist$Wave=="oc"|IDlist$Wave=="oc5"])
w4w6 <- unique(IDlist$member_id[IDlist$Wave=="oc2"|IDlist$Wave=="oc5"])
w8w8 <- IDlist$member_id[IDlist$Wave=="oc8"]
w9w9 <- IDlist$member_id[IDlist$Wave=="oc9"]
w8w9 <- unique(IDlist$member_id[IDlist$Wave=="oc8"|IDlist$Wave=="oc9"])
w8w14 <- unique(IDlist$member_id[IDlist$Wave=="oc8"|IDlist$Wave==" cs6"])
w9w14 <- unique(IDlist$member_id[IDlist$Wave=="oc9"|IDlist$Wave=="cs6"])


#### Short-term and mid-term impact negative binomial regression with MI ####
##  adjusted for wave 2 sociodemographics, prior family apgar, prior  depressive symptoms, prior conflicts
imp_mod_nb <- function(outcome,priordep,priorapgar,priorvar,variable,IDs,baseline) {
  varlist <- paste0(c('sex','age','emply2','marital','edulevel','hincome'),baseline)
  fit_models <- with(data = for_imp_mi, expr = {
    MASS::glm.nb(as.formula(paste(outcome," ~ ",varlist[1]," +",varlist[2]," +",varlist[3]," +",varlist[4]," +",varlist[5]," +",varlist[6]," +",priorapgar, " +", priordep,"+", priorvar,"+",variable)))
  })
  
  pooled_results <- pool(fit_models)
  out <- summary(pooled_results) %>% data.frame()
  out <- out %>% dplyr::filter(term == variable)
  OR = exp(out$estimate)
  LL = exp(out$estimate-qnorm(.975)*out$std.error)
  UL = exp(out$estimate+qnorm(.975)*out$std.error)
  data.frame(OR = OR, LL= LL, UL = UL,var = variable, outcome = outcome)
}


# Other conflicts without prior
imp_mod_oth_nb <- function(outcome,priordep,priorapgar,variable,IDs,baseline) {
  varlist <- paste0(c('sex','age','emply2','marital','edulevel','hincome'),baseline)
  fit_models <- with(data = for_imp_mi, expr = {
    MASS::glm.nb(as.formula(paste(outcome," ~ ",varlist[1]," +",varlist[2]," +",varlist[3]," +",varlist[4]," +",varlist[5]," +",varlist[6]," +",priorapgar, " +", priordep,"+",variable)))
  })
  
  pooled_results <- pool(fit_models)
  out <- summary(pooled_results) %>% data.frame()
  out <- out %>% dplyr::filter(term == variable)
  OR = exp(out$estimate)
  LL = exp(out$estimate-qnorm(.975)*out$std.error)
  UL = exp(out$estimate+qnorm(.975)*out$std.error)
  data.frame(OR = OR, LL= LL, UL = UL,var = variable, outcome = outcome)
}

# Merge results IRR
results_tbl_irr <- rbind(
  # imp_mod_nb('phq3','phq2','fapgar2',"polconf2",'polconf3',w3w3,'w2'),
  # imp_mod_nb('phq4','phq2','fapgar2',"polconf2",'polconf3',w3w4,'w2'),
  # imp_mod_nb('phq6','phq2','fapgar2',"polconf2",'polconf3',w3w6,'w2'),
  # imp_mod_nb('phq4','phq3','fapgar3',"polconf3",'polconf4',w4w4,'w2'),
  # imp_mod_nb('phq6','phq3','fapgar3',"polconf3",'polconf4',w4w6,'w2'),
  # imp_mod_nb('phq8','phq7','fapgar7',"polconf7",'polconf8',w8w8,'w2'),
  # imp_mod_nb('phq9','phq8','fapgar8',"polconf8",'polconf9',w9w9,'w2'),
  # imp_mod_nb('phq9','phq7','fapgar7',"polconf7",'polconf8',w8w9,'w2'),
  imp_mod_nb('phq14','phq7','fapgar7',"polconf7",'polconf8',w8w14,'w2'),
  imp_mod_nb('phq14','phq8','fapgar8',"polconf8",'polconf9',w9w14,'w2')
)

results_tbl_irr_fc <- rbind(
  # imp_mod_oth_nb('phq3','phq2','fapgar2','polconffc3',w3w3,'w2'),
  # imp_mod_oth_nb('phq4','phq2','fapgar2','polconffc3',w3w4,'w2'),
  # imp_mod_oth_nb('phq6','phq2','fapgar2','polconffc3',w3w6,'w2'),
  # imp_mod_nb('phq4','phq3','fapgar3',"polconffc3",'polconffc4',w4w4,'w2'),
  # imp_mod_nb('phq6','phq3','fapgar3',"polconffc3",'polconffc4',w4w6,'w2'),
  # imp_mod_nb('phq8','phq7','fapgar7',"polconffc7",'polconffc8',w8w8,'w2'),
  # imp_mod_nb('phq9','phq8','fapgar8',"polconffc8",'polconffc9',w9w9,'w2'),
  # imp_mod_nb('phq9','phq7','fapgar7',"polconffc7",'polconffc8',w8w9,'w2'),
  imp_mod_nb('phq14','phq7','fapgar7',"polconffc7",'polconffc8',w8w14,'w2'),
  imp_mod_nb('phq14','phq8','fapgar8',"polconffc8",'polconffc9',w9w14,'w2')
)

results_tbl_irr_st <- rbind(
  # imp_mod_oth_nb('phq3','phq2','fapgar2','polconfst3',w3w3,'w2'),
  # imp_mod_oth_nb('phq4','phq2','fapgar2','polconfst3',w3w4,'w2'),
  # imp_mod_oth_nb('phq6','phq2','fapgar2','polconfst3',w3w6,'w2'),
  # imp_mod_nb('phq4','phq3','fapgar3',"polconfst3",'polconfst4',w4w4,'w2'),
  # imp_mod_nb('phq6','phq3','fapgar3',"polconfst3",'polconfst4',w4w6,'w2'),
  # imp_mod_nb('phq8','phq7','fapgar7',"polconfst7",'polconfst8',w8w8,'w2'),
  # imp_mod_nb('phq9','phq8','fapgar8',"polconfst8",'polconfst9',w9w9,'w2'),
  # imp_mod_nb('phq9','phq7','fapgar7',"polconfst7",'polconfst8',w8w9,'w2'),
  imp_mod_nb('phq14','phq7','fapgar7',"polconfst7",'polconfst8',w8w14,'w2'),
  imp_mod_nb('phq14','phq8','fapgar8',"polconfst8",'polconfst9',w9w14,'w2')
)


results_tbl_irr %>%
  mutate(adj_or = paste0(sprintf("%.2f",(round(OR,2)))," (",sprintf("%.2f",round(LL,2)),", ",sprintf("%.2f",round(UL,2)),")")) #%>%
  # write.csv(.,'Adjusted IRR_fam_mi.csv',row.names = F)

results_tbl_irr_fc %>%
  mutate(adj_or = paste0(sprintf("%.2f",(round(OR,2)))," (",sprintf("%.2f",round(LL,2)),", ",sprintf("%.2f",round(UL,2)),")")) #%>%
  # write.csv(.,'Adjusted IRR_fc_mi.csv',row.names = F)

results_tbl_irr_st %>%
  mutate(adj_or = paste0(sprintf("%.2f",(round(OR,2)))," (",sprintf("%.2f",round(LL,2)),", ",sprintf("%.2f",round(UL,2)),")")) #%>%
  # write.csv(.,'Adjusted IRR_st_mi.csv',row.names = F)





