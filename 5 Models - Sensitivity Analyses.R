library(tidyverse)
library(glmmTMB)
library(sjPlot)
library(jtools)

# setwd("C:/WD")
# 
# load(file="Data/Output/sparcspol_zip_yearmonth")


sparcspol_zip_yearmonth_m <- sparcspol_zip_yearmonth %>% filter(is.na(acs_age_10_24) == FALSE)
sparcspol_zip_yearmonth_m$npsych_ip[is.na(sparcspol_zip_yearmonth_m$npsych_ip)] <- 0
sparcspol_zip_yearmonth_m$psychdays_ip[is.na(sparcspol_zip_yearmonth_m$npsych_ip)] <- 0


sparcspol_zip_yearmonth_m <- sparcspol_zip_yearmonth_m %>% 
  group_by(year) %>%
  mutate(npol_all_rate_z = (npol_all_rate - mean(npol_all_rate,na.rm=TRUE)) / sd(npol_all_rate,na.rm=TRUE),
         propol_rate = (nstop + nsummon)/acs_total_pop,
         propol_rate_z = (propol_rate - mean(propol_rate,na.rm=TRUE)) / sd(propol_rate,na.rm=TRUE)) %>% ungroup()


sparcspol_zip_yearmonth_m <- within(sparcspol_zip_yearmonth_m, acs_pct_black_q3 <- as.integer(cut(acs_pct_black, quantile(acs_pct_black, probs=0:3/3, na.rm = TRUE), include.lowest=TRUE)))
sparcspol_zip_yearmonth_m$acs_pct_black_q3 <- as.factor(sparcspol_zip_yearmonth_m$acs_pct_black_q3)

########### Sensitivity Analysis 1: Primary psych diagnosis only 

s1_npol_all_rate_z_days <- glmmTMB(psychdays_ip ~ (1 + yearmonth |zip) + 
                                       npol_all_rate_z +
                                       acs_pct_poverty  +
                                       acs_edu_pct_lthighschool +
                                       acs_pct_unemployed +
                                       dist_to_hospital +
                                       offset(log(acs_age_10_24)),  
                                     family=nbinom2(link = "log"),
                                     control=glmmTMBControl(parallel=8),
                                     data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

# save(s1_npol_all_rate_z_days, file="Data/Output/s1_npol_all_rate_z_days")

s1_xiblack_q3_days <- glmmTMB(psych1days_ip ~ (1 + yearmonth |zip) + 
                                     npol_all_rate_z * acs_pct_black_q3 +
                                     acs_pct_poverty  +
                                     acs_edu_pct_lthighschool +
                                     acs_pct_unemployed +
                                     dist_to_hospital +
                                     offset(log(acs_age_10_24)),  
                                   family=nbinom2(link = "log"),
                                   control=glmmTMBControl(parallel=8),
                                   data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

# save(s1_xiblack_q3_days, file="Data/Output/s1_xiblack_q3_days")


tab_model(s1_npol_all_rate_z_days,s1_xiblack_q3_days,
          terms = c("npol_all_rate_z",
                     "acs_pct_black_q32",
                     "acs_pct_black_q33",
                     "npol_all_rate_z:acs_pct_black_q32",
                     "npol_all_rate_z:acs_pct_black_q33"),
          pred.labels = c("Z-Score of Policing Rate",
                          "Black Residents (2nd Quartile)",
                          "Black Residents (3rd Quartile)",
                          "Policing Rate X Black Residents (2nd Quartile)",
                          "Policing Rate X Black Residents (3rd Quartile)"),
          dv.labels = c("Model 1","Model 2"),
          title = "Inpatient Person-Days and Policing Rates",
          show.intercept = FALSE, 
          show.icc = TRUE, 
          show.aic = FALSE, 
          show.r2 = FALSE, 
          show.re.var = FALSE,
          show.obs = FALSE,
          show.ngroups = FALSE,
          p.style = "stars",
          file="C:/Users/subli/Dropbox/Projects/Prins/Tables and Figures/SPARCS and Policing/Sensitivity1.html")


########### Sensitivity Analysis 2: Deprivation and Social Fragmentation Indices
s2_npol_all_rate_z_days <- glmmTMB(psychdays_ip ~ (1 + yearmonth |zip) + 
                                     npol_all_rate_z +
                                     acs_pct_poverty  +
                                     acs_edu_pct_lthighschool +
                                     acs_pct_unemployed +
                                     social_frag +
                                     dist_to_hospital +
                                     offset(log(acs_age_10_24)),  
                                   family=nbinom2(link = "log"),
                                   control=glmmTMBControl(parallel=8),
                                   data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

# save(s2_npol_all_rate_z_days, file="Data/Output/s2_npol_all_rate_z_days")

s2_xiblack_q3_days <- glmmTMB(psych1days_ip ~ (1 + yearmonth |zip) + 
                                npol_all_rate_z * acs_pct_black_q3 +
                                acs_pct_poverty  +
                                acs_edu_pct_lthighschool +
                                acs_pct_unemployed +
                                social_frag +
                                dist_to_hospital +
                                offset(log(acs_age_10_24)),  
                              family=nbinom2(link = "log"),
                              control=glmmTMBControl(parallel=8),
                              data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

# save(s2_xiblack_q3_days, file="Data/Output/s2_xiblack_q3_days")


tab_model(s2_npol_all_rate_z_days,s2_xiblack_q3_days,
          terms = c("npol_all_rate_z",
                    "acs_pct_black_q32",
                    "acs_pct_black_q33",
                    "npol_all_rate_z:acs_pct_black_q32",
                    "npol_all_rate_z:acs_pct_black_q33"),
          pred.labels = c("Z-Score of Policing Rate",
                          "Black Residents (2nd Quartile)",
                          "Black Residents (3rd Quartile)",
                          "Policing Rate X Black Residents (2nd Quartile)",
                          "Policing Rate X Black Residents (3rd Quartile)"),
          dv.labels = c("Model 1","Model 2"),
          title = "Inpatient Person-Days and Policing Rates",
          show.intercept = FALSE, 
          show.icc = TRUE, 
          show.aic = FALSE, 
          show.r2 = FALSE, 
          show.re.var = FALSE,
          show.obs = FALSE,
          show.ngroups = FALSE,
          p.style = "stars",
          file="C:/Users/subli/Dropbox/Projects/Prins/Tables and Figures/SPARCS and Policing/Sensitivity2.html")

########## Sensitivity Analysis 3: Proactive Policing

s3_npol_all_rate_z_days <- glmmTMB(psychdays_ip ~ (1 + yearmonth |zip) + 
                                     propol_rate_z + 
                                     acs_pct_poverty  +
                                     acs_edu_pct_lthighschool +
                                     acs_pct_unemployed +
                                     dist_to_hospital +
                                     offset(log(acs_age_10_24)),  
                                   family=nbinom2(link = "log"),
                                   control=glmmTMBControl(parallel=8),
                                   data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

save(s3_npol_all_rate_z_days, file="Data/Output/s3_npol_all_rate_z_days")

s3_xiblack_q3_days <- glmmTMB(psych1days_ip ~ (1 + yearmonth |zip) + 
                                propol_rate_z * acs_pct_black_q3 +
                                acs_pct_poverty  +
                                acs_edu_pct_lthighschool +
                                acs_pct_unemployed +
                                dist_to_hospital +
                                offset(log(acs_age_10_24)),  
                              family=nbinom2(link = "log"),
                              control=glmmTMBControl(parallel=8),
                              data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

save(s3_xiblack_q3_days, file="Data/Output/s3_xiblack_q3_days")

tab_model(s3_npol_all_rate_z_days,s3_xiblack_q3_days,
          terms = c("propol_rate_z",
                    "acs_pct_black_q32",
                    "acs_pct_black_q33",
                    "propol_rate_z:acs_pct_black_q32",
                    "propol_rate_z:acs_pct_black_q33"),
          pred.labels = c("Z-Score of Policing Rate",
                          "Black Residents (2nd Quartile)",
                          "Black Residents (3rd Quartile)",
                          "Policing Rate X Black Residents (2nd Quartile)",
                          "Policing Rate X Black Residents (3rd Quartile)"),
          dv.labels = c("Model 1","Model 2"),
          title = "Inpatient Person-Days and Policing Rates",
          show.intercept = FALSE, 
          show.icc = TRUE, 
          show.aic = FALSE, 
          show.r2 = FALSE, 
          show.re.var = FALSE,
          show.obs = FALSE,
          show.ngroups = FALSE,
          p.style = "stars" ,
          file="Tables and Figures/SPARCS and Policing/Sensitivity3.html")

########## Sensitivity Analysis 4: Counts of Hospitalizations
s4_npol_all_rate_z <- glmmTMB(npsych_ip ~ (1 + yearmonth |zip) + 
                                     npol_all_rate_z +
                                     acs_pct_poverty  +
                                     acs_edu_pct_lthighschool +
                                     acs_pct_unemployed +
                                     dist_to_hospital +
                                     offset(log(acs_age_10_24)),  
                                   family=nbinom2(link = "log"),
                                   control=glmmTMBControl(parallel=8),
                                   data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

save(s4_npol_all_rate_z, file="Data/Output/s4_npol_all_rate_z_days")

s4_xiblack_q3 <- glmmTMB(npsych_ip ~ (1 + yearmonth |zip) + 
                                npol_all_rate_z * acs_pct_black_q3 +
                                acs_pct_poverty  +
                                acs_edu_pct_lthighschool +
                                acs_pct_unemployed +
                                dist_to_hospital +
                                offset(log(acs_age_10_24)),  
                              family=nbinom2(link = "log"),
                              control=glmmTMBControl(parallel=8),
                              data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

save(s4_xiblack_q3, file="Data/Output/s4_xiblack_q3_days")

tab_model(s4_npol_all_rate_z,s4_xiblack_q3,
          terms = c("npol_all_rate_z",
                    "acs_pct_black_q32",
                    "acs_pct_black_q33",
                    "npol_all_rate_z:acs_pct_black_q32",
                    "npol_all_rate_z:acs_pct_black_q33"),
          pred.labels = c("Z-Score of Policing Rate",
                          "Black Residents (2nd Quartile)",
                          "Black Residents (3rd Quartile)",
                          "Policing Rate X Black Residents (2nd Quartile)",
                          "Policing Rate X Black Residents (3rd Quartile)"),
          dv.labels = c("Model 1","Model 2"),
          title = "Inpatient Person-Days and Policing Rates",
          show.intercept = FALSE, 
          show.icc = TRUE, 
          show.aic = FALSE, 
          show.r2 = FALSE, 
          show.re.var = FALSE,
          show.obs = FALSE,
          show.ngroups = FALSE,
          p.style = "stars",
          file="Tables and Figures/SPARCS and Policing/Sensitivity4.html")

########## Sensitivity Analysis 5: Remove outliers hospitalizations > 30 days

s5_npol_all_rate_z <- glmmTMB(psychdays_ip_60days ~ (1 + yearmonth |zip) + 
                                npol_all_rate_z +
                                acs_pct_poverty  +
                                acs_edu_pct_lthighschool +
                                acs_pct_unemployed +
                                dist_to_hospital +
                                offset(log(acs_age_10_24)),  
                              family=nbinom2(link = "log"),
                              control=glmmTMBControl(parallel=8),
                              data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

save(s5_npol_all_rate_z, file="Data/Output/s5_npol_all_rate_z_days")

s5_xiblack_q3 <- glmmTMB(psychdays_ip_60days ~ (1 + yearmonth |zip) + 
                           npol_all_rate_z * acs_pct_black_q3 +
                           acs_pct_poverty  +
                           acs_edu_pct_lthighschool +
                           acs_pct_unemployed +
                           dist_to_hospital +
                           offset(log(acs_age_10_24)),  
                         family=nbinom2(link = "log"),
                         control=glmmTMBControl(parallel=8),
                         data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

save(s5_xiblack_q3, file="Data/Output/s5_xiblack_q3_days")

tab_model(s5_npol_all_rate_z,s5_xiblack_q3,
          terms = c("npol_all_rate_z",
                    "acs_pct_black_q32",
                    "acs_pct_black_q33",
                    "npol_all_rate_z:acs_pct_black_q32",
                    "npol_all_rate_z:acs_pct_black_q33"),
          pred.labels = c("Z-Score of Policing Rate",
                          "Black Residents (2nd Quartile)",
                          "Black Residents (3rd Quartile)",
                          "Policing Rate X Black Residents (2nd Quartile)",
                          "Policing Rate X Black Residents (3rd Quartile)"),
          dv.labels = c("Model 1","Model 2"),
          title = "Inpatient Person-Days and Policing Rates",
          show.intercept = FALSE, 
          show.icc = TRUE, 
          show.aic = FALSE, 
          show.r2 = FALSE, 
          show.re.var = FALSE,
          show.obs = FALSE,
          show.ngroups = FALSE,
          p.style = "stars",
          file="Tables and Figures/SPARCS and Policing/Sensitivity5.html")
