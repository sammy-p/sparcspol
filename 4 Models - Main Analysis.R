library(tidyverse)
library(sjPlot)
library(lme4)
library(glmmTMB)

setwd("C:/WD")


load(file="Data/Output/sparcspol_zip_yearmonth")


sparcspol_zip_yearmonth_m <- sparcspol_zip_yearmonth %>% filter(is.na(acs_age_10_24) == FALSE)

sparcspol_zip_yearmonth_m$npsych_ip[is.na(sparcspol_zip_yearmonth_m$npsych_ip)] <- 0
sparcspol_zip_yearmonth_m$psychdays_ip[is.na(sparcspol_zip_yearmonth_m$npsych_ip)] <- 0


sparcspol_zip_yearmonth_m <- sparcspol_zip_yearmonth_m %>% 
  group_by(year) %>%
  mutate(npol_all_rate_z = (npol_all_rate - mean(npol_all_rate,na.rm=TRUE)) / sd(npol_all_rate,na.rm=TRUE)

  ) %>% ungroup()





# m_npol_all_rate_z <- glmmTMB(npsych_ip ~ (1 + yearmonth |zip) + 
#                                npol_all_rate_z +
#                                acs_pct_poverty  +
#                                acs_edu_pct_lthighschool +
#                                acs_pct_unemployed +
#                                dist_to_hospital +
#                                offset(log(acs_age_10_24))
#                              ,  
#                              family=nbinom2(link = "log"),
#                              control=glmmTMBControl(parallel=min(parallel::detectCores(),5)),
#                              data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))
# 
# summary(m_npol_all_rate_z)
# tab_model(m_npol_all_rate_z)
# 
# save(m_npol_all_rate_z, file="Data/Output/m_npol_all_rate_z")

m_npol_all_rate_z_days <- glmmTMB(psychdays_ip ~ (1 + yearmonth |zip) + 
                               npol_all_rate_z +
                               acs_pct_poverty  +
                               acs_edu_pct_lthighschool +
                               acs_pct_unemployed +
                               dist_to_hospital +
                               offset(log(acs_age_10_24))
                             ,  
                             family=nbinom2(link = "log"),
                             control=glmmTMBControl(parallel=min(parallel::detectCores(),5)),
                             data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

summary(m_npol_all_rate_z_days)
tab_model(m_npol_all_rate_z_days)

save(m_npol_all_rate_z_days, file="Data/Output/m_npol_all_rate_z_days")
# 
# 
# m_xiblack_q3 <- glmmTMB(npsych_ip ~ (1 + yearmonth |zip) +
#                                npol_all_rate_z * acs_pct_black_q3  +
#                                acs_pct_poverty  +
#                                acs_edu_pct_lthighschool +
#                                acs_pct_unemployed +
#                                dist_to_hospital +
#                                offset(log(acs_age_10_24))
#                              ,
#                              family=nbinom2(link = "log"),
#                              control=glmmTMBControl(parallel=5),
#                              data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))
# 
# save(m_xiblack_q3, file="Data/Output/m_xiblack_q3")
# 
# summary(m_xiblack_q3)
# tab_model(m_xiblack_q3)

m_xiblack_q3_days <- glmmTMB(psychdays_ip ~ (1 + yearmonth|zip) +
                               npol_all_rate_z * acs_pct_black_q3  +
                               acs_pct_poverty  +
                               acs_edu_pct_lthighschool +
                               acs_pct_unemployed +
                               dist_to_hospital +
                               offset(log(acs_age_10_24))
                             ,
                             family=nbinom2(link = "log"),
                             control=glmmTMBControl(parallel=5),
                             data=subset(sparcspol_zip_yearmonth_m, year %in% 2006:2014))

save(m_xiblack_q3_days, file="Data/Output/m_xiblack_q3_days")

summary(m_xiblack_q3_days)
tab_model(m_xiblack_q3_days)



