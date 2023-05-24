library(sjPlot)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(gt)
library(gtsummary)
library(MetBrewer)

setwd("C:/WD")

load(file="Data/Output/2022-07-20 m_npol_all_rate_z_days")
load(file="Data/Output/2022-07-20 m_xiblack_q3_days")
load(file="Data/Output/2022-07-20 m_xipoc_q3_days")

load(file="Data/Output/2022-07-20 m_npol_all_rate_z")
load(file="Data/Output/2022-07-20 m_xiblack_q3")
load(file="Data/Output/2022-07-20 m_xipoc_q3")


##################### TABLES ##########################################


#### Table 1: Neighborhood Characteristics by ACS 5-Year Estimates

table1 <- sparcspol_zip_yearmonth %>% select(zip,
                                   year_range,
                                   acs_total_pop,
                                   acs_pct_10_17,
                                   acs_pct_18_24,
                                   acs_pct_poverty,
                                   acs_median_income,
                                   acs_edu_pct_lthighschool,
                                   acs_pct_unemployed,
                                   #acs_pct_poc,
                                   acs_pct_black,
                                   dist_to_hospital,
                                   npsych_rate_ip,
                                   psychdays_ip,
                                   npol_all_rate,
) %>%
  mutate(acs_pct_10_17 = round(acs_pct_10_17 * 100,1),
         acs_pct_18_24 = round(acs_pct_18_24 * 100,1)
  ) %>%
  group_by(zip, year_range) %>%
  summarize_all(mean,na.rm=TRUE) %>%
  ungroup() %>%
  select(-zip) %>%
  
  tbl_summary(by = year_range,
              digits = all_continuous() ~ 1,
              label = list(acs_total_pop ~ "Total Population",
                           acs_pct_10_17 ~ "% Age 10-17",
                           acs_pct_18_24 ~ "% Age 18-24",
                           acs_pct_poverty ~ "% Household Poverty",
                           acs_edu_pct_lthighschool ~ "% Education < High School ",
                           acs_pct_unemployed ~ "% Adults Unemployed",
                           #acs_pct_poc ~ "% Residents of Color",
                           acs_pct_black ~ "% Black Residents",
                           dist_to_hospital ~ "Distance to Hospital (Miles)",
                           npsych_rate_ip ~ "Rate of Psychiatric Hospitalizations",
                           psychdays_ip ~ "Rate of Psychiatric Inpatient Days",
                           npol_all_rate ~ "Rate of Policing Incidents"),
              
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               #acs_total_pop ~ "{mean} ({sd})",
                               #acs_median_income ~ "{mean} ({sd})",
                               dist_to_hospital ~ "{mean} ({sd})")
              
  ) %>% 
  modify_header(stat_1="2007-2011", stat_2 = "2009-2013", stat_3 = "2012-2016") %>% 
  modify_spanning_header(all_stat_cols() ~ "ACS 5-year estimate period")

table1 %>% as_gt() %>% gtsave(file = "C:/Users/subli/Dropbox/Projects/Journal Submissions/2023-05 SPARCSPOL/SPPE/Table1.rtf")


#### Table 2: Neighborhood Characteristics by % Black Residents

table2 <- sparcspol_zip_yearmonth %>% select(zip,
                                   acs_pct_10_17,
                                   acs_pct_18_24,
                                   acs_pct_poverty,
                                   acs_edu_pct_lthighschool,
                                   acs_pct_unemployed,
                                   #acs_pct_poc,
                                   acs_pct_black,
                                   dist_to_hospital,
                                   acs_pct_black_q3,
                                   npsych_rate_ip,
                                   psychdays_ip,
                                   npol_all_rate,
                                   #npol_rate
) %>%
  mutate(acs_pct_10_17 = round(acs_pct_10_17 * 100,1),
         acs_pct_18_24 = round(acs_pct_18_24 * 100,1)
  ) %>%
  group_by(zip, acs_pct_black_q3) %>%
  summarize_all(mean,na.rm=TRUE) %>%
  ungroup() %>%
  select(-zip) %>%
  
  tbl_summary(by = acs_pct_black_q3,
              digits = all_continuous() ~ 1,
              label = list(acs_pct_10_17 ~ "% Age 10-17",
                           acs_pct_18_24 ~ "% Age 18-24",
                           acs_pct_poverty ~ "% Household Poverty",
                           acs_edu_pct_lthighschool ~ "% Education < High School ",
                           acs_pct_unemployed ~ "% Adults Unemployed",
                           #acs_pct_poc ~ "% Residents of Color",
                           acs_pct_black ~ "% Black Residents",
                           dist_to_hospital ~ "Distance to Hospital (Miles)",
                           npsych_rate_ip ~ "Rate of Psychiatric Hospitalizations",
                           psychdays_ip ~ "Rate of Psychiatric Inpatient Days",
                           npol_all_rate ~ "Rate of Policing Incidents"),
              
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               #acs_total_pop ~ "{mean} ({sd})",
                               #acs_median_income ~ "{mean} ({sd})",
                               dist_to_hospital ~ "{mean} ({sd})")
              
  ) %>% 
  modify_header(stat_1="0-4%", stat_2 = "4-25%", stat_3 = "25-93%") %>% 
  modify_spanning_header(all_stat_cols() ~ "ZCTA % Black Residents")


table2 %>% as_gt() %>% gtsave(file = "C:/Users/subli/Dropbox/Projects/Journal Submissions/2023-05 SPARCSPOL/SPPE/Table2.rtf")




############### Table 3: Main Analysis
tab_model(m_npol_all_rate_z_days, m_xiblack_q3_days,m_xipoc_q3_days,
              terms = c("npol_all_rate_z",
                        "acs_pct_poc_q32",
                        "acs_pct_poc_q33",
                        
                        "acs_pct_black_q32",
                        "acs_pct_black_q33",
                        
                        "npol_all_rate_z:acs_pct_poc_q32",
                        "npol_all_rate_z:acs_pct_poc_q33",
                        
                        "npol_all_rate_z:acs_pct_black_q32",
                        "npol_all_rate_z:acs_pct_black_q33"),
              #order.terms = c(1,2,3,6,7,4,5,8,9),
              pred.labels = c("Z-Score of Policing Rate",
                              "Residents of Color (2nd Quartile)",
                              "Residents of Color (3rd Quartile)",
                              
                              
                              "Residents of Color X Policing Rate (2nd Quartile)",
                              "Residents of Color X Policing Rate (3rd Quartile)",
                              
                              
                              "Black Residents (2nd Quartile)",
                              "Black Residents (3rd Quartile)",
                              
                              "Black Residents X Policing Rate (2nd Quartile)",
                              "Black Residents X Policing Rate (3rd Quartile)"),
              dv.labels = c("Model 1","Model 2", "Model 3"),
              title = "Inpatient Person-Days and Policing Rates",
              
              show.intercept = FALSE, 
              show.icc = TRUE, 
              show.aic = FALSE, 
              show.r2 = FALSE, 
              show.re.var = FALSE,
              show.obs = FALSE,
              show.ngroups = FALSE,
              p.style = "stars" )

tab_model(m_npol_all_rate_z, m_xipoc_q3, m_xiblack_q3,
          terms = c("npol_all_rate_z",
                    "acs_pct_poc_q32",
                    "acs_pct_poc_q33",
                    
                    "acs_pct_black_q32",
                    "acs_pct_black_q33",
                    
                    "npol_all_rate_z:acs_pct_poc_q32",
                    "npol_all_rate_z:acs_pct_poc_q33",
                    
                    "npol_all_rate_z:acs_pct_black_q32",
                    "npol_all_rate_z:acs_pct_black_q33"),
          #order.terms = c(1,2,3,6,7,4,5,8,9),
          pred.labels = c("Z-Score of Policing Rate",
                          "Residents of Color (2nd Quartile)",
                          "Residents of Color (3rd Quartile)",
                          
                          
                          "Policing Rate X Residents of Color (2nd Quartile)",
                          "Policing Rate X Residents of Color (3rd Quartile)",
                          
                          "Black Residents (2nd Quartile)",
                          "Black Residents (3rd Quartile)",
                          
                          
                          "Policing Rate X Black Residents (2nd Quartile)",
                          "Policing Rate X Black Residents (3rd Quartile)"),
          dv.labels = c("Model 1","Model 2", "Model 3"),
          title = "Inpatient Admission Counts and Policing Rates",
          
          show.intercept = FALSE, 
          show.icc = TRUE, 
          show.aic = FALSE, 
          show.r2 = FALSE, 
          show.re.var = FALSE,
          show.obs = FALSE,
          show.ngroups = FALSE,
          p.style = "stars" )


#### Figure 1: Maps of psychiatric inpatient days and policing incidents #####################################

library(tidyverse)
library(tmap)
library(sf)

options(tigris_use_cache = TRUE)

zip_geo <- tigris::zctas(cb = FALSE,year = 2010, state = "NY", starts_with = unique(sparcspol_zip$zip)) %>%
  mutate(zip = as.factor(ZCTA5CE10)) %>%
  left_join(sparcspol_zip) 
# %>%
#   mutate(npol_all_rate = ifelse(is.infinite(npol_all_rate),0,npol_all_rate),
#          psychdays_rate_ip  = ifelse(is.infinite(nhosp_rate),0,nhosp_rate),
#          npsych_rate_ip = ifelse(is.infinite(nhosp_rate),0,nhosp_rate)
#   ) %>% filter(npol_all_rate < 10000 )

m_zip_hosp <- tm_shape(zip_geo) + 
  tm_polygons("psychdays_rate_ip",
              style = "quantile", 
              n = 10,
              legend.hist = TRUE, 
              palette = "RdPu",
              title="Psychiatric Inpatient Days \n(Rate per 1,000)") +
  tm_layout(legend.format = list(digits = 0),legend.outside = TRUE, legend.hist.width = 1)

m_zip_pol <- tm_shape(zip_geo) + 
  tm_polygons("npol_all_rate",
              style = "quantile", 
              n = 10,
              legend.hist = TRUE, 
              palette = "Blues",
              title="Policing Incidents \n(Rate per 1,000)") +
  tm_layout(legend.outside = TRUE, legend.hist.width = 1, legend.format = list(digits = 0))

m_comb <- tmap_arrange(m_zip_hosp ,m_zip_pol,ncol=1)
m_comb

m_comb %>% tmap_save("C:/Users/subli/Dropbox/Projects/Journal Submissions/2023-05 SPARCSPOL/SPPE/fig1.tiff")


#### Figure 2
model2_plot<-plot_model(m_xiblack_q3_days, 
           type = "pred", 
           terms = c("npol_all_rate_z [-1:3]", "acs_pct_black_q3"),
           line.size = 1.5,
           show.p = TRUE
           ) + 
  theme_bw() +ggtitle("") +
  xlab("Policing Z-Score")+
  ylab("Psychiatric Inpatient Days per Month") + 
  #labs(color = "") + 
  scale_color_discrete(name = "ZCTA % Black Residents",labels = c("0-4%", "4-25%","25-94%"))+
  theme(legend.position = "bottom") +
  coord_cartesian(ylim=c(70, 165))

model2_plot

ggsave(filename = "C:/Users/subli/Dropbox/Projects/Journal Submissions/2023-05 SPARCSPOL/SPPE/Fig2.tiff",
  plot = last_plot(),
  width = 6,
  height = 5,
  units = "in",
  dpi = 800
)





# 
# 
# plot_model(m_xiblack_q3_days, 
#            type = "pred", 
#            terms = c("npol_all_rate_z [-2:2]", "acs_pct_black_q3"),
#            line.size = 1.5,
#            show.p = TRUE
# ) + 
#   theme_bw() +ggtitle("") +
#   xlab("Policing Z-Score")+
#   #ylab("") + 
#   labs(color = "") + 
#   scale_color_discrete(name = "ZCTA % Black Residents", labels = c("0-4%", "4-25%","25-93%"))+
#   theme(legend.position = "right") +
#   coord_cartesian(ylim=c(75, 175))
# 
# model2_plot
# 

