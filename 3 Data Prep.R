# Set working directory
setwd("C:/WD")

# load("Data/Output/acs_zip_2007_2011")
# load("Data/Output/acs_zip_2009_2013")
# load("Data/Output/acs_zip_2012_2016")

ptm <- proc.time()

library(tidyverse)
library(lubridate)
library(zoo)
library(janitor)

# Function to recode non-residential ZCTAs to match the nearest ZIP
zip_recode <- function(x) {
  x <- x %>%
    mutate(zip = as.character(zip)) %>%
    mutate(zip = case_when(
      zip %in% 10119 ~ "10001",
      zip %in% c(10279,10278) ~ "10007",
      zip %in% 10115 ~ "10027",
      zip %in% 10110 ~ "10036",
        zip %in% c(10020, 10103, 10111, 10112) ~ "10019",
        zip %in% c(10152,10154,10153) ~ "10022",
        zip %in% c(10165, 10172, 10173,10168,10171,10174,10177) ~ "10017",
        zip %in% 11351 ~ "11357",
        zip %in% 11371 ~ "11369",
        zip %in% 11359 ~ "11360",
        zip %in% 11430 ~ "11436",
        zip %in% 11451 ~ "11433",
        zip %in% 11005 ~ "11004",
        zip %in% 11424 ~ "11415",
        TRUE ~ zip)
    ) %>%
    mutate(zip = as.factor(zip))
}

# Read in Policing Data
 pol <- read_csv("Data/Output/SQF Arrests Summons All Ages with ZIP.csv")
 format(object.size(pol),units = "auto") 
 
 pol <- pol %>%
   mutate(year = year(date)) %>%
   filter(year %in% 2006:2014) %>%
   mutate(yearmonth = as.Date(as.yearmon(date, "%m/%Y")),
          month = month(as.yearmon(date, "%m/%Y")),
          
          raceeth = case_when(race == "A" ~	"ASIAN / PACIFIC ISLANDER",
                              race == "B" ~	"BLACK",
                              race == "I" ~	"AMERICAN INDIAN / ALASKAN NATIVE",
                              race == "P" ~	"BLACK HISPANIC",
                              race == "Q" ~	"WHITE HISPANIC",
                              race == "W" ~	"WHITE",
                              race == "X" ~	"UNKNOWN",
                              race == "U" ~ "UNKNOWN",
                              race == "Z" ~	"OTHER",
                              TRUE ~ race),
          census = 1,
          zip = factor(as.character(zip)),
   ) %>%
   zip_recode() 

# Read in SPARCS Data


sparcs <- read_csv("Data/Raw/sjp_IP_00_14_zip no ethnicity exclusion.csv") %>% 
  rename_with(tolower) %>%
  filter(as.numeric(age) >= 10 &
         as.numeric(age) <= 24 &
         patco %in% c("58", "59", "60", "61", "62") &
         year(ymd(admdt)) %in% 2006:2014
        ) %>%
  mutate(
    visit_type = "ip",
    sex = factor(sex, levels = c('F', 'M', 'U')),
    admdt = ymd(admdt),
    disdt = ymd(disdt),
    days = as.numeric(disdt - admdt + 1),
    year = year(ymd(admdt)),
    month = month(ymd(admdt)),
    psych1 = ifelse(substr(dx01,1,3) %in% 290:316,1,0), # Visits where the primary diagnosis is psych/sud
    yearmonth = as.Date(as.yearmon(ymd(admdt), "%m/%Y")),
    age = as.numeric(age),
    agegroup = as.factor(ifelse(as.numeric(age) < 18,"<18","18-24")),
    bipoc = ifelse(race %in% "01", 0, 1),
    zip = factor(zip),
    census = 1,
  ) %>%
  
  zip_recode()

# Generate variables for first psychiatric diagnosis in record
sparcs$firstpsych_pos = apply(sparcs[ , grepl("dx", names(sparcs))], 1, function(x) {
  names(x)[min(which(as.numeric(substr(x,1,3)) %in% 290:316))]
})

sparcs$firstpsych = apply(sparcs[ , grepl("dx", names(sparcs))], 1, function(x) {
  substr(x,1,3)[min(which(as.numeric(substr(x,1,3)) %in% 290:316))]
})

# This code reads in ZCTA boundaries using the tigris package. The dataframe is then saved for future use
#   zip_area <- tigris::zctas(cb = FALSE,year = 2010, state = "NY", starts_with = unique(pol$zip)) %>%
#     mutate(zip = as.factor(ZCTA5CE10),
#     area = ALAND10) %>%
#     select(zip,area) %>%
#     as.data.frame()
# 
# save(zip_area, file="Data/Output/zip_area")

load(file="Data/Output/zip_area")
  
### Calculate average distance to nearest hospital for each ZIP code
  {  
    library(sf)
    facility_discharges <- read_csv("Data/Raw/All_Payer_Hospital_Inpatient_Discharges_by_Facility__SPARCS_De-Identified___Beginning_2009.csv") %>%
      clean_names(case="snake") %>%
      filter(discharge_year == 2009) %>%
      mutate(facility_id = as.factor(facility_id)) %>%
      select(c(hospital_name,facility_id)) %>%
      group_by(hospital_name,facility_id) %>%
      summarize() 
    
    facility_info <- read_csv("Data/Raw/Health_Facility_General_Information.csv") %>%
      clean_names(case="snake") %>% 
      select(facility_id,facility_name,facility_address_1,facility_address_2,facility_city,facility_state,facility_zip_code,facility_county_code,facility_county,facility_latitude,facility_longitude) %>%
      mutate(facility_id = as.factor(facility_id))
    
    facilities_sf = st_as_sf(left_join(facility_discharges,facility_info) %>% 
                               distinct() %>%
                               filter(is.na(facility_latitude) == FALSE), 
                             coords = c("facility_longitude", "facility_latitude"), 
                             crs = "NAD83", agr = "constant") 
    
    rm(facility_info,facility_discharges)
    
    zip_geo <- tigris::zctas(cb = FALSE,
                             year = 2010, 
                             state = "NY", 
                             starts_with = as.character(unique(sparcs$zip))
    )
    
    nearest_hospital_index <- st_nearest_feature(zip_geo,facilities_sf)
    zip_geo$nearest_hospital <- facilities_sf$facility_name[nearest_hospital_index]
    zip_geo$dist_to_hospital <- as.vector(st_distance(zip_geo, 
                                                      facilities_sf[nearest_hospital_index,], 
                                                      by_element=TRUE) / 1609.344)
    hospital_distance <- as.data.frame(zip_geo) %>% 
      mutate(zip = as.factor(ZCTA5CE10)) %>% 
      select(c(zip,nearest_hospital,dist_to_hospital))
    
    rm(nearest_hospital_index,zip_geo)  
    }  
  
# Make a function that can be applied to any dataset with the zip variable, 
# adds distance to nearest hospital for each zip

  zip_distance_to_hospital <- function(x){
    x %>% left_join(hospital_distance)
  }  

# We use a function to transform the variables in the policing dataset
fun_sumpol <- function(x){
  x %>%
  #group_by(zip) %>%
  summarize(
    npol        = sum(census[type %in% c("arrest","summons")],na.rm=TRUE),
    npol_all    = sum(census,na.rm = TRUE),
    nstop       = sum(census[type=="SQF"],na.rm=TRUE),
    narrest     = sum(census[type=="arrest"],na.rm=TRUE),
    nsummon     = sum(census[type=="summons"],na.rm=TRUE),
    
    npol_black        = sum(census[type %in% c("arrest","summons") & raceeth == "BLACK" ],na.rm=TRUE),
    npol_all_black    = sum(census[raceeth == "BLACK"],na.rm = TRUE),
    nstop_black       = sum(census[type=="SQF" & raceeth == "BLACK"],na.rm=TRUE),
    narrest_black     = sum(census[type=="arrest" & raceeth == "BLACK"],na.rm=TRUE),
    nsummon_black     = sum(census[type=="summons" & raceeth == "BLACK"],na.rm=TRUE),
  ) 
}

# Similarly, we use a function to create analytic variables in the hospitalization data
fun_sumsparcs <- function(x){
  x %>%
    summarize(

      npsych_ip = sum(census[psychiatric_9 == 1 & visit_type == "ip"],na.rm=TRUE),
  
      psychdays_ip = sum(days[psychiatric_9 == 1 & visit_type == "ip"],na.rm=TRUE),
      psychdays_ip_18 = sum(days[agegroup == "<18" & psychiatric_9 == 1 & visit_type == "ip"],na.rm=TRUE),
      psychdays_ip_24 = sum(days[agegroup == "18-24" & psychiatric_9 == 1 & visit_type == "ip"],na.rm=TRUE),
      
      psychdays_ip_30days = sum(days[psychiatric_9 == 1 & visit_type == "ip" & days <= 30],na.rm=TRUE),
      psychdays_ip_60days = sum(days[psychiatric_9 == 1 & visit_type == "ip" & days <= 60],na.rm=TRUE),
      
      psych1days_ip_30days = sum(days[psych1 == 1 & visit_type == "ip"& days <= 30],na.rm=TRUE),
      psych1days_ip_60days = sum(days[psych1 == 1 & visit_type == "ip"& days <= 60],na.rm=TRUE),
      
      psych1days_ip = sum(days[psych1 == 1 & visit_type == "ip"],na.rm=TRUE),
      npsych1_ip = sum(census[psych1 == 1 & visit_type == "ip"],na.rm=TRUE),
      
      npsych_18_ip = sum(census[agegroup == "<18" & psychiatric_9 == 1 & visit_type == "ip"],na.rm=TRUE),
      npsych_24_ip = sum(census[agegroup == "18-24" & psychiatric_9 == 1 & visit_type == "ip"],na.rm=TRUE),
      
      npsych1_18_ip = sum(census[agegroup == "<18" & psych1 == 1 & visit_type == "ip"],na.rm=TRUE),
      npsych1_24_ip = sum(census[agegroup == "18-24" & psych1 == 1 & visit_type == "ip"],na.rm=TRUE),
      
      npsych_ip_m = sum(census[psychiatric_9 == 1 & visit_type == "ip" & sex == "M"],na.rm=TRUE),
      psychdays_ip_m = sum(days[psychiatric_9 == 1 & visit_type == "ip" & sex == "M"],na.rm=TRUE),
      
      npsych_ip_f = sum(census[psychiatric_9 == 1 & visit_type == "ip" & sex == "F"],na.rm=TRUE),
      psychdays_ip_f = sum(days[psychiatric_9 == 1 & visit_type == "ip" & sex == "F"],na.rm=TRUE),
    )
}

# Here we aggregate the individual-level data to ZCTAs for the entire timeframe, and also by year-month
pol_zip <- pol %>%
  group_by(zip) %>%
  fun_sumpol()

pol_zip_yearmonth <- pol %>%
  group_by(zip,year,yearmonth,month)%>%
  fun_sumpol()

sparcs_zip <- sparcs %>%
  group_by(zip) %>%
  fun_sumsparcs()

sparcs_zip_yearmonth <- sparcs %>%
  group_by(zip,year,yearmonth,month)%>%
  fun_sumsparcs()

# Identify ZIP codes common to the Policing and SPARCS data, save as vector for future use
zips <- as.factor(as.character(unique(sparcs$zip[sparcs$zip %in% unique(pol$zip)])))
#save(zips, file = "Data/Output/zips")

# Create empty dataframes with month-year combinations from 2006-2014
# We will subsequently merge both the policing and hosp data into this dataframe
# Note: We do this to ensure that we have a record for every month-year combination, 
#       even those without any hospitalization or policing events

dates_06_08 <- seq(as.Date("2006/1/1"), by = "month", length.out = 36)
dates_09_11 <- seq(as.Date("2009/1/1"), by = "month", length.out = 36)
dates_12_14 <- seq(as.Date("2012/1/1"), by = "month", length.out = 36)

sparcspol_zip_yearmonth_06_08 <- expand.grid(zips,dates_06_08)
names(sparcspol_zip_yearmonth_06_08) <- c("zip","yearmonth")

sparcspol_zip_yearmonth_09_11 <- expand.grid(zips,dates_09_11)
names(sparcspol_zip_yearmonth_09_11) <- c("zip","yearmonth")

sparcspol_zip_yearmonth_12_14 <- expand.grid(zips,dates_12_14)
names(sparcspol_zip_yearmonth_12_14) <- c("zip","yearmonth")

# Bind ACS data from 3 different 5-year estimates
acs_comb <- rbind(acs_zip_2007_2011,
                  acs_zip_2009_2013,
                  acs_zip_2012_2016)

# Bind empty dataframes and fill with ACS data
sparcspol_zip_yearmonth_empty <- rbind(sparcspol_zip_yearmonth_06_08,
                                 sparcspol_zip_yearmonth_09_11,
                                 sparcspol_zip_yearmonth_12_14) %>% 
  mutate(year_range = case_when(year(yearmonth) %in% 2006:2008 ~ "2007-2011",
                                year(yearmonth) %in% 2009:2011 ~ "2009-2013",
                                year(yearmonth) %in% 2012:2014 ~ "2012-2016")) %>%
                              left_join(acs_comb,
                                        by=c("zip","year_range"))

rm(sparcspol_zip_yearmonth_06_08,sparcspol_zip_yearmonth_09_11,sparcspol_zip_yearmonth_12_14)

sparcspol_zip_empty <- as.data.frame(zips) %>% arrange(zips)
names(sparcspol_zip_empty) <- "zip"

# Combine Policing and Hospitalization Data with ACS Data (All data aggregated for the entire 9-year study period)
sparcspol_zip <- left_join(sparcspol_zip_empty,acs_zip_2009_2013,by="zip") %>% 
                 left_join(pol_zip,by="zip") %>% 
                 left_join(sparcs_zip,by="zip") %>%
                 left_join(hospital_distance,by="zip") %>%
                 left_join(zip_area,by="zip") %>%
                 select(-geometry) %>% 
  # Generate new analytic variables for 9-year timeframe
                 mutate(npol_rate = (npol/(acs_total_pop)*1000)/9,
                        npol_all_rate = (npol_all/(acs_total_pop)*1000)/9,
                        nstop_rate = (nstop/(acs_total_pop)*1000)/9,
                        narrest_rate = (narrest/(acs_total_pop)*1000)/9,
                        nsummon_rate = (nsummon/(acs_total_pop)*1000)/9,
                        
                        pct_pol_black       = ((narrest_black + nstop_black) / (narrest + nstop))*100,
                        pct_pol_black_diff  = pct_pol_black - acs_pct_black,
                        pct_pol_black_ratio  = pct_pol_black/acs_pct_black ,
                        
                        psychdays_rate_ip = (psychdays_ip/(acs_age_10_24)*1000)/9,
                        npsych_rate_ip = (npsych_ip/(acs_age_10_24)*1000)/9,
                        
                        pop_dens = acs_total_pop / area * 1000
                        ) 

sparcspol_zip <- within(sparcspol_zip, acs_pct_black_q3 <- as.integer(cut(acs_pct_black, quantile(acs_pct_black, probs=0:3/3, na.rm = TRUE), include.lowest=TRUE)))
sparcspol_zip$acs_pct_black_q3 <- as.factor(sparcspol_zip$acs_pct_black_q3)



# Combine Policing and Hospitalization Data with ACS Data (Data aggregated by year-month)
sparcspol_zip_yearmonth <- sparcspol_zip_yearmonth_empty %>%
                           left_join(pol_zip_yearmonth,by=c("zip","yearmonth")) %>% 
                           left_join(sparcs_zip_yearmonth,by=c("zip","year","yearmonth","month")) %>%
                           left_join(hospital_distance,by="zip") %>%
                           left_join(zip_area,by="zip") %>%
                           select(-geometry) %>% 
# Generate new analytic variables for 9-year timeframe
                           mutate(npol_rate = (npol/(acs_total_pop)*1000*12),
                                  npol_all_rate = (npol_all/(acs_total_pop)*1000*12),
                                  nstop_rate = (nstop/(acs_total_pop)*1000*12),
                                  narrest_rate = (narrest/(acs_total_pop)*1000*12),
                                  nsummon_rate = (nsummon/(acs_total_pop)*1000*12),
                                  pop_dens = acs_total_pop / area,
                                  pct_pol_black       = ((narrest_black + nstop_black) / (narrest + nstop))*100,
                                  pct_pol_black_diff  = pct_pol_black - acs_pct_black,
                                  pct_pol_black_ratio  = pct_pol_black/acs_pct_black,
                                  
                                  psychdays_rate_ip = (psychdays_ip/acs_age_10_24)*1000*12,
                                  npsych_rate_ip = (npsych_ip/(acs_age_10_24)*1000*12),
                          ) 

sparcspol_zip_yearmonth <- within(sparcspol_zip_yearmonth, acs_pct_black_q3 <- as.integer(cut(acs_pct_black, quantile(acs_pct_black, probs=0:3/3, na.rm = TRUE), include.lowest=TRUE)))
sparcspol_zip_yearmonth$acs_pct_black_q3 <- as.factor(sparcspol_zip_yearmonth$acs_pct_black_q3)

sparcspol_zip_yearmonth <- sparcspol_zip_yearmonth %>% filter(zip != "11003" & zip != "11040")
sparcspol_zip <- sparcspol_zip %>% filter(zip != "11003" & zip != "11040")

sparcspol_zip_yearmonth <- sparcspol_zip_yearmonth %>% 
  mutate(acs_pct_alone_z = as.numeric(scale(acs_pct_alone)),
         acs_pct_rent_z = as.numeric(scale(acs_pct_rent)),
         acs_pct_moved_z = as.numeric(scale(acs_pct_moved)),
         acs_pct_unmarried_z = as.numeric(scale(acs_pct_unmarried)),
         social_frag = (acs_pct_alone_z+acs_pct_rent_z+acs_pct_moved_z+acs_pct_unmarried_z)/4)


# A few infinite values may have been generated for rate calculations with a zero or NA denominator
sparcspol_zip_yearmonth[sapply(sparcspol_zip_yearmonth, is.infinite)] <- NA
sparcspol_zip[sapply(sparcspol_zip, is.infinite)] <- NA

(proc.time() - ptm)/60

# save(sparcspol_zip, file="Data/Output/sparcspol_zip")
# save(sparcspol_zip_yearmonth, file="Data/Output/sparcspol_zip_yearmonth")
