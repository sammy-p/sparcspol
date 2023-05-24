library(tidycensus)

# Note that this code requires a Census API Key to run fully. See documentation for tidycensus package
# API Key and Variable Selection for American Community Survey 5-year estimates from the tidycensus package
# NOTE: THIS REQUIRES AN API KEY. Request a key here: https://api.census.gov/data/key_signup.html
census_api_key("INSERT API KEY HERE", overwrite = TRUE, install = TRUE)
readRenviron("~/.Renviron")

# Set working directory
#setwd("C:/WORKINGDIR/")

ptm <- proc.time()

library(tidycensus)
library(tidyverse)
library(lubridate)
library(zoo)
library(janitor)

# Create a function to crosswalk UHF districts and ZCTAs (Note: This was not used in final analysis) 
# This function can be run on any vector of ZCTAs
zip_to_uhf <- function(x) {
  x <- x %>%
    mutate(uhf_name = as.factor(case_when(
      zip %in% c(10463,10471)  ~ "Kingsbridge-Riverdale",
      zip %in% c(10464,10466,10469,10470,10475)  ~ "NE Bronx",
      zip %in% c(10458,10467,10468)  ~ "Fordham-Bronx Park",
      zip %in% c(10461,10462,10465,10472,10473)  ~ "Pelham-Throgs Neck",
      zip %in% c(10453,10457,10460)  ~ "Crotona-Tremont",
      zip %in% c(10451,10452,10456)  ~ "High-Bridge-Morisania",
      zip %in% c(10454,10455,10459,10474)  ~ "Hunts Point-Mott Haven",
      zip %in% c(11211,11222)  ~ "Greenpoint",
      zip %in% c(11201,11205,11215,11217,11231)  ~ "Downtown Heightts-Slope",
      zip %in% c(11212,11213,11216,11233,11238)  ~ "Bedford Stuyvesant-Crown Heights",
      zip %in% c(11207,11208)  ~ "East New York",
      zip %in% c(11220,11232)  ~ "Sunset Park",
      zip %in% c(11204,11218,11219,11230)  ~ "Borough Park",
      zip %in% c(11203,11210,11225,11226)  ~ "East Flatbush-Flatbush",
      zip %in% c(11234,11236,11239)  ~ "Canarsie-Flatlands",
      zip %in% c(11209,11214,11228)  ~ "Bensonhurst-Bay Ridge",
      zip %in% c(11223,11224,11229,11235)  ~ "Coney Island-Sheepshead Bay",
      zip %in% c(11206,11221,11237)  ~ "Williamsburg-Bushwick",
      zip %in% c(10031,10032,10033,10034,10040)  ~ "Washington Heights-Inwood",
      zip %in% c(10026,10027,10030,10037,10039,10115)  ~ "Central Harlem-Morningside Heights",
      zip %in% c(10029,10035)  ~ "East Harlem",
      zip %in% c(10023,10024,10025,10069)  ~ "Upper West Side",
      zip %in% c(10021,10028,10044,10128,10075,10065,10162)  ~ "Upper East Side",
      zip %in% c(10001,10011,10018,10019,10020,10036,10103,10110,10111,10112,10119)  ~ "Chelsea-Clinton",
      zip %in% c(10010,10016,10017,10022,10152,10153, 10154,10165,10167, 10168, 10169, 10171, 10172, 10173, 10174, 10177)  ~ "Gramercy Park-Murray",
      zip %in% c(10012,10013,10014)  ~ "Greenwich Village-Soho",
      zip %in% c(10002,10003,10009)  ~ "Union Square-Lower East Side",
      zip %in% c(10004,10005,10006,10007,10038,10280,10271, 10278, 10279,10282)  ~ "Lower Manhattan",
      zip %in% c(11101,11102,11103,11104,11105,11106,11109)  ~ "Long Island City-Astoria",
      zip %in% c(11368,11369,11370,11372,11373,11377,11378)  ~ "West Queens",
      zip %in% c(11354,11355,11356,11357,11358,11359,11360,11351)  ~ "Flushing-Clearview",
      zip %in% c(11361,11362,11363,11364,11040)  ~ "Bayside-Littleneck",
      zip %in% c(11374,11375,11379,11385)  ~ "Ridgewood-Forest Hills",
      zip %in% c(11365,11366,11367)  ~ "Fresh Meadows",
      zip %in% c(11414,11415,11416,11417,11418,11419,11420,11421,11424)  ~ "SW Queens",
      zip %in% c(11412,11423,11430,11432,11433,11434,11435,11436,11001,11451)  ~ "Jamaica",
      zip %in% c(11004,11005,11411,11413,11422,11426,11427,11428,11429)  ~ "SE Queens",
      zip %in% c(11691,11692,11693,11694,11695,11697)  ~ "Rockaway",
      zip %in% c(10302,10303,10310)  ~ "Port Richmond",
      zip %in% c(10301,10304,10305)  ~ "Stapleton St. George",
      zip %in% c(10314,10311)  ~ "Willowbrook",
      zip %in% c(10306,10307,10308,10309,10312)  ~ "South Beach-Tottenville")),
      
      uhf_code = as.factor(case_when(
        zip %in% c(10463,10471)  ~ "101",
        zip %in% c(10464,10466,10469,10470,10475)  ~ "102",
        zip %in% c(10458,10467,10468)  ~ "103",
        zip %in% c(10461,10462,10465,10472,10473)  ~ "104",
        zip %in% c(10453,10457,10460)  ~ "105",
        zip %in% c(10451,10452,10456)  ~ "106",
        zip %in% c(10454,10455,10459,10474)  ~ "107",
        zip %in% c(11211,11222)  ~ "201",
        zip %in% c(11201,11205,11215,11217,11231)  ~ "202",
        zip %in% c(11212,11213,11216,11233,11238)  ~ "203",
        zip %in% c(11207,11208)  ~ "204",
        zip %in% c(11220,11232)  ~ "205",
        zip %in% c(11204,11218,11219,11230)  ~ "206",
        zip %in% c(11203,11210,11225,11226)  ~ "207",
        zip %in% c(11234,11236,11239)  ~ "208",
        zip %in% c(11209,11214,11228)  ~ "209",
        zip %in% c(11223,11224,11229,11235)  ~ "210",
        zip %in% c(11206,11221,11237)  ~ "211",
        zip %in% c(10031,10032,10033,10034,10040)  ~ "301",
        zip %in% c(10026,10027,10030,10037,10039,10115)  ~ "302",
        zip %in% c(10029,10035)  ~ "303",
        zip %in% c(10023,10024,10025,10069)  ~ "304",
        zip %in% c(10021,10028,10044,10128,10075,10065,10162)  ~ "305",
        zip %in% c(10001,10011,10018,10019,10020,10036,10103,10110,10111,10112,10119)  ~ "306",
        zip %in% c(10010,10016,10017,10022,10152,10153, 10154,10165,10167, 10168, 10169, 10171, 10172, 10173, 10174, 10177)  ~ "307",
        zip %in% c(10012,10013,10014)  ~ "308",
        zip %in% c(10002,10003,10009)  ~ "309",
        zip %in% c(10004,10005,10006,10007,10038,10280,10271, 10278, 10279,10282)  ~ "310",
        zip %in% c(11101,11102,11103,11104,11105,11106,11109)  ~ "401",
        zip %in% c(11368,11369,11370,11372,11373,11377,11378)  ~ "402",
        zip %in% c(11354,11355,11356,11357,11358,11359,11360,11351)  ~ "403",
        zip %in% c(11361,11362,11363,11364,11040)  ~ "404",
        zip %in% c(11374,11375,11379,11385)  ~ "405",
        zip %in% c(11365,11366,11367)  ~ "406",
        zip %in% c(11414,11415,11416,11417,11418,11419,11420,11421,11424)  ~ "407",
        zip %in% c(11412,11423,11430,11432,11433,11434,11435,11436,11001,11451)  ~ "408",
        zip %in% c(11004,11005,11411,11413,11422,11426,11427,11428,11429)  ~ "409",
        zip %in% c(11691,11692,11693,11694,11695,11697)  ~ "410",
        zip %in% c(10302,10303,10310)  ~ "501",
        zip %in% c(10301,10304,10305)  ~ "502",
        zip %in% c(10314,10311)  ~ "503",
        zip %in% c(10306,10307,10308,10309,10312)  ~ "504"))
    )
  
  x <- x %>%
    mutate(
      borough = case_when(
        uhf_code %in% 101:107 ~ "Bronx",
        uhf_code %in% 201:211 ~ "Brooklyn",
        uhf_code %in% 301:310 ~ "Manhattan",
        uhf_code %in% 401:410 ~ "Queens",
        uhf_code %in% 501:504 ~ "Staten Island")
    )
}

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


# List of ACS Variable Calls. This list will be supplied to the function which accesses ACS data.
# A function is used because we call data from 3 different ACS datasets
{
get_acsvars <- c(
  acs_age_total_pop = "B01001_001",
  acs_age_10_14m = "B01001_005",
  acs_age_15_17m = "B01001_006",
  acs_age_18_19m = "B01001_007",
  acs_age_20m = "B01001_008",
  acs_age_21m = "B01001_009",
  acs_age_22_24m = "B01001_010",

  acs_age_10_14f = "B01001_029",
  acs_age_15_17f = "B01001_030",
  acs_age_18_19f = "B01001_031",
  acs_age_20f = "B01001_032",
  acs_age_21f = "B01001_033",
  acs_age_22_24f = "B01001_034",

  acs_age_median = "B01002_001",

  acs_race_total = "B02001_001",
  acs_race_white = "B03002_003"  ,
  acs_race_hispanic = "B03002_013",
  acs_race_hispanic2 = "B03001_003",
  acs_race_black = "B02001_003"  ,
  acs_race_native = "B02001_004"  ,
  acs_race_asian = "B02001_005"  ,
  acs_race_hawaiian = "B02001_006"  ,
  acs_race_other = "B02001_007"  ,
  acs_race_two1 = "B02001_008"  ,
  acs_race_two2 = "B02001_009"  ,
  acs_race_two3 = "B02001_010"  ,
  acs_SNAP_yes = "B22008_002",
  acs_SNAP_no = "B22008_003",
  acs_poverty_total = "B17001_001",
  acs_poverty_below = "B17001_002",
  acs_median_income_raw = "B19013_001",
  
  #New vars
  acs_median_housing_cost = "B25105_001",
  acs_median_gross_rent = "B25111_001",
  acs_condition_estimate_total = "B25123_001",
  acs_condition_owner_total = "B25123_002",
  acs_condition_owner_with_one_selected_condition = "B25123_003",
  acs_condition_owner_with_two_selected_conditions = "B25123_004",
  acs_condition_owner_with_three_selected_conditions = "B25123_005",
  acs_condition_owner_with_four_selected_conditions = "B25123_006",
  acs_condition_owner_no_selected_conditions = "B25123_007",
  acs_condition_renter_total = "B25123_008",
  acs_condition_renter_with_one_selected_condition = "B25123_009",
  acs_condition_renter_with_two_selected_conditions = "B25123_010",
  acs_condition_renter_with_three_selected_conditions = "B25123_011",
  acs_condition_renter_with_four_selected_conditions = "B25123_012",
  acs_condition_renter_no_selected_conditions = "B25123_013",
  
  marital_total = "B12001_001",
  marital_never_male = "B12001_003",
  marital_divorced_male = "B12001_010",
  marital_separated_male = "B12001_007",
  marital_never_female = "B12001_012",
  marital_divorced_female = "B12001_019",
  marital_separated_female = "B12001_016",

  acs_occupied = "B25008_001",
  acs_occupied_owner = "B25008_002",
  acs_occupied_renter = "B25008_003",

  mobility_total = "B07001_001",
  mobility_samehouse = "B07001_017",
  
  # household_total = "B09016_001E",
  # household_alone_male = "B09016_022E",
  # household_alone_female = "B09016_025E",

  #End new vars
  acs_labor_force = "B23025_003",
  acs_unemployed = "B23025_005",
  acs_gini_raw = "B19083_001",
  acs_edu_male_tot = "B15002_002",
  acs_edu_male_0 = "B15002_003",
  acs_edu_male_0_4 = "B15002_004",
  acs_edu_male_5_6 = "B15002_005",
  acs_edu_male_7_8 = "B15002_006",
  acs_edu_male_9 = "B15002_007",
  acs_edu_male_10 = "B15002_008",
  acs_edu_male_11 = "B15002_009",
  acs_edu_male_12 = "B15002_010",
  acs_edu_male_HS = "B15002_011",
  acs_edu_male_col1 = "B15002_012",
  acs_edu_male_col2 = "B15002_013",
  acs_edu_male_aa = "B15002_014",
  acs_edu_male_ba = "B15002_015",
  acs_edu_male_ma = "B15002_016",
  acs_edu_male_prof = "B15002_017",
  acs_edu_male_doc = "B15002_018",
  acs_edu_female_tot = "B15002_019",
  acs_edu_female_0 = "B15002_020",
  acs_edu_female_0_4 = "B15002_021",
  acs_edu_female_5_6 = "B15002_022",
  acs_edu_female_7_8 = "B15002_023",
  acs_edu_female_9 = "B15002_024",
  acs_edu_female_10 = "B15002_025",
  acs_edu_female_11 = "B15002_026",
  acs_edu_female_12 = "B15002_027",
  acs_edu_female_HS = "B15002_028",
  acs_edu_female_col1 = "B15002_029",
  acs_edu_female_col2 = "B15002_030",
  acs_edu_female_aa = "B15002_031",
  acs_edu_female_ba = "B15002_032",
  acs_edu_female_ma = "B15002_033",
  acs_edu_female_prof = "B15002_034",
  acs_edu_female_doc = "B15002_035"
)
  
}

# List of raw ACS variables that can be removed after transformation 
# (In order to declutter our data frame once we have the variables we want)
{
 acsvars <- c(
  "acs_age_total_pop", "acs_age_10_14m","acs_age_15_17m","acs_age_18_19m","acs_age_20m","acs_age_21m","acs_age_22_24m","acs_age_10_14f",
  "acs_age_15_17f","acs_age_18_19f","acs_age_20f","acs_age_21f","acs_age_22_24f","acs_age_median","acs_race_total","acs_race_white",
  "acs_race_black","acs_race_native","acs_race_asian","acs_race_hawaiian" ,"acs_race_other","acs_race_two1","acs_race_two2",
  "acs_race_two3","acs_occupied","acs_occupied_owner","acs_occupied_renter","acs_SNAP_yes","acs_SNAP_no","acs_poverty_total",
  "acs_poverty_below","acs_median_income_raw","acs_labor_force","acs_unemployed","acs_gini_raw","acs_edu_male_tot","acs_edu_male_0",
  "acs_edu_male_0_4","acs_edu_male_5_6","acs_edu_male_7_8","acs_edu_male_9","acs_edu_male_10","acs_edu_male_11","acs_edu_male_12",
  "acs_edu_male_HS","acs_edu_male_col1","acs_edu_male_col2","acs_edu_male_aa","acs_edu_male_ba","acs_edu_male_ma","acs_edu_male_prof",
  "acs_edu_male_doc","acs_edu_female_tot","acs_edu_female_0","acs_edu_female_0_4","acs_edu_female_5_6","acs_edu_female_7_8",
  "acs_edu_female_9","acs_edu_female_10","acs_edu_female_11","acs_edu_female_12","acs_edu_female_HS","acs_edu_female_col1",
  "acs_edu_female_col2","acs_edu_female_aa","acs_edu_female_ba","acs_edu_female_ma","acs_edu_female_prof","acs_edu_female_doc",
  "acs_condition_owner_total" ,  "acs_condition_owner_with_one_selected_condition" ,  "acs_condition_owner_with_two_selected_conditions" ,  
  "acs_condition_owner_with_three_selected_conditions" ,  "acs_condition_owner_with_four_selected_conditions", 
  "acs_condition_owner_no_selected_conditions" , "acs_condition_renter_total",  "acs_condition_renter_with_one_selected_condition" ,  
  "acs_condition_renter_with_two_selected_conditions",  "acs_condition_renter_with_three_selected_conditions",  
  "acs_condition_renter_with_four_selected_conditions" ,  "acs_condition_renter_no_selected_conditions" ,"acs_occupied","acs_occupied_owner","acs_occupied_renter","mobility_total","mobility_samehouse","household_total","household_alone_male","household_alone_female"
)
}

# Get ACS 5-year estimates for 2007-2011, wrangle the variables
acs_zip_2007_2011_raw <- get_acs(geography = "zcta",
                                 variables = c(get_acsvars,c(household_total = "B09016_001",
                                                           household_alone_male = "B09016_022",
                                                           household_alone_female = "B09016_025")),
                                 state = "NY",
                                 year = 2011,
                                 survey="acs5",
                                 show_call = TRUE) %>%
  select(-c(moe,GEOID)) %>%
  spread(key = c("variable"), value = "estimate") %>%
  mutate(
    zip = substr(NAME,7,12),
    year_range = "2007-2011") 

# Get ACS 5-year estimates for 2009-2013, wrangle the variables, and apply UHF/Borough coding to each ZIP code
acs_zip_2009_2013_raw <- get_acs(geography = "zcta",
                                 variables = c(get_acsvars,c(household_total = "B09019_001",
                                                           household_alone_male = "B09019_027",
                                                           household_alone_female = "B09019_030")),
                                 state = "NY",
                                 year = 2013,
                                 survey="acs5",
                                 show_call = TRUE) %>%
  select(-c(moe,GEOID)) %>%
  spread(key = c("variable"), value = "estimate") %>%
  mutate(
    zip = substr(NAME,7,12),
    year_range = "2009-2013")

# Get ACS 5-year estimates for 2012-2016, wrangle the variables, and apply UHF/Borough coding to each ZIP code

acs_zip_2012_2016_raw <- get_acs(geography = "zcta",
                                 variables = c(get_acsvars,c(household_total = "B09019_001",
                                                           household_alone_male = "B09019_027",
                                                           household_alone_female = "B09019_030")),
                                 state = "NY",
                                 year = 2016,
                                 survey="acs5",
                                 show_call = TRUE) %>%
  select(-c(moe,GEOID)) %>%
  spread(key = c("variable"), value = "estimate") %>%
  mutate(
    zip = substr(NAME,7,12),
    year_range = "2012-2016") 

# Function to transform ACS raw variables into useful variables for analysis
acs5_transform <- function(x){
  x %>%
    mutate(
      # POPULATION COUNTS FOR AGE GROUPS
      acs_total_pop = acs_age_total_pop,
      
      acs_age_10_17 = acs_age_10_14m + acs_age_15_17m + acs_age_10_14f + acs_age_15_17f,
      acs_age_18_24 = acs_age_18_19m + acs_age_20m + acs_age_21m + acs_age_22_24m + acs_age_18_19f + acs_age_20f + acs_age_21f + acs_age_22_24f,
      acs_age_10_24 = acs_age_10_17 + acs_age_18_24,
      
      acs_pct_10_17 = acs_age_10_17 / acs_total_pop,
      acs_pct_18_24 = acs_age_18_24 / acs_total_pop,
      acs_pct_10_24 = acs_age_10_24 / acs_total_pop,
      
      acs_age_10_17_m = acs_age_10_14m + acs_age_15_17m,
      acs_age_18_24_m = acs_age_18_19m + acs_age_20m + acs_age_21m + acs_age_22_24m,
      acs_age_10_24_m = acs_age_10_17_m + acs_age_18_24_m,
      
      acs_age_10_17_f = acs_age_10_14f + acs_age_15_17f,
      acs_age_18_24_f = acs_age_18_19f + acs_age_20f + acs_age_21f + acs_age_22_24f,
      acs_age_10_24_f = acs_age_10_17_f + acs_age_18_24_f,
       
      # SOCIO-ECONOMIC NEIGHBORHOOD CHARACTERISTICS
      
      acs_pct_SNAP = round(acs_SNAP_yes / (acs_SNAP_yes + acs_SNAP_no)*100,digits = 2),
      acs_pct_poverty = round(acs_poverty_below/acs_poverty_total * 100,digits=2),
      acs_pct_unemployed = round(acs_unemployed/acs_labor_force* 100,digits=2),
      
      acs_pct_conditions_renter_any = round((1 - (acs_condition_renter_no_selected_conditions/acs_condition_renter_total))*100,digits=2),
      acs_pct_conditions_renter_2plus = round((1 - ( (acs_condition_renter_no_selected_conditions + acs_condition_renter_with_one_selected_condition) /acs_condition_renter_total))*100,digits=2),
      
      acs_pct_conditions_owner = round((1 - (acs_condition_owner_no_selected_conditions/acs_condition_owner_total))*100,digits=2),
      acs_pct_conditions_owner_2plus = round((1 - ( (acs_condition_owner_no_selected_conditions + acs_condition_owner_with_one_selected_condition) /acs_condition_owner_total))*100,digits=2),
      
      # SOCIAL FRAGMENTATION
      # Indicators of social isolation or social fragmentation were calculated by 
      # combining standardized (z-score) transformations of the following components:
      # % population living alone.
      # % population not unmarried or separated.
      # % population in rented accommodation.
      # % population who moved house in the previous year.
      
      acs_pct_alone = ((household_alone_male + household_alone_female) / household_total) * 100,
      acs_pct_rent = round(acs_occupied_renter / (acs_occupied_renter + acs_occupied_owner)*100,digits = 2),
      acs_pct_moved = (1 - (mobility_samehouse/mobility_total)) * 100,
      acs_pct_unmarried = ((marital_never_male + marital_never_female + 
                                      marital_divorced_male + marital_divorced_female+
                                      marital_separated_male + marital_separated_female) / marital_total) * 100,
      

      # EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
      acs_edu_pct_highschool = round ((acs_edu_male_HS + 
                                         acs_edu_female_HS +
                                         acs_edu_male_col1+
                                         acs_edu_male_col2+
                                         acs_edu_female_col1+
                                         acs_edu_female_col2) / 
                                        (acs_edu_male_tot + acs_edu_female_tot) * 100,digits=2),
      acs_edu_pct_lthighschool = round(
        ((acs_edu_male_0 +
            acs_edu_male_0_4 + acs_edu_male_5_6 +
            acs_edu_male_7_8 + acs_edu_male_9 +
            acs_edu_male_10 + acs_edu_male_11+
            acs_edu_male_12 + acs_edu_female_0+
            acs_edu_female_0_4+ acs_edu_female_5_6+
            acs_edu_female_7_8+ acs_edu_female_9+
            acs_edu_female_10+ acs_edu_female_11+ acs_edu_female_12) / 
           (acs_edu_male_tot + acs_edu_female_tot)) * 100,digits=2),
      acs_edu_pct_gthighschool = round(
        ((
          acs_edu_male_aa+
            acs_edu_male_ba+
            acs_edu_male_ma+
            acs_edu_male_prof+
            acs_edu_male_doc+
            acs_edu_female_aa+
            acs_edu_female_ba+
            acs_edu_female_ma+
            acs_edu_female_prof+
            acs_edu_female_doc) / (acs_edu_male_tot + acs_edu_female_tot)) * 100,digits=2),
      
      # NEIGHBORHOOD RACIAL CHARACTERISTICS
      acs_pct_white =  round ((acs_race_white  / acs_race_total) * 100,digits=2),
      acs_pct_poc = round (((acs_race_black +acs_race_hispanic +
                               acs_race_native +
                               acs_race_asian +
                               acs_race_hawaiian +
                               acs_race_other +
                               acs_race_two1 +
                               acs_race_two2 +
                               acs_race_two3)  / acs_race_total) * 100,digits=2),
      acs_pct_black =  round ((acs_race_black  / acs_race_total) * 100,digits=2),
      acs_pct_hispanic =  round ((acs_race_hispanic  / acs_race_total) * 100,digits=2),
      
      # These variables are already ZIP-level summaries, cannot be aggregated to UHF
      acs_median_income = acs_median_income_raw,
      acs_gini = acs_gini_raw
    ) %>% select(-acsvars)
}

####################

# Use function to recode raw ACS variables from all 3 dataframes
acs_zip_2007_2011 <- acs5_transform(acs_zip_2007_2011_raw)
acs_zip_2009_2013 <- acs5_transform(acs_zip_2009_2013_raw)
acs_zip_2012_2016 <- acs5_transform(acs_zip_2012_2016_raw)

rm(acs_zip_2007_2011_raw,acs_zip_2009_2013_raw,acs_zip_2012_2016_raw)

# save(acs_zip_2007_2011, "C:/WORKINGDIR/Data/Output")
# save(acs_zip_2009_2013, "C:/WORKINGDIR/Data/Output")
# save(acs_zip_2012_2016, "C:/WORKINGDIR/Data/Output")