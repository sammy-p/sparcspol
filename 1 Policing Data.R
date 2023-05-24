library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(tigris)
library(lubridate)
library(zoo)

# Set working directory
setwd("C:/WD")

summons <- read_csv("Data/Raw/NYPD_Criminal_Court_Summons__Historic_.csv") %>% 
  rename_with(tolower) %>% 
  #filter(age_group %in% c("<18","18-24"))%>%
  mutate(key = summons_key,
         type = "summons",
         date = as.Date(summons_date,format="%m/%d/%Y"),
         year = year(date),
         yearmonth = as.yearmon(date),
         census = 1,
         #lon = sub(" .*", "", substr(gsub("[()]", "", lon_lat),7,99)),
         #lat = sub(".* ", "", substr(gsub("[()]", "", lon_lat),7,99))
         ) %>%
  mutate(age_group = as.factor(case_when(age_group %in% c("<18","18-24","25-44","45-64","65+") ~ age_group ))) %>%
  select(c(key,type,date,year,yearmonth,race,sex,age_group,census,latitude,longitude))

arrests <- read_csv("Data/Raw/NYPD_Arrests_Data__Historic_.csv") %>% 
  rename_with(tolower) %>% 
  #filter(age_group %in% c("<18","18-24")) %>%
  mutate(key = arrest_key,
         type = "arrest",
         date = as.Date(arrest_date,format="%m/%d/%Y"),
         year = year(date),
         yearmonth = as.yearmon(date),
         census = 1,
         #lon = sub(" .*", "", substr(gsub("[()]", "", lon_lat),7,99)),
         #lat = sub(".* ", "", substr(gsub("[()]", "", lon_lat),7,99))
        ) %>% 
  rename(race = perp_race, sex = perp_sex) %>%
  mutate(age_group = as.factor(case_when(age_group %in% c("<18","18-24","25-44","45-64","65+") ~ age_group ))) %>%
  select(c(key,type,date,year,yearmonth,race,sex,age_group,census,latitude,longitude))

# Standardize columns and combine arrests and summonses

pol <- rbind(arrests,summons) %>% 
       filter(is.na(latitude) == FALSE & is.na(longitude) == FALSE)


#Convert the lat/long coordinates to a spatial points dataframe class
polgeo_sp <- SpatialPointsDataFrame(coords = select(pol,longitude,latitude), 
                                    data = pol, proj4string = CRS("+proj=longlat +datum=WGS84"))

ny_ziplist <- c("10007", "10038", "10013", "10004", "10006", "10012", "10282", "10014", "10280", "10005", "10002", "10003", "10011", "10009", "10001", "10036", "10018", "10016", "10010", "10017", "10119", "10110", "10022", "10174", "10154", "10314", "10019", "10112", "10023", "10171", "10028", "10128", "10021", "10075", "10065", "10162", "10024", "10069", "10029", "10025", "10026", "10035", "10037", "10027", "10031", "10030", "10032", "10039", "10033", "10034", "10040", "10454", "10455", "10451", "10459", "10456", "11211", "10474", "10460", "10457", "10458", "10472", "10473", "10462", "10461", "10452", "10453", "10475", "10464", "10465", "10468", "10467", "10466", "10469", "10470", "10471", "10463", "11235", "11224", "11223", "11214", "11229", "11230", "11234", "11210", "11204", "11228", "11219", "11209", "11236", "11203", "11220", "11232", "11218", "11215", "11226", "11212", "11213", "11207", "11225", "11238", "11233", "11208", "11239", "11416", "11385", "11421", "11414", "11231", "11217", "11201", "11216", "11206", "11205", "11221", "11237", "11222", "11692", "11693", "11694", "11697", "11691", "11096", "11419", "11418", "11415", "11417", "11435", "11375", "11433", "11432", "11423", "11357", "11412", "11427", "11428", "11379", "11378", "11373", "11377", "11374", "11429", "11413", "11411", "11422", "11434", "11001", "11426", "11004", "11436", "11040", "11361", "11420", "11366", "11367", "11365", "11364", "11355", "11104", "11101", "11360", "11354", "11356", "11358", "11368", "11372", "11362", "11363", "11105", "11106", "11102", "11103", "11370", "11371", "11369", "10304", "10301", "10303", "10310", "10302", "10305", "10177", "10306", "10308", "10312", "10309", "10307", "10168", "10044", "11003", "10173", "10169", "10153", "11109", "10278", "10279", "11021")

# Download NYC shapefile, restricted to the zip codes of interest
zcta_ny <- tigris::zctas(cb = FALSE, starts_with = ny_ziplist, year=2010,state="NY")
plot(zcta_ny$geometry)

# Convert shapefile from Simple Features to spatial class and project to same CRS as policing data
zcta_sp <- spTransform(as_Spatial(zcta_ny), CRS("+proj=longlat +datum=WGS84"))

# Overlay the policing data onto the ZIP codes, and add a new variable to the polgeo dataframe
pol$zip <- over(polgeo_sp, zcta_sp[,"ZCTA5CE10"])$ZCTA5CE10

rm(polgeo_sp)

rm(arrests, summons)
gc()


########### SQF Data
library(readr)
sqf_2006 <- read_csv("Data/Raw/sqf/sqf-2006.csv") %>%
  mutate(date = as.Date(as.character(datestop))) %>%
  select(c(date,race,age,sex,xcoord,ycoord))

sqf_2006$date[sqf_2006$date == "1900-12-31"] <- NA

sqf_2007 <- read_csv("Data/Raw/sqf/sqf-2007.csv") %>%
  mutate(date = as.Date(as.character(datestop), format = "%m%d%Y")) %>%
  select(c(date,race,age,sex,xcoord,ycoord))

sqf_2007$date[sqf_2007$date == "1900-12-31"] <- NA

sqf_2008 <- read_csv("Data/Raw/sqf/sqf-2008.csv")  %>%
  mutate(date = as.Date(as.character(datestop), format = "%m%d%Y")) %>%
  select(c(date,race,age,sex,xcoord,ycoord))

sqf_2008$date[sqf_2008$date == "1900-12-31"] <- NA

sqf_2009 <- read_csv("Data/Raw/sqf/sqf-2009.csv") %>%
  mutate(date = case_when(nchar(as.character(datestop)) == 7 ~ 
                            as.Date(paste0(0,as.character(datestop)), format = "%m%d%Y"),
                          nchar(as.character(datestop)) == 8 ~ 
                            as.Date(as.character(datestop), format = "%m%d%Y"),
                          TRUE ~ NA_Date_)) %>%
  select(c(date,race,age,sex,xcoord,ycoord))


sqf_2010 <- read_csv("Data/Raw/sqf/sqf-2010.csv") %>%
  mutate(date = case_when(nchar(as.character(datestop)) == 7 ~ 
                            as.Date(paste0(0,as.character(datestop)), format = "%m%d%Y"),
                          nchar(as.character(datestop)) == 8 ~ 
                            as.Date(as.character(datestop), format = "%m%d%Y"),
                          TRUE ~ NA_Date_)) %>%
  select(c(date,race,age,sex,xcoord,ycoord))


sqf_2011 <- read_csv("Data/Raw/sqf/sqf-2011.csv") %>%
  mutate(date = case_when(nchar(as.character(datestop)) == 7 ~ 
                            as.Date(paste0(0,as.character(datestop)), format = "%m%d%Y"),
                          nchar(as.character(datestop)) == 8 ~ 
                            as.Date(as.character(datestop), format = "%m%d%Y"),
                          TRUE ~ NA_Date_)) %>%
  select(c(date,race,age,sex,xcoord,ycoord))


sqf_2012 <- read_csv("Data/Raw/sqf/sqf-2012.csv") %>%
  mutate(date = case_when(nchar(as.character(datestop)) == 7 ~ 
                            as.Date(paste0(0,as.character(datestop)), format = "%m%d%Y"),
                          nchar(as.character(datestop)) == 8 ~ 
                            as.Date(as.character(datestop), format = "%m%d%Y"),
                          TRUE ~ NA_Date_)) %>%
  select(c(date,race,age,sex,xcoord,ycoord))


sqf_2013 <- read_csv("Data/Raw/sqf/sqf-2013.csv") %>%
  mutate(date = case_when(nchar(as.character(datestop)) == 7 ~ 
                            as.Date(paste0(0,as.character(datestop)), format = "%m%d%Y"),
                          nchar(as.character(datestop)) == 8 ~ 
                            as.Date(as.character(datestop), format = "%m%d%Y"),
                          TRUE ~ NA_Date_)) %>%
  select(c(date,race,age,sex,xcoord,ycoord))


sqf_2014 <- read_csv("Data/Raw/sqf/sqf-2014.csv")  %>%
  mutate(date = case_when(nchar(as.character(datestop)) == 7 ~ 
                            as.Date(paste0(0,as.character(datestop)), format = "%m%d%Y"),
                          nchar(as.character(datestop)) == 8 ~ 
                            as.Date(as.character(datestop), format = "%m%d%Y"),
                          TRUE ~ NA_Date_)) %>%
  select(c(date,race,age,sex,xcoord,ycoord))



sqf <- rbind(sqf_2006, sqf_2007, sqf_2008, sqf_2009, sqf_2010, sqf_2011, sqf_2012, sqf_2013, sqf_2014 ) 
rm(sqf_2006,sqf_2007,sqf_2008, sqf_2009, sqf_2010, sqf_2011, sqf_2012, sqf_2013, sqf_2014)
gc()

sqf$age[as.numeric(sqf$age) > 98] <- NA

sqf <- sqf %>%
  mutate(key = "NA",
         type = "SQF",
         age = as.numeric(age),
         census = 1,
         year = year(date),
         yearmonth = zoo::as.yearmon(date),
         age_group = case_when(age < 18 ~ "<18",
                               age %in% 18:24 ~ "18-24",
                               age %in% 25:44 ~ "25-44",
                               age %in% 45:64 ~ "45-64",
                               age > 64 ~ "65+")
  ) %>%
  select(-age)


library(sf)

### Code from Megan F. to convert xcoord and ycoord to lat/lon
# SAF must be converted to lon/lat
# X and Y coordinates are in "New York State Plane Coordinate System", (Long Island Zone, NAD 83, units feet (FIPS 3104)
proj4string <- "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"
sqf[c("longitude", "latitude")] <- proj4::project(sqf[c("xcoord", "ycoord")], proj4string, inverse=TRUE)
sqf <- sqf %>% filter(!is.na(xcoord) & !is.na(ycoord))

# # project from world to local coordinate system per David
# # from NYC doc: 12. Latitude and Longitude Coordinates are provided in Global Coordinate System WGS 1984 decimal degrees (EPSG 4326).
# sqf_geo <- sqf %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4236, remove = FALSE) %>% # EPSG 4326 is coord system for latitude/longitude
#   st_transform(32618) # EPSG 32618 (UTM 18N)

# ny_ziplist <- c("10007", "10038", "10013", "10004", "10006", "10012", "10282", "10014", "10280", "10005", "10002", "10003", "10011", "10009", "10001", "10036", "10018", "10016", "10010", "10017", "10119", "10110", "10022", "10174", "10154", "10314", "10019", "10112", "10023", "10171", "10028", "10128", "10021", "10075", "10065", "10162", "10024", "10069", "10029", "10025", "10026", "10035", "10037", "10027", "10031", "10030", "10032", "10039", "10033", "10034", "10040", "10454", "10455", "10451", "10459", "10456", "11211", "10474", "10460", "10457", "10458", "10472", "10473", "10462", "10461", "10452", "10453", "10475", "10464", "10465", "10468", "10467", "10466", "10469", "10470", "10471", "10463", "11235", "11224", "11223", "11214", "11229", "11230", "11234", "11210", "11204", "11228", "11219", "11209", "11236", "11203", "11220", "11232", "11218", "11215", "11226", "11212", "11213", "11207", "11225", "11238", "11233", "11208", "11239", "11416", "11385", "11421", "11414", "11231", "11217", "11201", "11216", "11206", "11205", "11221", "11237", "11222", "11692", "11693", "11694", "11697", "11691", "11096", "11419", "11418", "11415", "11417", "11435", "11375", "11433", "11432", "11423", "11357", "11412", "11427", "11428", "11379", "11378", "11373", "11377", "11374", "11429", "11413", "11411", "11422", "11434", "11001", "11426", "11004", "11436", "11040", "11361", "11420", "11366", "11367", "11365", "11364", "11355", "11104", "11101", "11360", "11354", "11356", "11358", "11368", "11372", "11362", "11363", "11105", "11106", "11102", "11103", "11370", "11371", "11369", "10304", "10301", "10303", "10310", "10302", "10305", "10177", "10306", "10308", "10312", "10309", "10307", "10168", "10044", "11003", "10173", "10169", "10153", "11109", "10278", "10279", "11021")
# 
# # Download NYC shapefile, restricted to the zip codes of interest
# zcta_ny <- tigris::zctas(cb = FALSE, starts_with = ny_ziplist, year=2010,state="NY")
# plot(zcta_ny$geometry)
# 
# # Convert shapefile from Simple Features to spatial class and project to same CRS as policing data
# zcta_sp <- spTransform(as_Spatial(zcta_ny), CRS("+proj=longlat +datum=WGS84"))

#Convert the lat/long coordinates to a spatial points dataframe class
sqf_geo_sp <- SpatialPointsDataFrame(coords = select(sqf,longitude,latitude), 
                                     data = sqf, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Overlay the policing data onto the ZIP codes, and add a new variable to the polgeo dataframe
sqf$zip <- sp::over(sqf_geo_sp, zcta_sp[,"ZCTA5CE10"])$ZCTA5CE10
sqf <- sqf %>% select(-c(xcoord,ycoord))

rm(sqf_geo_sp)
gc()

pol_sqf <- rbind(pol,sqf)

write_csv(pol_sqf, "Data/Output/SQF Arrests Summons All Ages with ZIP.csv")
