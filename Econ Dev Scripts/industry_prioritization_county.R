# Libraries-----------------
library(httr)
library(data.table)
library(magrittr)
library(blsAPI)
library(dplyr)
library(stringr)
library(tidyr)
library(sf)
library(tigris)
library(readxl)
library(janitor)
library(purrr)
library(readr)


#Adjust folder locations as necessary----------------------------------

setwd("C:/Users/allie.jobe/")
raw_data<-"C:/Users/allie.jobe/RMI/US Program - Documents/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/"
acre_data<-"C:/Users/allie.jobe/RMI/US Program - Documents/6_Projects/Clean Regional Economic Development/ACRE/Data"


#-------Set Region Paramater (all counties in your state)
region<-c("35001",
          "35003",
          "35005",
          "35006",
          "35007",
          "35009",
          "35011",
          "35013",
          "35015",
          "35017",
          "35019",
          "35021",
          "35023",
          "35025",
          "35027",
          "35028",
          "35029",
          "35031",
          "35033",
          "35035",
          "35037",
          "35039",
          "35041",
          "35043",
          "35045",
          "35047",
          "35049",
          "35051",
          "35053",
          "35055",
          "35057",
          "35059",
          "35061"
)

# ----- Set State Parameter -----

state_name <- "New Mexico" #Change to desired state full name
state_abbr <- "NM"  # Change to desired state, e.g., "SC", "TN", "NY", etc.

state_fips <- c(
  "AL"="01", "AK"="02", "AZ"="04", "AR"="05", "CA"="06", "CO"="08", "CT"="09",
  "DE"="10", "FL"="12", "GA"="13", "HI"="15", "ID"="16", "IL"="17", "IN"="18",
  "IA"="19", "KS"="20", "KY"="21", "LA"="22", "ME"="23", "MD"="24", "MA"="25",
  "MI"="26", "MN"="27", "MS"="28", "MO"="29", "MT"="30", "NE"="31", "NV"="32",
  "NH"="33", "NJ"="34", "NM"="35", "NY"="36", "NC"="37", "ND"="38", "OH"="39",
  "OK"="40", "OR"="41", "PA"="42", "RI"="44", "SC"="45", "SD"="46", "TN"="47",
  "TX"="48", "UT"="49", "VT"="50", "VA"="51", "WA"="53", "WV"="54", "WI"="55",
  "WY"="56"
)
state_area <- paste0(state_fips[state_abbr], "000")

#Clean Industry NAICS codes and industry category crosswalk
clean_industry_naics <- read.csv(paste0(raw_data,"clean_industry_naics.csv")) %>% select(-X) 

#Employment - Location Quotients, Employment Change------------------------------------------




# ----- Download QCEW Data for 2024 and 2019 -----or most recent BLS full-year and 5 years prior

county_data_2024<-data.frame()

for(county in region){
  df   <- blsQCEW('Area', year = '2024', quarter = 'a', area = county)
  
  county_data_2024<-rbind(county_data_2024,df)
  
}

county_data_2019<-data.frame()

for(county in region){
  df<- blsQCEW('Area', year='2019', quarter= 'a', area = county)
  
  county_data_2019<-rbind(county_data_2019,df)
}

##US-data
USdata       <- blsQCEW('Area', year = '2024', quarter = 'a', area = 'US000')
USdata19     <- blsQCEW('Area', year = '2019', quarter = 'a', area = 'US000')


# Filter to include only disclosed state data (own_code == 5)----------------------------
available_county_data   <- county_data_2024   %>% filter(disclosure_code != "N", own_code == 5)
available_county_data19 <- county_data_2019 %>% filter(disclosure_code != "N", own_code == 5)

# ----- Prepare Pivot Data (from clean_industry_naics) -----
filtered_pivot <- clean_industry_naics %>% 
  mutate(detailed_naics = as.character(X6.Digit.Code),
         naics_3 = str_sub(detailed_naics, 1, 3)) %>% 
  filter(!is.na(detailed_naics))

# ----- Function: Match NAICS Codes at 6-, 5-, 4-, and 3-digit levels -----

match_naics <- function(available_data, naics_metadata) {
  # Start by extracting NAICS levels from industry_code
  matched <- available_data %>%
    mutate(
      industry_code = as.character(industry_code),
      naics_6 = industry_code,
      naics_5 = substr(industry_code, 1, 5),
      naics_4 = substr(industry_code, 1, 4),
      naics_3 = substr(industry_code, 1, 3)
    ) %>%
    
    # 6-digit match
    left_join(naics_metadata, by = c("naics_6" = "detailed_naics")) %>%
    mutate(match_level = ifelse(!is.na(clean_industry), "6-digit", NA_character_)) %>%
    
    # 5-digit match
    left_join(naics_metadata, by = c("naics_5" = "detailed_naics"), suffix = c("", "_5")) %>%
    mutate(match_level = ifelse(is.na(match_level) & !is.na(clean_industry_5), "5-digit", match_level)) %>%
    
    # 4-digit match
    left_join(naics_metadata, by = c("naics_4" = "detailed_naics"), suffix = c("", "_4")) %>%
    mutate(match_level = ifelse(is.na(match_level) & !is.na(clean_industry_4), "4-digit", match_level)) %>%
    
    # 3-digit match
    left_join(naics_metadata, by = c("naics_3" = "detailed_naics"), suffix = c("", "_3")) %>%
    mutate(match_level = ifelse(is.na(match_level) & !is.na(clean_industry_3), "3-digit", match_level)) %>%
    
    # Use the most specific match available for each metadata column
    mutate(
      clean_industry = coalesce(clean_industry, clean_industry_5, clean_industry_4, clean_industry_3),
      Production.Phase = coalesce(Production.Phase, Production.Phase_5, Production.Phase_4, Production.Phase_3),
      matched_naics = case_when(
        match_level == "6-digit" ~ naics_6,
        match_level == "5-digit" ~ naics_5,
        match_level == "4-digit" ~ naics_4,
        match_level == "3-digit" ~ naics_3,
        TRUE ~ NA_character_
      )
    ) %>%
    
    # Final selection of output columns
    select(
      area_fips,
      matched_naics,
      clean_industry,
      Production.Phase,
      industry_code,
      naics_desc,
      match_level,
     # lq_annual_avg_emplvl,
      annual_avg_emplvl
    #  avg_annual_pay,
    #  lq_annual_avg_wkly_wage
    )
  
  return(matched)
}
# ----- Process 2024 Data -----
county_energy <- match_naics(available_county_data, filtered_pivot) %>% distinct()


county_energylqs <- county_energy %>%
  group_by(area_fips, clean_industry, industry_code) %>%
  mutate(is_duplicate = n() > 1,
         Production.Phase = if_else(
           is_duplicate,
           case_when(
             startsWith(as.character(industry_code), "2") ~ "Operations",
             startsWith(as.character(industry_code), "3") ~ "Manufacturing",
             TRUE ~ Production.Phase
           ),
           Production.Phase
         )) %>%
  ungroup() %>%
  select(-is_duplicate) %>%
  distinct()


# Helper: Remove Nested NAICS Codes
get_unique_naics <- function(codes) {
  codes <- unique(codes[order(nchar(codes))])
  unique_codes <- c()
  for (code in codes) {
    if (!any(sapply(unique_codes, function(x) str_starts(code, x) && code != x)))
      unique_codes <- c(unique_codes, code)
  }
  unique_codes
}

county_subsectors <- county_energylqs %>%
  mutate(matched_naics = as.character(matched_naics)) %>%
  filter(!is.na(matched_naics)) %>%
  group_by(area_fips, clean_industry, Production.Phase) %>%
  summarise(unique_naics = list(get_unique_naics(matched_naics)), .groups = 'drop') %>%
  unnest(unique_naics)

##calculate totals for county and US employment
county_qcew_tot <- county_data_2024 %>%
  filter(own_code == 0) %>%
  group_by(area_fips) %>%
  summarise(county_qcew_tot = sum(annual_avg_emplvl, na.rm = TRUE), .groups = "drop")
US_qcew_tot    <- USdata     %>% filter(own_code == 0) %>% pull(annual_avg_emplvl)

##finished products *to be a column in complete industry prioritization
county_ind_emp <- county_subsectors %>% 
  left_join(county_data_2024, by = c("area_fips", "unique_naics" = "industry_code")) %>%
  filter(!is.na(unique_naics), own_code == 5) %>%  
  group_by(area_fips, clean_industry, Production.Phase) %>%
  summarise(county_ind_emp = sum(annual_avg_emplvl, na.rm = TRUE), .groups = 'drop') %>%
  left_join(county_qcew_tot, by = "area_fips") %>%
  mutate(county_emp_perc = county_ind_emp / county_qcew_tot*100)

US_ind_emp <- county_subsectors %>% 
  group_by(clean_industry, Production.Phase) %>%
  left_join(USdata, by = c("unique_naics" = "industry_code")) %>%
  filter(!is.na(unique_naics), own_code == 5) %>%
  group_by(clean_industry, Production.Phase) %>%
  summarise(US_ind_emp = sum(annual_avg_emplvl, na.rm = TRUE), .groups = 'drop') %>%
  mutate(US_emp_perc = US_ind_emp / US_qcew_tot*100)

ind_emp_combined <- left_join(county_ind_emp, US_ind_emp, by = c("clean_industry", "Production.Phase")) %>%
  mutate(consolidated_ind_lq = county_emp_perc / US_emp_perc)

# ----- Process 2019 Data -----
county_energy19 <- match_naics(available_county_data15, filtered_pivot) #%>%

county_energylqs19 <- county_energy19 %>%
  group_by(area_fips, clean_industry, industry_code) %>%
  mutate(is_duplicate = n() > 1,
         Production.Phase = if_else(
           is_duplicate,
           case_when(
             startsWith(as.character(industry_code), "2") ~ "Operations",
             startsWith(as.character(industry_code), "3") ~ "Manufacturing",
             TRUE ~ Production.Phase
           ),
           Production.Phase
         )) %>%
  ungroup() %>%
  select(-is_duplicate) %>%
  distinct()

county_subsectors19 <- county_energylqs19 %>%
  mutate(matched_naics = as.character(matched_naics)) %>%
  filter(!is.na(matched_naics)) %>%
  group_by(area_fips, clean_industry, Production.Phase) %>%
  summarise(unique_naics = list(get_unique_naics(matched_naics)), .groups = 'drop') %>%
  unnest(unique_naics)

##get county and US total employment in 2019
county_qcew_tot19 <- county_data_2019 %>%
  filter(own_code == 0) %>%
  group_by(area_fips) %>%
  summarise(county_qcew_tot = sum(annual_avg_emplvl, na.rm = TRUE), .groups = "drop")

US_qcew_tot19    <- USdata19   %>% filter(own_code == 0) %>% pull(annual_avg_emplvl)


## ind emp
county_ind_emp19 <- county_subsectors19 %>% 
  left_join(county_data_2019, by = c("area_fips", "unique_naics" = "industry_code")) %>%
  filter(!is.na(unique_naics), own_code == 5) %>%  
  group_by(area_fips, clean_industry, Production.Phase) %>%
  summarise(county_ind_emp19 = sum(annual_avg_emplvl, na.rm = TRUE), .groups = 'drop') %>%
  left_join(county_qcew_tot19, by = "area_fips") %>%
  mutate(county_emp_perc19 = county_ind_emp19 / county_qcew_tot*100)


US_ind_emp19 <- county_subsectors19 %>%
  group_by(clean_industry, Production.Phase) %>%
  left_join(USdata, by = c("unique_naics" = "industry_code")) %>%
  filter(!is.na(unique_naics), own_code == 5) %>%
  group_by(clean_industry, Production.Phase) %>%
  summarise(US_ind_emp19 = sum(annual_avg_emplvl, na.rm = TRUE), .groups = 'drop') %>%
  mutate(US_emp_perc19 = US_ind_emp19 / US_qcew_tot19*100)

ind_emp_combined19 <- left_join(county_ind_emp19, US_ind_emp19, by = c("clean_industry", "Production.Phase")) %>%
  mutate(consolidated_ind_lq_2019 = county_emp_perc19 / US_emp_perc19) %>%
  select(area_fips, clean_industry, Production.Phase, consolidated_ind_lq_2019, county_ind_emp19) %>%
  left_join(ind_emp_combined, by = c("area_fips","clean_industry", "Production.Phase")) %>%
  mutate(employment_change = (county_ind_emp - county_ind_emp19) / county_ind_emp19,
         lq_change         = consolidated_ind_lq - consolidated_ind_lq_2019) %>%
  filter(clean_industry != "Wave Energy") %>%
  select(area_fips, clean_industry, Production.Phase, county_ind_emp, county_ind_emp19, employment_change,
         consolidated_ind_lq, consolidated_ind_lq_2019, lq_change)

# ----- Final Output -----
# ind_emp_combined and ind_emp_combined19 now contain the calculated measures for 2024,
# and the changes from 2019 to 2023, respectively.



#Feasibility------------------------

feas<-read.csv("C:/Users/allie.jobe/Downloads/cgt_county_data_08_29_2024.csv")

feas_county <- feas %>%
  filter(county %in% region, aggregation_level == 4) %>%
  rename(
    area_fips = county)  %>%
  
  ## attach clean-industry / Production.Phase
  inner_join(
    clean_industry_naics %>%
      mutate(naics_6 = as.numeric(X6.Digit.Code)),
    by = c("industry_code" = "naics_6")
  ) %>%
  
  ## bring in employment (may be entirely missing)
  left_join(
    county_energy %>%
     mutate(industry_code = as.numeric(industry_code)),
    by = c("area_fips","industry_code","clean_industry", "Production.Phase")) %>%
  
  ## choose the non-missing county, give fallback weight = 1
  mutate(
    weight    = coalesce(annual_avg_emplvl, 1)
  ) %>%         # house-keeping
  
  ## county-level summary
  group_by(area_fips, clean_industry, Production.Phase) %>%
  summarise(
    across(c(annual_avg_emplvl,
             density_county_perc, density, pci),
           ~ weighted.mean(.x, w = weight, na.rm = TRUE)),
    .groups = "drop"
  )

#Clean investment Monitor Data---------------------------
investment_data_path <- "C:/Users/allie.jobe/RMI/US Program - Documents/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/clean_investment_monitor_q1_2025/quarterly_actual_investment.csv"
facilities_data_path <- "C:/Users/allie.jobe/RMI/US Program - Documents/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/clean_investment_monitor_q1_2025/manufacturing_energy_and_industry_facility_metadata.csv"
socioeconomics_data_path <- "C:/Users/allie.jobe/RMI/US Program - Documents/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/clean_investment_monitor_q1_2025/socioeconomics.csv"


##county-level CIM analysis
#### add county fips column for facilities data
facilities_sf <- facilities %>%
  filter(LatLon_Valid == 'True') %>%  # Only keep valid points
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 

counties <- counties(cb = TRUE, year = 2020, class = "sf") %>%
  st_transform(crs = st_crs(facilities_sf)) %>%  # match CRS
  select(STATEFP, COUNTYFP, GEOID, NAME, geometry)

facilities_with_county <- st_join(facilities_sf, counties, join = st_within)

facilities_with_county <- facilities_with_county %>%
  mutate(county_fips = GEOID) %>%
  st_drop_geometry()  # Drop spatial geometry if you want just a data frame

facilities_eco <-facilities_with_county %>%
  left_join(CIM_eco_eti,by=c("Technology","Segment")) %>%
  mutate(clean_industry=ifelse(Technology=="Other" & Subcategory=="Geothermal","Geothermal",clean_industry),
         Production.Phase=ifelse(clean_industry=="Geothermal","Operations",Production.Phase)) %>%
  filter(county_fips %in% region,
         clean_industry!= "",
         Current_Facility_Status %in% c("Under Construction",
                                        "Announced")) %>%
  group_by(county_fips,clean_industry,Production.Phase) %>%
  summarize_at(vars(Estimated_Total_Facility_CAPEX),sum,na.rm=T)

facilities_eco<- facilities_eco %>%
  mutate(area_fips = as.numeric(county_fips))
##workaround-- since investment doesn't have county-level data, filter facilities for "operating" status
investment_eco <- facilities_with_county %>%
  left_join(CIM_eco_eti, by = c("Technology","Segment"))%>%
  mutate(clean_industry=ifelse(Technology=="Other" & Subcategory=="Geothermal","Geothermal",clean_industry),
         Production.Phase=ifelse(clean_industry=="Geothermal","Operations",Production.Phase)) %>%
  filter(county_fips %in% region,
         clean_industry!= "",
         Current_Facility_Status %in% c("Operating")) %>%
  group_by(county_fips,clean_industry,Production.Phase) %>%
  summarize_at(vars(Estimated_Total_Facility_CAPEX),sum,na.rm=T)

investment_eco<- investment_eco %>%
  mutate(area_fips = as.numeric(county_fips))%>%
  rename(Operating_estimated_total_facility_capex=Estimated_Total_Facility_CAPEX)

#USEER Employment Data ---------------------------------

file_path <- file.path(acre_data, "USEER 2024 County Estimates FINAL less than 10 1.2.xlsx")

county_useer <- read_excel(file_path,
                           skip       = 1,
                           .name_repair = "unique") %>%  # makes every header unique
  clean_names() %>%                                      # spaces → _, lower-case
  # natural_gas...9 → natural_gas_9
  pivot_longer(
    cols         = -(state:county_name),       # every energy column
    names_to     = "category_raw",
    values_to    = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    category = str_remove(category_raw, "_\\d+$")        # natural_gas_9 → natural_gas
  ) %>%
  group_by(across(-c(value, category_raw)), category) %>% # state, fips, county, …
  summarise(
    value = sum(as.numeric(value), na.rm = TRUE),        # combine dupes; <10 → NA
    .groups = "drop"
  ) %>% rename(area_fips = county_fips)

useer_eco <- read.csv(paste0(raw_data,"/state_useer_cat.csv"))
useer_eco<-useer_eco %>%
  filter(clean_industry != "")

## help get all category into the same format by applying this function to both data-sets, so we can later merge them
canon <- function(x) {
  x %>%
    str_trim() %>%                     # remove outer blanks
    str_to_lower() %>%                 # case-insensitive
    str_replace_all("[^a-z0-9]", "")   # drop spaces & punctuation
}

county_useer <- county_useer %>%              
  mutate(cat_key = canon(category))


useer_eco <- useer_eco %>%
  mutate(cat_key = canon(category))

county_useer_eco<-county_useer %>%
  inner_join(useer_eco,by="cat_key") %>%
  filter(area_fips %in% region) %>%
  group_by(area_fips, clean_industry,Production.Phase) %>%
  summarize_at(vars(value),sum,na.rm=T) %>%
  arrange(desc(value))%>%
  rename(USEER_emp = value)


#Energy Potential - NREL Supply Curve Data------------------------------
#load data, which comes from EIA supply curve estimates for 2030 moderate scenario
supply_curves_long<-read.csv(file.path(raw_data,"supply_curves_county.csv"))%>%
  mutate(
    clean_industry = recode(clean_industry,
                            Wind = "Wind Energy"))

#Manufacturing Potential - Employment & Cost Data-------------------------
# Initialize an empty list to store results
county_data_list <- list()

# Loop over each FIPS code
for (fips in region) {
  county_data <- blsQCEW('Area', year = '2024', quarter = '3', area = fips)
  county_data_list[[fips]] <- county_data
}

# Combine all into a single data frame
county_data_all_man <- do.call(rbind, county_data_list)



#Federal Policy Support---------------------------------
federal_support <- clean_industry_naics %>%
  distinct(clean_industry,Production.Phase) %>%
  mutate(fed_support = ifelse(clean_industry == "Solar" & 
                                Production.Phase == "Operations",1,0),
         fed_support = ifelse(clean_industry == "Wind" & 
                                Production.Phase == "Operations",1,fed_support),
         fed_support = ifelse(clean_industry == "Solar" & 
                                Production.Phase == "Manufacturing",1,fed_support),
         fed_support = ifelse(clean_industry == "Wind" & 
                                Production.Phase == "Manufacturing",1,fed_support),
         fed_support = ifelse(clean_industry == "Batteries" & 
                                Production.Phase == "Manufacturing",1,fed_support),
         fed_support = ifelse(clean_industry == "Inverters" & 
                                Production.Phase == "Manufacturing",1,fed_support),
         fed_support = ifelse(clean_industry == "Green Hydrogen" & 
                                Production.Phase == "Operations",1,fed_support),
         fed_support = ifelse(clean_industry == "Nuclear" & 
                                Production.Phase == "Operations",1,fed_support),
         fed_support = ifelse(clean_industry == "Critical Minerals" & 
                                Production.Phase == "Manufacturing",1,fed_support),
         fed_support = ifelse(clean_industry == "Geothermal" & 
                                Production.Phase == "Operations",1,fed_support),
         fed_support = ifelse(clean_industry == "Biofuels" & 
                                Production.Phase == "Manufacturing",1,fed_support),
         fed_support = ifelse(clean_industry == "Energy Storage" & 
                                Production.Phase == "Operations",1,fed_support)
  )

#State Policy Support------------------------------------
xchange_state<-read.csv(paste0(raw_data,"xchange.csv")) %>% select(-X) %>%
  filter(grepl("index",Policy),
         abbr==state_abbr) 

state_support <- clean_industry_naics %>%
  distinct(clean_industry,Production.Phase) %>%
  mutate(state_support = ifelse(clean_industry == "Solar" & 
                                  Production.Phase == "Operations",xchange_state$value[xchange_state$Topic == "Electricity"],""),
         state_support = ifelse(clean_industry == "Wind" & 
                                  Production.Phase == "Operations",xchange_state$value[xchange_state$Topic == "Electricity"],state_support),
         state_support = ifelse(clean_industry == "Green Hydrogen" & 
                                  Production.Phase == "Operations",xchange_state$value[xchange_state$Topic == "Industry"],state_support),
         state_support = ifelse(clean_industry == "Nuclear" & 
                                  Production.Phase == "Operations",xchange_state$value[xchange_state$Topic == "Electricity"],state_support),
         state_support = ifelse(clean_industry == "Geothermal" & 
                                  Production.Phase == "Operations",xchange_state$value[xchange_state$Topic == "Electricity"],state_support),
         state_support = ifelse(clean_industry == "Biofuels" & 
                                  Production.Phase == "Manufacturing",xchange_state$value[xchange_state$Topic == "Industry"],state_support),
         state_support = ifelse(clean_industry == "Energy Transition Metals" & 
                                  Production.Phase == "Manufacturing",xchange_state$value[xchange_state$Topic == "Industry"],state_support),
         state_support = ifelse(clean_industry == "Energy Storage" & 
                                  Production.Phase == "Operations",xchange_state$value[xchange_state$Topic == "Electricity"],state_support),
         state_support = ifelse(clean_industry == "Transmission & Distribution" & 
                                  Production.Phase == "Construction",xchange_state$value[xchange_state$Topic == "Electricity"],state_support),
         state_support = ifelse(clean_industry == "Energy Efficient Heating/Cooling",xchange_state$value[xchange_state$Topic == "Buildings"],state_support),
         state_support = ifelse(clean_industry == "Energy Efficient Lighting",xchange_state$value[xchange_state$Topic == "Buildings"],state_support),
         state_support = ifelse(clean_industry == "Energy Efficient Appliances",xchange_state$value[xchange_state$Topic == "Buildings"],state_support))


#Technology Readiness from IEA -----------------------------------
iea_cleantech_sector<-read.csv(paste0(raw_data,"iea_cleantech_sector.csv"))

#National Investment from CIM----------------------

investment_national<-investment %>%
  left_join(CIM_eco_eti,by=c("Technology","Segment")) %>%
  mutate(clean_industry=ifelse(Technology=="Other" & Subcategory=="Geothermal","Geothermal",clean_industry),
         Production.Phase=ifelse(clean_industry=="Geothermal","Operations",Production.Phase)) %>%
  filter(clean_industry!= "") %>%
  mutate(year=as.numeric(substr(quarter,1, 4))) %>%
  group_by(clean_industry,Production.Phase,year) %>%
  summarize(value=sum(Estimated_Actual_Quarterly_Expenditure,na.rm=T),
            .groups='drop') %>%
  pivot_wider(names_from="year",values_from='value') %>%
  mutate(growth_2224=(`2024`-`2022`)/`2022`*100) %>%
  rename("inv_24"="2024") %>%
  select(clean_industry,Production.Phase,growth_2224,inv_24)

#Innovation Funding from ITIF-------------------------------------
#county level does not exist here
url <- 'https://cdn.sanity.io/files/03hnmfyj/production/77717b609392dedba6f8ba316ce16d6629bf6666.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = url, write_disk(temp_file, overwrite = TRUE))
innov_state <- read_excel(temp_file, sheet = 3)  # 'sheet = 1' to read the first sheet
innov_metro <- read_excel(temp_file, sheet = 4)  # 'sheet = 1' to read the first sheet
innov_vars<- read_excel(temp_file, sheet = 1,skip=58)  # 'sheet = 1' to read the first sheet
key_vars <-innov_vars %>% filter(Subindex %in% c("Knowledge Development and Diffusion (KDD)","Entrepreneurial Experimentation (EE)"))
sectors <- c("bioenergy", "ccus", "efficiency", "geothermal", 
             "grid", "hydrogen", "mfg", "nuclear", "solar", 
             "storage", "transport", "water", "wind","all")

# Build a regular expression that matches columns ending in one of the sectors.
regex <- paste0("^(.*)_(", paste(sectors, collapse = "|"), ")$")

# Pivot the data longer: keep 'year' and 'statecode' and split the other columns
innov_state_long <- innov_state %>%
  pivot_longer(
    cols = matches(regex),
    names_to = c("measure", "sector"),
    names_pattern = regex
  ) %>%
  distinct(year,statecode,measure,sector,value) %>%
  inner_join(innov_vars %>% 
               filter(Subindex %in% c("Knowledge Development and Diffusion (KDD)","Entrepreneurial Experimentation (EE)")) %>%
               select(`Variable Name`,Subindex),by=c("measure"="Variable Name"))%>% 
  group_by(statecode,measure,Subindex,sector) %>%
  summarize_at(vars(value),sum,na.rm=T) %>%
  group_by(measure,Subindex,sector) %>%
  mutate(rank=rank(value))

# --- US-level normalization ---
# Step 1.1: Compute the US-wide baseline (the "all" sector) for each measure & Subindex.
us_denom <- innov_state_long %>%
  filter(sector != "all") %>%
  group_by(measure, Subindex) %>%
  summarize(us_denom = sum(value, na.rm = TRUE), .groups = "drop")

# Step 1.2: For each (measure, sector, Subindex), sum values across states and compute the US norm.
us_norm <- innov_state_long %>%
  group_by(measure, sector, Subindex) %>%
  summarize(us_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(us_denom, by = c("measure", "Subindex")) %>%
  mutate(us_norm = us_value / us_denom) %>%
  select(measure, sector, Subindex, us_norm)

# --- State-level normalization ---
# Step 2.1: Compute the state-level baseline (for sector == "all") for each state, measure, Subindex.
state_denom <- innov_state_long %>%
  filter(sector != "all") %>%
  group_by(statecode, measure, Subindex) %>%
  summarize(state_denom = sum(value, na.rm = TRUE), .groups = "drop")

# Step 2.2: Compute the state-level value for each (state, measure, sector, Subindex)
# and calculate the state normalized value.
state_norm <- innov_state_long %>%
  group_by(statecode, measure, sector, Subindex) %>%
  summarize(state_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(state_denom, by = c("statecode", "measure", "Subindex")) %>%
  mutate(state_norm = state_value / state_denom) 

# --- Combine and create the state index ---
# Step 3: For each (state, measure, sector, Subindex), merge the state and US norms and compute the ratio.
combined <- state_norm %>%
  left_join(us_norm, by = c("measure", "sector", "Subindex")) %>%
  mutate(ratio = state_norm / us_norm) %>%
  group_by(measure,sector,Subindex) %>%
  mutate(min_val = min(ratio, na.rm = TRUE),
         max_val = max(ratio, na.rm = TRUE),
         norm_0_1 = if_else(max_val == min_val, 0, (ratio - min_val) / (max_val - min_val))) 

# Step 4: Now average the ratio within each state and Subindex.
state_index <- combined %>%
  group_by(statecode, Subindex, sector) %>%
  summarize(index = mean(ratio, na.rm = TRUE), .groups = "drop")

rdd_state_index <- state_index %>%
  filter(Subindex == "Knowledge Development and Diffusion (KDD)",
         sector != "all") %>%
  mutate(clean_industry=ifelse(sector=="bioenergy","Biofuels",""),
         clean_industry=ifelse(sector=="ccus","Carbon Capture",clean_industry),
         clean_industry=ifelse(sector=="geothermal","Geothermal",clean_industry),
         clean_industry=ifelse(sector=="grid","Transmission & Distribution",clean_industry),
         clean_industry=ifelse(sector=="efficiency","Energy Efficient Appliances|Energy Efficient Heating/Cooling|Energy Efficient Lighting",clean_industry),
         clean_industry=ifelse(sector=="hydrogen","Green Hydrogen",clean_industry),
         clean_industry=ifelse(sector=="nuclear","Nuclear",clean_industry),
         clean_industry=ifelse(sector=="solar","Solar",clean_industry),
         clean_industry=ifelse(sector=="storage","Energy Storage|Batteries",clean_industry),
         clean_industry=ifelse(sector=="transport","Electric Vehicles",clean_industry),
         clean_industry=ifelse(sector=="wind","Wind Energy",clean_industry),
         clean_industry=ifelse(sector=="water","Water Purification",clean_industry)) %>%
  separate_rows(clean_industry, sep = "\\|")

state_sector_norm <- rdd_state_index %>%
  group_by(Subindex, clean_industry) %>%
  mutate(min_val = min(index, na.rm = TRUE),
         max_val = max(index, na.rm = TRUE),
         norm_0_1 = if_else(max_val == min_val, 0, (index - min_val) / (max_val - min_val))) %>%
  ungroup()


state_sector_norm <- state_sector_norm %>% 
  select(statecode,sector,norm_0_1) %>%
  rename("RDD_specialization"="norm_0_1") 

innov_state_sum <- innov_state_long %>%
  filter(Subindex=="Knowledge Development and Diffusion (KDD)",
         sector != "all") %>%
  group_by(sector,measure) %>%
  mutate(min_val = min(value, na.rm = TRUE),
         max_val = max(value, na.rm = TRUE),
         norm_0_1 = if_else(max_val == min_val, 0, (value - min_val) / (max_val - min_val))) %>%
  group_by(statecode,sector) %>%
  summarize_at(vars(norm_0_1),sum,na.rm=T) %>%
  rename("RDD_total"="norm_0_1") %>%
  left_join(state_sector_norm,by=c("statecode","sector")) %>%
  mutate(clean_industry=ifelse(sector=="bioenergy","Biofuels",""),
         clean_industry=ifelse(sector=="ccus","Carbon Capture",clean_industry),
         clean_industry=ifelse(sector=="geothermal","Geothermal",clean_industry),
         clean_industry=ifelse(sector=="grid","Transmission & Distribution",clean_industry),
         clean_industry=ifelse(sector=="efficiency","Energy Efficient Appliances|Energy Efficient Heating/Cooling|Energy Efficient Lighting",clean_industry),
         clean_industry=ifelse(sector=="hydrogen","Green Hydrogen",clean_industry),
         clean_industry=ifelse(sector=="nuclear","Nuclear",clean_industry),
         clean_industry=ifelse(sector=="solar","Solar",clean_industry),
         clean_industry=ifelse(sector=="storage","Energy Storage|Batteries",clean_industry),
         clean_industry=ifelse(sector=="transport","Electric Vehicles",clean_industry),
         clean_industry=ifelse(sector=="wind","Wind Energy",clean_industry),
         clean_industry=ifelse(sector=="water","Water Purification",clean_industry))%>%
  select(-sector)%>%
  separate_rows(clean_industry, sep = "\\|")

#Putting the Matrix Together------------------

# ── 1.  a lookup of valid industry-phase pairs ────────────────────────────────
# Replace `naics_lookup` with the name of the data frame that holds the long
# table you pasted (must have columns `clean_industry` and `Production.Phase`).

valid_pairs <- clean_industry_naics %>%                      # <-- your big table
  distinct(clean_industry, Production.Phase)         # just the unique combos

# ── 2.  New-Mexico FIPS codes as a tibble ─────────────────────────────────────
nm_fips <- tibble(
  area_fips = c(
    "35001","35003","35005","35006","35007","35009","35011","35013",
    "35015","35017","35019","35021","35023","35025","35027","35028",
    "35029","35031","35033","35035","35037","35039","35041","35043",
    "35045","35047","35049","35051","35053","35055","35057","35059", "35061"
  )
) %>% 
  mutate(area_fips = as.numeric(area_fips))           # make it numeric if needed

# ── 3.  Cartesian join (FIPS × valid pairs) ───────────────────────────────────
all_combinations <- tidyr::crossing(nm_fips, valid_pairs)
# columns: area_fips, clean_industry, Production.Phase

# ── 4.  (optional) inspect or save ────────────────────────────────────────────
View(all_combinations)


all_combinations<-all_combinations %>%
  left_join(supply_curves_long%>% 
              filter(area_fips %in% region) %>% 
              select(-State,-County,)%>%
              mutate(lcoe_inv = 1 - lcoe_percentile), #inverts lcoe percentile so cheaper lcoe is closer to 1, for use in calculating index
            by=c("area_fips","clean_industry","Production.Phase"))%>%
  left_join(ind_emp_combined19, by=c("area_fips","clean_industry","Production.Phase")) %>%
  left_join(feas_county, by=c("area_fips","clean_industry","Production.Phase"))%>%
  left_join(investment_eco %>% ungroup() %>% select(-county_fips), by=c("area_fips","clean_industry","Production.Phase"))%>%
  left_join(facilities_eco %>% ungroup() %>% select(-county_fips), by=c("area_fips","clean_industry","Production.Phase")) %>%
  left_join(iea_cleantech_sector %>%
              select(-X),by=c("clean_industry"="rmi_sector")) %>% 
  left_join(state_support,by=c("clean_industry","Production.Phase")) %>%
  left_join(innov_state_sum %>%
              filter(statecode==state_abbr) %>% ungroup()%>%
              select(-statecode),by="clean_industry") %>%
  left_join(county_useer_eco,by=c("area_fips", "clean_industry","Production.Phase"))%>%
  left_join(federal_support,by=c("clean_industry","Production.Phase")) 
  
  


########### The weighting needs to be fixed I believe still #############
#Normalized Matrix & Index----------------------------------------- 
normalized_ind_matrix <- NULL
already_01 <- c("density_county_perc",
                  "capacity_percentile",
                  "lcoe_inv") ## a vector of columns that are already normalized 0 to 1 to an external national scale,
                                      ##to avoid re-normalizing in the df

normalized_ind_matrix <- all_combinations %>%
  mutate(area_fips = as.character(area_fips))%>%
  ungroup() %>%
  select(-county_ind_emp19, -consolidated_ind_lq_2019, -total_lcoe, -lcoe_percentile, - capacity_mw, -annual_avg_emplvl, -density) %>%
  
  # ── 1.  replace Inf / NA on *all* numeric columns ──────────────────────────
  mutate(across(
    where(is.numeric),
    ~ case_when(
      is.infinite(.) ~ max(.[!is.infinite(.)], na.rm = TRUE),
      is.na(.)       ~ min(.[!is.infinite(.)], na.rm = TRUE),
      TRUE           ~ .
    )
  )) %>%
  
  # ── 2.  first 0-1 rescale, skipping the three pre-normalized cols ─────────
  mutate(across(
    where(is.numeric) & !any_of(already_01),
    ~ (. - min(., na.rm = TRUE)) /
      (max(., na.rm = TRUE) - min(., na.rm = TRUE))
  )) %>%
  
  # ── 3.  row-wise composite index (density_county_perc gets 4× weight) ──────
  rowwise() %>%                       # treat each row independently
  mutate(
    index = {
      # pull every numeric value *except* area_fips
      row_vals <- c_across(where(is.numeric) & !any_of("area_fips"))
      
      if ("density_county_perc" %in% names(row_vals) &&
          row_vals[["density_county_perc"]] != 0) {
        
        # repeat density_county_perc 4×, then average
        other_vals <- row_vals[names(row_vals) != "density_county_perc"]
        mean(c(other_vals, rep(row_vals[["density_county_perc"]], 4)), na.rm = TRUE)
        
      } else {
        mean(row_vals, na.rm = TRUE)
      }
    }
  ) %>% 
  ungroup() %>%
  
  # ── 4.  final 0-1 rescale so the new index is on the same scale ───────────
  mutate(across(
    where(is.numeric) & !any_of(already_01),
    ~ (. - min(., na.rm = TRUE)) /
      (max(., na.rm = TRUE) - min(., na.rm = TRUE))
  ))
 

#Final Matrix----------------------------
ind_matrix_final <- left_join(all_combinations %>% 
                                mutate(area_fips = as.character(area_fips)), normalized_ind_matrix %>%
                                select(area_fips,clean_industry,Production.Phase,index),by=c("area_fips", "clean_industry","Production.Phase")) %>%
  ungroup() %>%
  arrange(desc(index))

#add county names-----
  county_lookup <- counties(cb = TRUE, year = 2020) %>%
    transmute(
      area_fips = as.character(GEOID),  # or keep as character if leading zeroes matter
      state_name = NAME,              # or use STUSPS for abbreviation
      county_name = NAMELSAD
    )
  
ind_matrix_final<-ind_matrix_final %>%
  left_join(county_lookup %>% mutate(area_fips = as.character(area_fips)), by = "area_fips")%>%
  relocate(county_name, .after = 1) %>% # Moves county_name to 2nd column
  select(-state_name,-geometry)


## Make sure county_name and area_fips are character (for filenames)
ind_matrix_final <- ind_matrix_final %>%
  mutate(
    area_fips = as.character(area_fips),
    county_name_clean = gsub("[^A-Za-z0-9]", "_", county_name)  # remove special chars
  )


### clean for datawrapper processing
ind_matrix_dw <- ind_matrix_final %>%
  select(area_fips,county_name,county_name_clean, 
         clean_industry, Production.Phase,index, density, consolidated_ind_lq,
         county_ind_emp, Operating_estimated_total_facility_capex,
         Estimated_Total_Facility_CAPEX, capacity_percentile,
         lcoe_percentile, trl2023, pci) %>%
  arrange(across(everything())) %>%
  rename(`Prioritization Index` = index,
         Industry = clean_industry,
         Phase = Production.Phase,
         Feasibility = density,
         `Location Quotient` = consolidated_ind_lq,
         `2024 Employment` = county_ind_emp,
         `2022-2025 (Q1) Investment ($ millions)` = Operating_estimated_total_facility_capex,
         `Announced & Under Construction Investment ($ millions)` = Estimated_Total_Facility_CAPEX,
         `Resource Potential (county percentile)` = capacity_percentile,
         `Resource Cost (county percentile)` = lcoe_percentile,
         `Technology Readiness` = trl2023,
         `Complexity` = pci) %>%
  distinct() %>%
  arrange(desc(`Prioritization Index`))
  
# Split by county
county_list <- group_split(ind_matrix_dw, county_name_clean, keep = TRUE)

# Get list of names to use for files
county_names <- group_keys(ind_matrix_dw, county_name_clean)$county_name_clean

dir.create("C:/Users/allie.jobe/Documents/county_csvs", showWarnings = FALSE, recursive = TRUE)


# Write each county to separate CSV
walk2(
  .x = county_list,
  .y = county_names,
  .f = ~ write_csv(.x, file = paste0("Documents/county_csvs/", .y, ".csv"))
)


# ──────────────────────────────────────────────────────────────────────────────
#  Datawrapper automation – New Mexico county-level tables
#  * Copies the Bernalillo-County template (`BzO9a`)
#  * Uploads the full 12-column dataset for EACH CSV in "county_csvs/"
#  * Publishes the chart and logs county → public URL
# ──────────────────────────────────────────────────────────────────────────────

# 0. INSTALL & LOAD PACKAGES ---------------------------------------------------
# (Run the install line once; comment it out afterwards.)
#install.packages("DatawRappr", type = "binary")   # no Rtools needed
#packageVersion("DatawRappr")      # should print ≥ 1.2
library(purrr)  
library(DatawRappr)
library(dplyr)
library(readr)
library(stringr)
# 1. AUTHENTICATION -----------------------------------------------------------
datawrapper_auth(api_key = "OeShPJIYxPz4CaLNYkWrjoa3riQhRMq49vG3SKm3HnS06wUltY76I9cBfhncqNru", overwrite = TRUE)         # writes to ~/.Renviron
# Then restart R and the token is picked up automatically.

dw_test_key(api_key = "OeShPJIYxPz4CaLNYkWrjoa3riQhRMq49vG3SKm3HnS06wUltY76I9cBfhncqNru")   # aborts with an error if the key is invalid

# 2. USER PARAMETERS ----------------------------------------------------------
template_id <- "BzO9a"            # Bernalillo-County chart ID
csv_dir     <- "C:/Users/allie.jobe/Documents/county_csvs"    # folder with all NM county CSVs
out_log     <- "nm_dw_urls.csv"   # file to save county → URL lookup

# 3. HELPER FUNCTION – DUPLICATE + PUBLISH -----------------------------------
build_chart <- function(path, template_id) {
  
  # a) read the CSV -----------------------------------------------------------
  df_raw <- read_csv(path, show_col_types = FALSE)
  
  # b) keep the 12 columns the template shows (order matters!) ---------------
  template_cols <- c(
    "Industry", "Phase",
    "Prioritization Index", "Feasibility", "Location Quotient",
    "2024 Employment",
    "2022-2025 (Q1) Investment ($ millions)",
    "Announced & Under Construction Investment ($ millions)",
    "Resource Potential (county percentile)",
    "Resource Cost (county percentile)",
    "Technology Readiness", "Complexity"
  )
  
  missing <- setdiff(template_cols, names(df_raw))
  if (length(missing)) df_raw[missing] <- NA
  
  df <- df_raw |>
    select(all_of(template_cols)) |>
    arrange(desc(`Prioritization Index`)) |>
    slice_head(n = 10)
  
  # c) county name from the file name ----------------------------------------
  filename <- basename(path)                           # e.g. "Santa_Fe_County.csv"
  
  county <- filename |>
    stringr::str_remove("_County\\.csv$") |>           # 1. trim suffix
    stringr::str_replace_all("_", " ") |>              # 2. underscores → spaces
    dplyr::recode(                                     # 3. recode in special cases (R doesn't love tildes)
      "Do a Ana"    = "Doña Ana") |>
    paste("County")  
  
  # d) duplicate template, upload data, tweak title, publish ------------------
  chart <- dw_copy_chart(copy_from = template_id)
  dw_data_to_chart(df, chart)
  
  dw_edit_chart(
    chart,
    title = paste("Top 10 Industries in", county),     # ← NEW headline
    metadata = list(
      visualize = list(                               # optional UI tweaks
        table = list(
          sortBy     = "Prioritization Index",
          sortByDesc = TRUE,
          paginate   = FALSE,                         # show only those 10
          rowsPerPage = 10
        )
      ),
      annotated = list(
        notes = "Source: Synthesis of BLS, Clean Investment Monitor, NREL, Lightcast, and DOE data"
      )
    )
  )
  
  pub <- dw_publish_chart(chart, return_object = TRUE)
  
  # e) return a tidy record ---------------------------------------------------
  tibble(
    county     = county,
    chart_id   = pub$id,
    public_url = pub$publicUrl
  )
}

# 4. LOOP OVER ALL CSVs & BUILD CHARTS ----------------------------------------
## 4. BUILD & PUBLISH ALL CHARTS ----------------------------------------------
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) stop("No CSVs found in ", csv_dir)

results <- purrr::map_dfr(csv_files, build_chart, template_id = template_id)

write_csv(results, out_log) #writes a csv with county name, chart id, and public URL column
print(results, n = nrow(results))
# ──────────────────────────────────────────────────────────────────────────────
#  End of script – embed `public_url` wherever you need the live charts!
# ──────────────────────────────────────────────────────────────────────────────