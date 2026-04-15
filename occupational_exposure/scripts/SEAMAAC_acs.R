# Pulling pertinent data from the ACS 5-Year (2024) for study area based on census tracts. The goal is to 1) pull data related to occupation and 2) to pull data related to housing. Shoutout if it is also available at the bg level.

# SETUP ----
options(
  tigris_use_cache = T,
  scipen = 999
)
## Install packages ----
library(pacman)
p_load(tidyverse, sf, tigris, tidycensus, data.table, DT, tmap, stringr)


# PULL DATA ----
## Create filtering vectors ----
# filter by tracts
tracts <- c("004001", "004002", "004101", "004103", "004104", "004201", "004202", "003720")

## DP03 `Selected Economic Characteristics` ----
### Pull variable labels ----

vars <- load_variables(2024, "acs5/profile", cache = TRUE)

dp03_vars <- vars %>%
  filter(grepl("^DP03", name)) %>%
  select(name, label)

### Pull DP03 and join with variable labels ----
dp03 <- get_acs(
  geography = "tract",
  state = "PA",
  year = 2024,
  survey = "acs5",
  table = "DP03",
  cache_table = T,
  geometry = T
) %>% 
  rename(
    name = NAME,
    variable = VARIABLE
    )

# rename the variable column
dp03_vars <- dp03_vars %>% 
  rename(variable = label)

dp03 <- dp03 %>% 
  left_join(dp03_vars, by = "variable")

### Coding for tract and subsetting study area ----
dp03 <- dp03 %>% 
  mutate(tract = str_sub(GEOID, -6))

study_dp03 <- dp03 %>% 
  filter(tract %in% tracts)

# filtering out estimates by selecting variables that do NOT end with a P (indicates percent)
study_dp03e <- study_dp03 %>% 
  filter(!str_detect(variable, "P$"))

# separating the label column so I can understand wtf it is talking about
study_dp03e <- study_dp03 %>% 
  separate(
    col = label,
    into = c("measure", "indicator", "metric", "detail_1", "detail_2", "detail_3", "detail_4"),
    sep = "!!",
    remove = F
      )

# looking at the different categories 
measure_sum <- study_dp03e %>% 
  group_by(measure) %>% 
  summarise(n = n())

indicator_sum <- study_dp03e %>% 
  group_by(indicator) %>% 
  summarise(n = n())

 metric_sum <- study_dp03e %>% 
   group_by(metric) %>% 
   summarise(n = n())
 


 
 ## Subsetting for industry/occupation ----
 # Filtering out data on study area industry/occupation breakdown. Removing percentages because they are at the tract level. Will compute this later on.
ind_occ <- study_dp03e %>% 
  filter(indicator == "INDUSTRY" | indicator == "OCCUPATION",
         !str_detect(variable, "P$")) %>% 
   select(
     GEOID,
     tract,
     indicator,
     metric,
     industry_occupation = detail_1,
     estimate,
     moe,
     geometry
   ) %>% 
   mutate(industry_occupation = case_when(
     is.na(industry_occupation) ~ "TOTAL STUDY AREA EMPLOYMENT",
     TRUE ~ industry_occupation
   )) 
 
 # Creating a summary of aggregate industry/occupation employment across the entire study area. 
 
   ind_sum <- ind_occ %>% 
   group_by(indicator, industry_occupation) %>% 
   summarise(study_area_count = sum(estimate)) %>% 
     mutate(
       total = study_area_count[industry_occupation == "TOTAL STUDY AREA EMPLOYMENT"][1],
       share_occupied = study_area_count / total,
       .before = geometry
     ) %>% 
     select(-total) %>% 
     group_by(indicator) %>% 
     arrange(indicator, desc(share_occupied))
 
 # ind_sum <- ind_occ %>% 
 #   filter(!str_detect(variable, "P$") &
 #          indicator == "INDUSTRY | OCCUPATION"
 #          ) %>% 
 #   select(
 #     GEOID,
 #     variable,
 #     estimate,
 #     moe,
 #     indicator,
 #     metric,
 #     industry_occupation = detail_1,
 #     tract,
 #     geometry
 #   ) %>% 
 #   group_by(indicator, industry_occupation) %>% 
 #   mutate(study_area_count = sum(estimate))

## Summarizing aggregate totals for study area ----
 ### Total labor force ----
 laborforce <- study_dp03e %>% 
   filter(metric == "Civilian labor force")
 
 agg_laborforce <- laborforce %>% 
   filter(!str_detect(variable, "P$")) %>% 
   group_by(metric, indicator, detail_1) %>% 
   summarise(
     total_count = sum(estimate)
   )
 
 ### Total by industry/occupation ----
agg_ind_occ <- ind_occ %>% 
  group_by(industry_occupation, indicator) %>% 
  summarise(
    total_count = sum(estimate)
  )

 
 employed <- 16527 
 laborforce <- 17730
 
## Read in NAICS 2-digit codes ----
# naics <- readxl::read_xlsx("/Users/sydneyjones/Library/CloudStorage/Box-Box/kauffman_penn/documents/NAICS/2022_NAICS_2_Digit.xlsx") 

# ## Change all fields to uppercase ----
#  naics <- naics %>% 
#    mutate(description_2_digit = toupper(primary_naics_2_description)) %>% 
#    rename(code_2_digit = primary_naics_2_digit)
#  
#  agg_ind_occ <- agg_ind_occ %>% 
#    mutate(
#     indicator = toupper(indicator),
#     metric = toupper(metric),
#     detail_1 = toupper(detail_1) 
#     ) %>% 
#    rename(description_2_digit = detail_1)
 
 
# ind_occ <- ind_occ %>% 
#   mutate(
#     indicator = toupper(indicator),
#     metric = toupper(metric),
#     detail_1 = toupper(detail_1) 
#   ) %>% 
#   rename(description_2_digit = detail_1)
 
 # importing exposure means from excel ----
 heat <- readxl::read_xlsx("/Users/sydneyjones/Library/CloudStorage/OneDrive-PennO365/DES-HCED-SEAMAAC - Documents/Our Individual Folders/Sydney/occupational_exposure/acs_grouping_exposure.xlsx")

# Removing punctuation and converting to uppercase for easy joining
ind_occ <- ind_occ  %>% 
  mutate(
    industry_occupation = toupper(industry_occupation)
  ) %>% 
  mutate( industry_occupation = str_remove_all(industry_occupation, "[[:punct:]]")
  ) %>%  
  mutate(industry_occupation = case_when(
      industry_occupation == "ARTS ENTERTAINMENT AND RECREATION AND ACCOMMODATION AND FOOD SERVICES" ~ "ARTS ENTERTAINMENT AND RECREATION AND ACCOMMODATION AND FOOD SERVICES AND RETAIL TRADE",
    TRUE ~ industry_occupation))

agg_ind_occ <- agg_ind_occ %>% 
  mutate(
    industry_occupation = toupper(industry_occupation)
  ) %>% 
  mutate( industry_occupation = str_remove_all(industry_occupation, "[[:punct:]]")
  ) %>% 
  mutate(
    industry_occupation = case_when(
      industry_occupation == "ARTS ENTERTAINMENT AND RECREATION AND ACCOMMODATION AND FOOD SERVICES" ~ "ARTS ENTERTAINMENT AND RECREATION AND ACCOMMODATION AND FOOD SERVICES AND RETAIL TRADE",
      TRUE ~ industry_occupation
  ))

heat <- heat %>% 
  mutate(
    industry_occupation = toupper(industry_occupation)
  ) %>% 
  mutate(
    industry_occupation = str_remove_all(industry_occupation, "[[:punct:]]")
  )
   
 
#Joining by ACS description (so annoying they don't have codes)
ind_occ <- ind_occ %>% 
   left_join(heat, by = "industry_occupation")
 
agg_ind_occ <- agg_ind_occ %>% 
  left_join(heat, by = "industry_occupation")

# Table of findings ----
## Adding a scaled variable ----
# calculating agg share of workers in different exposure categories
agg_ind_occ <- agg_ind_occ %>% 
  mutate(indoors_uncontrolled_severity = case_when(
    indoors_controlled >= 75 ~ "Indoors with Uncontrolled Temperatures Once a Week or More but Not Every Day",
    indoors_controlled > 50 & indoors_controlled <= 74.99 ~ "Indoors with Uncontrolled Temperatures Once a Month or More but Not Every Week",
    indoors_controlled > 25 & indoors_controlled <= 49.99 ~ "Indoors with Uncontrolled Temperatures Once a Year or More but Not Every Month",
    indoors_controlled <= 25 ~ "Never Indoors with Uncontrolled Temperatures",
    TRUE ~ NA_character_
  )) %>% 
  mutate(outdoors_exposed_severity = case_when(
    outdoors_exposed >= 75 ~ "Outdoors and Exposed to Temperatures Once a Week or More but Not Every Day",
    outdoors_exposed > 50 & outdoors_exposed <= 74.99 ~ "Outdoors and Exposed to Temperatures Once a Month or More but Not Every Week",
    outdoors_exposed > 25 & outdoors_exposed <= 49.99 ~ "Outdoors and Exposed to Temperatures Once a Year or More but Not Every Month",
    outdoors_exposed <= 25 ~ "Never Outdoors and Exposed to Temperatures",
    TRUE ~ NA_character_
  )) %>% 
  mutate(outdoors_undercover_severity = case_when(
    outdoors_undercover >= 75 ~ "Outdoors and Undercover Once a Week or More but Not Every Day",
    outdoors_undercover > 50 & outdoors_undercover <= 74.99 ~ "Outdoors and Undercover Once a Month or More but Not Every Week",
    outdoors_undercover > 25 & outdoors_undercover <= 49.99 ~ "Outdoors and Undercover Once a Year or More but Not Every Month",
    outdoors_undercover <= 25 ~ "Never Outdoors and Undercover",
    TRUE ~ NA_character_
  ))%>% 
  mutate(very_hot_very_cold_severity = case_when(
    very_hot_very_cold >= 75 ~ "Exposed to Very Hot or Very Cold Temperatures Once a Week or More but Not Every Day",
    very_hot_very_cold > 50 & very_hot_very_cold < 75 ~ "OutExposed to Very Hot or Very Cold Temperatures Once a Month or More but Not Every Week",
    very_hot_very_cold > 25 & very_hot_very_cold <= 50 ~ "Exposed to Very Hot or Very Cold Temperatures Once a Year or More but Not Every Month",
    very_hot_very_cold <= 25 ~ "Never Exposed to Very Hot or Very Cold Temperatures",
    TRUE ~ NA_character_
  ))


## getting into table format ----
# ### pivoting exposure type into its own column ----
# agg_long <- agg_ind_occ %>% 
#   pivot_longer(
#     cols = c(indoors_uncontrolled_severity, outdoors_exposed_severity, outdoors_undercover_severity, very_hot_very_cold_severity),
#       names_to = "exposure_type",
#       values_to = "severity"
#     )
# 
# ### dropping unnecessary columns, geom and dupes ----
# agg_clean <- agg_long %>% 
#   select(
#     industry_occupation,
#     indicator,
#     exposure_type,
#     severity,
#     total_per_ind_occ = total_count
#   ) %>% 
#   distinct() %>% 
#   st_drop_geometry()
# 
# ### calculating shares----
# agg_clean <- agg_clean %>% 
#   group_by(exposure_type, severity) %>% 
#   mutate(share = total_per_ind_occ/employed) %>% 
#   ungroup()
# 
# 
# ### recoding labels ----
# agg_clean <- agg_clean %>% 
#   mutate(
#     exposure_type = case_when(
#       exposure_type == "indoors_controlled_severity" ~ "Indoors (No Climate Control)",
#       exposure_type == "outdoors_exposed_severity" ~ "Outdoors (Exposed)",
#       exposure_type == "outdoors_undercover_severity" ~ "Outdoors (Undercover)",
#       exposure_type == "very_hot_very_cold_severity" ~ "Extreme Temps",
#       TRUE ~ exposure_type
#     )
#   ) %>% 
#   filter(!is.na(severity))
# 
# ## Summaries ----
# study_area_severity <- agg_clean %>% 
#   group_by(exposure_type, severity) %>% 
#   summarise(
#     total_employees = total_per_ind_occ,
#     share = total_employees / employed
#   )

# Start by calculating the exposure rate PER EACH EXPOSURE CATEGORY AND SEVERITY
agg_rates <- agg_ind_occ %>% 
    pivot_longer(
      cols = c(indoors_uncontrolled_severity, outdoors_exposed_severity, outdoors_undercover_severity, very_hot_very_cold_severity),
        names_to = "exposure_type",
        values_to = "severity"
      ) %>% 
  group_by(exposure_type) %>%
  mutate(
    indoors_rate = total_count / employed,
    outdoors_exposed_rate = total_count / employed,
    outdoors_undercover_rate = total_count / employed,
    extreme_temp_rate = total_count / employed
  ) %>% 
  ungroup() %>% 
  group_by(severity) %>% 
  mutate(
  indoors_severity_rate = total_count / employed,
outdoors_exposed_severity_rate = total_count / employed,
outdoors_undercover_severity_rate = total_count / employed,
very_hot_very_cold_severity_rate = total_count / employed
  ) %>% 
  ungroup()

# Building exposure indices
## Total average exposure ----
heat_clean <- heat %>% 
  mutate( #recoding variable to match calculation
    industry_occupation = case_when(
      industry_occupation == "ARTS ENTERTAINMENT AND RECREATION AND ACCOMMODATION AND FOOD SERVICES" ~ "ARTS ENTERTAINMENT AND RECREATION AND ACCOMMODATION AND FOOD SERVICES AND RETAIL TRADE",
      TRUE ~ industry_occupation
    )
  ) %>% 
  group_by(industry_occupation, code_2_digit) %>% 
  summarise(
    across(
      c(indoors_controlled, outdoors_exposed, outdoors_undercover, very_hot_very_cold),
      mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## Build indices----
heat_clean <- heat_clean %>% 
  mutate( #weighted exposure
    weighted_exposure = (
      0.2 * indoors_controlled +
        0.3 * outdoors_exposed +
        0.2 * outdoors_undercover +
        0.3 * very_hot_very_cold
    )
  ) %>% 
  mutate( #average exposure
    total_exposure = rowMeans(
      select(., indoors_controlled, outdoors_exposed, outdoors_undercover, very_hot_very_cold),
      na.rm = TRUE
    )
  )  %>% # standardized average (z-score) for comparing accross occupations. SHows relative exposure and if something is above/below average
  mutate(across(
    c(indoors_controlled, outdoors_exposed, outdoors_undercover, very_hot_very_cold),
    scale
  )) %>% 
  mutate(total_exposure_z = rowMeans(across(
    c(indoors_controlled, outdoors_exposed, outdoors_undercover, very_hot_very_cold)
  ), na.rm = TRUE)) 

exposure_indices <- heat_clean %>% 
  select(industry_occupation, total_exposure, weighted_exposure, z_score = total_exposure_z)

# Rebuilding basic ind_occ table with indices ----
## rebuild ind_occ from study area dp03----
# Filtering out data on study area industry/occupation breakdown. Removing percentages because they are at the tract level. 
ind_occ_new <- study_dp03e %>% 
  filter(indicator == "INDUSTRY" | indicator == "OCCUPATION",
         !str_detect(variable, "P$")) %>% 
  select(
    GEOID,
    tract,
    indicator,
    metric,
    industry_occupation = detail_1,
    estimate,
    moe,
    geometry
  ) %>% 
  mutate(industry_occupation = case_when(
    is.na(industry_occupation) ~ "TOTAL STUDY AREA EMPLOYMENT",
    TRUE ~ industry_occupation
  )) 


## summarizing across study area----
# Creating a summary of aggregate industry/occupation employment with shares across the entire study area. 
ind_sum_new <- ind_occ_new %>% 
  mutate( #combine retail and arts to accommodate the measure I created
    industry_occupation = case_when(
      industry_occupation == "RETAIL TRADE" ~ 
        "ARTS ENTERTAINMENT AND RECREATION AND ACCOMMODATION AND FOOD SERVICES AND RETAIL TRADE",
      industry_occupation == "ARTS ENTERTAINMENT AND RECREATION AND ACCOMMODATION AND FOOD SERVICES" ~ "ARTS ENTERTAINMENT AND RECREATION AND ACCOMMODATION AND FOOD SERVICES AND RETAIL TRADE",
      TRUE ~ industry_occupation
    )
  ) 

agg_ind_sum_new <- ind_sum_new %>% 
  group_by(indicator, industry_occupation) %>% 
  summarise(study_area_count = sum(estimate)) %>% 
  mutate(
    total = study_area_count[industry_occupation == "TOTAL STUDY AREA EMPLOYMENT"][1],
    share_occupied = study_area_count / total,
    .before = geometry
  ) %>% 
  select(-total) %>% 
  group_by(indicator) %>% 
  arrange(indicator, desc(share_occupied))

## Cleaning up labels ----

ind_occ_new <- ind_occ_new %>% 
  mutate(
    industry_occupation = toupper(industry_occupation)
  ) %>% 
  mutate(
    industry_occupation = str_remove_all(industry_occupation, "[[:punct:]]")
  )


ind_sum_new <- ind_sum_new %>% 
  mutate(
    industry_occupation = toupper(industry_occupation)
  ) %>% 
  mutate(
    industry_occupation = str_remove_all(industry_occupation, "[[:punct:]]")
  )

exposure_indices <- exposure_indices %>% 
  mutate(
    industry_occupation = toupper(industry_occupation)
  ) %>% 
  mutate(
    industry_occupation = str_remove_all(industry_occupation, "[[:punct:]]")
  )

## Tract-level exposure----

tract_exposure_summary <- ind_sum_new %>% 
  left_join(exposure_indices, by = "industry_occupation")

## Study-area exposure----
agg_exposure_summary <- agg_ind_sum_new %>% 
  left_join(exposure_indices, by = "industry_occupation")

## Adding variables to show total share above average exposure ----
agg_exposure_summary <- agg_exposure_summary %>% 
  mutate(
    above_below_z = case_when(
      z_score >= 1 ~ "Extremely High (more than 2x above average)",
      z_score >= 0.5 ~ "Moderately High (between 1.5x and 2x above average)",
      z_score > 0 ~ "Above Average (higher than average but not exceeding 1.5x)",
      z_score >= -0.5 ~ "Below Average (lower than average but not exceeding 1.5x)",
      z_score>= -1 ~ "Moderately Below Average (less than 1.5x to 2x below average)",
      z_score < -1 ~ "Extremely Low (less than 2x below overage)",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(
    above_0_5 = if_else(z_score >= 0.5, "Above 0.5", "Below 0.5")
  )

#removing rows for public sector jobs, total emp
agg_exposure_summary <- agg_exposure_summary %>% 
  filter(!is.na(total_exposure))


## Summarizing by exposure level----
### threshold measure ----
threshold_exposure <- agg_exposure_summary %>% 
  group_by(indicator) %>%
  mutate(
    ind_occ_employment = sum(study_area_count, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  group_by(indicator, above_0_5) %>% 
  summarise(
    total_employees = sum(study_area_count, na.rm = TRUE),
    ind_occ_employment = first(ind_occ_employment),
    share = total_employees / ind_occ_employment,
    .groups = "drop"
  )

### binned measure ----
binned_exposure <- agg_exposure_summary %>% 
  group_by(indicator) %>%
  mutate(
    ind_occ_employment = sum(study_area_count, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  group_by(indicator, above_below_z) %>% 
  summarise(
    total_employees = sum(study_area_count, na.rm = TRUE),
    ind_occ_employment = first(ind_occ_employment),
    share = total_employees / ind_occ_employment,
    .groups = "drop"
  )

# Tables for Exposure----
## Threshold----
### Table format ---
threshold_exposure_clean <- threshold_exposure %>% 
  sf::st_drop_geometry() %>% 
  ungroup() %>% 
  dplyr::select(indicator, above_0_5, total_employees, share)

### V1 ----
threshold_exposure_clean %>% 
  arrange(indicator, above_0_5) %>% 
  select(indicator, above_0_5, total_employees, share) %>% 
  kable(
    digits = 4,
    col.names = c("Sector", "Exposure Group", "Employees", "Share")
  ) %>% 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

### V2 ----
threshold_exposure_clean %>% 
  select(indicator, above_0_5, share) %>% 
  mutate(share = scales::percent(share, accuracy = 0.1)) %>% 
  tidyr::pivot_wider(
    names_from = above_0_5,
    values_from = share
  ) %>% 
  kable(
    col.names = c("Sector", "Below 0.5", "Above 0.5")
  ) %>% 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

## Binned----
### Table format ---
binned_exposure_clean <- binned_exposure %>% 
  sf::st_drop_geometry() %>% 
  ungroup() %>% 
  dplyr::select(indicator, above_below_z, total_employees, share)

### V1 ----
binned_exposure_clean %>% 
  arrange(indicator, above_below_z) %>% 
  select(indicator, above_below_z, total_employees, share) %>% 
  kable(
    digits = 4,
    col.names = c("Sector", "Exposure Severity", "Employees", "Share")
  ) %>% 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

### V2 ----
threshold_exposure_clean %>% 
  select(indicator, above_below_05, share) %>% 
  mutate(share = scales::percent(share, accuracy = 0.1)) %>% 
  tidyr::pivot_wider(
    names_from = above_below_05,
    values_from = share
  ) %>% 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )


  #             mutate(
  #             ind_occ_employment = sum(study_area_count),
  #             .before = geometry
  #             ) %>%
  # ungroup() %>% 
  # group_by(indicator, above_0_5) %>% 
  # summarise(
  #   total_employees = employed,
  #   share = study_area_count / ind_occ_employment
  # )
  # # group_by(indicator, above_below_z) %>% 
  # # summarise(severity_employees = sum(study_area_count)) %>% 
  # #             ungroup() %>% 
  # 
  # #             group_by(above_below_z) %>% 
  # #             mutate(
  # #           share = severity_employees/total_industry_employees, .before = geometry) %>% 
  # # ungroup()
