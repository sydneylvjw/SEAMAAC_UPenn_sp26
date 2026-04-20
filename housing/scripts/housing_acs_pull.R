# set up ----
options(
  tigris_use_cache = T,
  scipen = 999
)
## Install packages ----
library(pacman)
p_load(tidyverse, sf, tigris, tidycensus, data.table, DT, tmap, stringr, here)


# PULL DATA ----
## Create filtering vectors ----
# filter by tracts
tracts <- c("004001", "004002", "004101", "004103", "004104", "004201", "004202", "003720")

## Pulling housing data ----
### Pull variable labels ----

vars <- load_variables(2024, "acs5", cache = TRUE)

### Vector of applicable housing variables ----
var_names <- c(
  med_hh_income = "B19013_001",
  total_pop = "B01003_001",
  med_yr_built = "B25037_001",
  med_yr_built_owner = "B25037_002",
  med_yr_built_renter = "B25037_003",
  med_value = "B25107_001",
  total_households = "B25003_001",
  owner_occupied = "B25003_002",
  SMOCAPI_pct = "B25092_001",
  renter_occupied = "B25003_003",
  GRAPI_pct = "B25071_001",
  occupied_avg_hh_size = "B25010_001",
  owner_occupied_avg_hh_size = "B25010_002",
  renter_occupied_avg_hh_size = "B25010_003",
  plumbing_total = "B25047_001",
  lacks_plumbing_total = "B25047_003",
  owner_occupied_lacks_plumbing = "B25049_004",
  renter_occupied_lacks_plumbing = "B25049_007",
  kitchen_total = "B25051_001",
  lacks_kitchen_total = "B25051_003",
  owner_occupied_lacks_kitchen = "B25053_004",
  renter_occ_lacks_kitchen = "B25053_007",
  median_move_in_year = "B25039_001",
  owner_occupied_median_move_in_year = "B25039_002",
  renter_occupied_median_move_in_year = "B25039_003",
  poverty_below = "B16009_001",
  poverty_below_total = "B16009_002",
  poverty_below_child = "B16009_003",
  poverty_below_child_english = "B16009_004",
  poverty_below_child_spanish = "B16009_005",
  poverty_below_child_indoeuro = "B16009_006",
  poverty_below_child_aapi = "B16009_007",
  poverty_below_otherlang = "B16009_008",
  poverty_below_adult = "B16009_009",
  poverty_below_adult_english = "B16009_010",
  poverty_below_adult_spanish = "B16009_011",
  poverty_below_adult_euroindo = "B16009_012",
  poverty_below_adult_aapi = "B16009_013",
  poverty_below_adult_other = "B16009_014",
  poverty_below_male = "B17001_003",
  poverty_below_male_laborforce = "B17005_004",
  poverty_below_male_employed = "B17005_005",
  poverty_below_male_unemployed = "B17005_006",
  poverty_below_male_outside_laborforce = "B17005_007",
  poverty_below_female = "B17001_017",
  poverty_below_female_laborforce = "B17005_009",
  poverty_below_female_employed = "B17005_010",
  poverty_below_female_unemployed = "B17005_011",
  poverty_below_female_outside_laborforce = "B17005_012",
  disability_total = "B18101_001",
  disability_male = "B18101_002",
  disability_female = "B18101_021"
  )


### Pull housing data and join with variable labels ----
housing <- get_acs(
  geography = "tract",
  state = "PA",
  county = "Philadelphia County",
  year = 2024,
  survey = "acs5",
  variables = var_names,
  cache_table = T,
  geometry = T
)

## Filter by census tracts ----
### Separate geography name column ----
housing <- housing %>% 
  separate(col = NAME,
           into = c("tract_name", "county", "state"),
           sep = "; ",
           remove = T) 

### Break census tract code out of GEOID ----
housing <- housing %>% 
  mutate(tract = str_sub(GEOID, -6))

### Filter by tract vector ----
seamaac <- housing %>% 
  filter(tract %in% tracts)


# Claude step-by-step for HVI analysis----
## Principle Component Analysis ----
### Reformat to wide ----
# You'll drop the MOE for the PCA
seamaac_wide <- seamaac %>%
  select(GEOID, tract_name, tract, variable, estimate) %>% 
  pivot_wider(
    names_from  = variable,
    values_from = estimate
  )

### Engineer derived variable rates for PCA ----
# Can't use raw counts across different-sized tracts. Make sure you use summary variables from the table universe when calculating rates. 

seamaac_wide <- seamaac_wide %>%
  mutate(
    pct_owner_occupied  = owner_occupied / total_households,
    pct_renter_occupied = renter_occupied / total_households,
    pct_lacks_plumbing  = lacks_plumbing_total / plumbing_total,
    pct_lacks_kitchen   = lacks_kitchen_total / kitchen_total,
    pct_poverty_below   = poverty_below_total / poverty_below,
    owner_cost_ratio    = SMOCAPI_pct,           # corrected name
    renter_cost_ratio   = GRAPI_pct,             # corrected name
    log_med_income      = log(med_hh_income)
  )


### Pull PCA variables ----
# Sticking to just the derived totals and the medians. 

pca_vars <- c(
  #"med_yr_built", ===dropping this since all but one tract were built in 1938===
  "med_value",
  "log_med_income",
  "owner_cost_ratio",
  "renter_cost_ratio",
  "pct_owner_occupied",
  "pct_lacks_plumbing",
  "pct_lacks_kitchen",
  "pct_poverty_below",
  #"pct_disability", ===dropped this bc there is zero variance===
  "occupied_avg_hh_size"
)

# Pull just those columns, drop any tract with NA in any PCA variable
pca_input <- seamaac_wide %>%
  select(GEOID, tract_name, tract, all_of(pca_vars)) %>%
  drop_na(all_of(pca_vars)) %>% 
  st_drop_geometry() %>%                          # removes the sticky geometry column
  select(GEOID, tract_name, tract, all_of(pca_vars))

### Run PCA ----
#### Scale and center ----
# This is required — variables are on very different scales
pca_result <- pca_input %>%
  select(all_of(pca_vars)) %>%
  scale() %>%
  prcomp()

#### Inspect the results ----

# proportion of variance explained per component
summary(pca_result)


# loadings: how each variable contributes to each PC
pca_result$rotation %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  select(variable, PC1, PC2, PC3) %>%
  arrange(desc(abs(PC1))) %>%
  print()      

# sanity check to examine each tract by key metrics
pca_input %>%
  select(tract, renter_cost_ratio, pct_poverty_below, 
         pct_lacks_plumbing, log_med_income, occupied_avg_hh_size) %>%
  mutate(
    income_rank   = rank(-log_med_income),      # lower income = higher rank
    poverty_rank  = rank(-pct_poverty_below),
    rent_rank     = rank(-renter_cost_ratio),
    plumbing_rank = rank(-pct_lacks_plumbing),
    crowding_rank = rank(-occupied_avg_hh_size)
  ) %>%
  mutate(naive_rank = income_rank + poverty_rank + rent_rank + 
           plumbing_rank + crowding_rank) %>%
  arrange(naive_rank) %>%
  print()


### Extract PC scores and weights ----
# Extract the variance explained by each PC
pc_variance <- summary(pca_result)$importance["Proportion of Variance", ]

# Variance weights
pc1_weight   <- pc_variance["PC1"]   # 0.4433
pc2_weight   <- pc_variance["PC2"]   # 0.2424
total_weight <- pc1_weight + pc2_weight

# Extract PC scores and bind tract IDs
pc_scores <- as.data.frame(pca_result$x) %>%
  select(PC1, PC2) %>%
  bind_cols(pca_input %>% select(GEOID, tract_name, tract))

# Check directionality before flipping
# 004103 has highest poverty/lowest income — should have highest vulnerability
# Check its raw PC1 and PC2 scores:
pc_scores %>% arrange(PC1) %>% print()
pc_scores %>% arrange(PC2) %>% print()

### Run final composite ----
seamaac_scored <- pc_scores %>%
  mutate(
    PC1_oriented = PC1 * -1,   # flip: low PC1 = high poverty/low income = more vulnerable
    PC2_oriented = PC2 * -1,   # flip: low PC2 = high rent burden/lacks plumbing = more vulnerable
    hvi_raw = (PC1_oriented * pc1_weight + PC2_oriented * pc2_weight) / total_weight
  ) %>%
  mutate(
    hvi_score = (hvi_raw - min(hvi_raw)) / (max(hvi_raw) - min(hvi_raw)) * 100
  ) %>%
  arrange(desc(hvi_score))


### Preview weighted scores ----
seamaac_scored %>%
  select(tract, hvi_score, hvi_raw, PC1_oriented, PC2_oriented) %>%
  print()


### Rejoin to geometry and map ----
seamaac_hvi <- seamaac %>%
  select(GEOID, geometry) %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  right_join(
    seamaac_scored %>% select(GEOID, tract_name, tract, hvi_score, hvi_raw),
    by = "GEOID"
  )

ggplot(seamaac_hvi) +
  geom_sf(aes(fill = hvi_score), color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(
    option   = "magma",
    direction = -1,
    name     = "HVI Score",
    labels   = scales::label_number(accuracy = 1)
  ) +
  geom_sf_label(
    aes(label = paste0(tract, "\n", round(hvi_score, 1))),
    size = 2.8, fill = "white", alpha = 0.7
  ) +
  labs(
    title    = "Heat Vulnerability Index",
    subtitle = "SEAMAAC Service Area Census Tracts | 2020–2024 ACS 5-Year",
    caption  = "Higher scores indicate greater heat vulnerability.\nComposite of housing quality, cost burden, income, poverty, and crowding indicators.\nPC1+PC2 weighted z-score method (PC1=44.3%, PC2=24.2% variance explained)."
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    plot.caption  = element_text(size = 7, color = "grey50")
  )

### Outline of PC weights ----
# Document your PC weights explicitly for reproducibility
tibble(
  component  = c("PC1", "PC2"),
  weight     = c(pc1_weight, pc2_weight),
  pct_of_two = c(pc1_weight, pc2_weight) / total_weight * 100,
  top_drivers = c(
    "med_value, log_med_income, occupied_avg_hh_size (affluence/crowding axis)",
    "renter_cost_ratio, pct_lacks_plumbing (burden/housing quality axis)"
  )
) %>% print()
# 
# # Extract tract scores on PC1 and PC2
# pc_scores <- as.data.frame(pca_result$x) %>%
#   select(PC1, PC2) %>%
#   bind_cols(pca_input %>% select(GEOID, tract_name, tract))
# 
# ### Define directionaly and build z-score ----
# # Variables to FLIP (higher raw value = less vulnerable, need to invert)
# flip_vars <- c(
#   "med_value",
#   "pct_owner_occupied",
#   "pct_lacks_kitchen",    # loads negatively on PC1 — more lacking = more vulnerable
#   "pct_poverty_below",    # loads negatively on PC1 — more poverty = more vulnerable
#   "occupied_avg_hh_size"  # loads negatively on PC1 — more crowding = more vulnerable
# )
# 
# # Variables to KEEP as-is (higher raw value = more vulnerable)
# keep_vars <- c(
#   "med_SMOCAPI",
#   "med_GRAPI",
#   "pct_lacks_plumbing"
# )
# 
# # Drop pct_renter_occupied — redundant with pct_owner_occupied
# composite_vars <- c(keep_vars, flip_vars)
# 
# # Z-score all composite vars, then flip the ones that need inverting
# seamaac_scored <- pca_input %>%
#   mutate(across(all_of(composite_vars), scale, .names = "z_{.col}")) %>%
#   mutate(across(
#     all_of(paste0("z_", flip_vars)),
#     ~ . * -1
#   )) %>%
#   # Weighted composite: sum z-scores, weighted by PC1 and PC2 variance share
#   # For PC1+PC2 weighting we apply weights proportional to their share of the two-PC total
#   mutate(
#     pc1_contrib = pc1_weight / (pc1_weight + pc2_weight),
#     pc2_contrib = pc2_weight / (pc1_weight + pc2_weight),
#     # PC1-loaded variables (dominant signal)
#     pc1_score = (
#       z_med_value +
#         z_med_SMOCAPI +
#         z_pct_owner_occupied +
#         z_pct_lacks_plumbing +
#         z_med_GRAPI +
#         z_occupied_avg_hh_size +
#         z_pct_lacks_kitchen +
#         z_pct_poverty_below
#     ),
#     # PC2 adds independent weight to ownership/burden/plumbing axis
#     pc2_score = (
#       z_pct_owner_occupied +
#         z_med_GRAPI +
#         z_pct_lacks_plumbing
#     ),
#     hvi_raw = (pc1_score * pc1_contrib) + (pc2_score * pc2_contrib)
#   ) %>%
#   # Rescale to 0–100
#   mutate(
#     hvi_score = (hvi_raw - min(hvi_raw)) / (max(hvi_raw) - min(hvi_raw)) * 100
#   ) %>%
#   arrange(desc(hvi_score))
# 
# ### Preview results ----
# seamaac_scored %>%
#   select(GEOID, tract_name, tract, hvi_score, hvi_raw) %>%
#   print()
# 
# ### Join back to geometry for mapping ----
# seamaac_hvi <- seamaac %>%
#   select(GEOID, geometry) %>%
#   distinct(GEOID, .keep_all = TRUE) %>%
#   right_join(
#     seamaac_scored %>% select(GEOID, tract_name, tract, hvi_score, hvi_raw),
#     by = "GEOID"
#   )
# 
# ### Choropleth ----
# library(ggplot2)
# 
# ggplot(seamaac_hvi) +
#   geom_sf(aes(fill = hvi_score), color = "white", linewidth = 0.5) +
#   scale_fill_viridis_c(
#     option = "magma",
#     direction = -1,
#     name = "HVI Score",
#     labels = scales::label_number(accuracy = 1)
#   ) +
#   geom_sf_label(aes(label = paste0(tract, "\n", round(hvi_score, 1))),
#                 size = 2.8, fill = "white", alpha = 0.7) +
#   labs(
#     title = "Heat Vulnerability Index",
#     subtitle = "SEAMAAC Service Area Census Tracts | 2020–2024 ACS 5-Year",
#     caption = "Higher scores indicate greater heat vulnerability.\nComposite of housing quality, cost burden, poverty, and crowding indicators.\nPC1+PC2 weighted z-score method."
#   ) +
#   theme_void() +
#   theme(
#     plot.title    = element_text(face = "bold", size = 14),
#     plot.subtitle = element_text(size = 9, color = "grey40"),
#     plot.caption  = element_text(size = 7, color = "grey50")
#   )
