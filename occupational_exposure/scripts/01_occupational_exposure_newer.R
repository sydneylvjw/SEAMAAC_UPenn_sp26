# ==============================================================================
# SEAMAAC HVI — Occupational Heat Exposure Measure (v1, non-PUMS)
# ------------------------------------------------------------------------------
# Following Laskaris, Z. (2025). "Occupational characteristics are missing from
# heat vulnerability indices: A study in New York and New Jersey." Adapted for
# the 8-tract SEAMAAC study area in South Philadelphia.
#
# This script replaces the prior Excel-based `acs_grouping_exposure.xlsx`
# workflow with an end-to-end replicable R pipeline. No hand-edited values;
# no external crosswalks that collide with ACS DP03 supersector definitions.
#
# Methodological choices (see Step 0 config to change any of these):
#   • Three O*NET Work Context variables; "very hot or very cold" EXCLUDED to
#     match Laskaris (who drops it because it conflates furnace/stove radiant
#     heat with weather exposure).
#   • Absolute threshold of 75 on the 0-100 frequency scale = "once a week or
#     more" (Laskaris §2.1.1).
#   • Occupation: employment-weighted rollup from SOC-6 -> ACS occupation group
#     using BLS OES national cross-industry employment.
#   • Industry: employment-weighted rollup from SOC-6 within NAICS-4 -> ACS
#     industry supersector using BLS OES industry-specific employment.
#   • A v2 PUMS-based version (PUMA-level share at SOCP detail, applied to
#     tract counts) will replace Steps 9-10 of this file and live in
#     `occupational_exposure/scripts/02_occupational_exposure_pums.R`.
#
# Outputs, written to /output/:
#   occ_exposure_by_acs_group.csv   — ACS occupation group rollup
#   ind_exposure_by_acs_group.csv   — ACS industry supersector rollup
#   tract_occupational_exposure.gpkg — Tract-level exposure (map-ready)
# ==============================================================================


# ---- 0. CONFIG -----------------------------------------------------------------
# All user-modifiable settings live here. Nothing below this block requires
# editing to run the pipeline.

# ---------------------------------------------------------------------------
# Path to your Cowork workspace folder ("SEAMAAC HVI") on this machine.
# Derived automatically: two levels up from the R project root puts us in
# Documents/MCPMUSA/, and the workspace folder should be there.
# If this guess is wrong, replace the right-hand side with the literal path,
# e.g.  .workspace <- "/Users/sydneyjones/Desktop/SEAMAAC HVI"
# ---------------------------------------------------------------------------
.workspace <- file.path("occupational_exposure/")

config <- list(
  
  # SEAMAAC study area — 8 census tracts in Philadelphia County (FIPS 42101)
  study_tracts = c(
    "42101004001", "42101004002", "42101004101", "42101004103",
    "42101004104", "42101004201", "42101004202", "42101037200"
  ),
  
  # ACS vintage
  acs_year   = 2024,
  acs_survey = "acs5",
  acs_state  = "PA",
  acs_county = "101",
  
  # Laskaris (2025) threshold — raw 0-100 frequency scale
  exposure_threshold = 75,
  
  # Three O*NET Work Context elements. Add "4.C.2.b.1.e" (Very Hot or Very Cold)
  # if you want to depart from Laskaris and include radiant-heat exposure.
  onet_elements = c(
    indoors_uncontrolled = "4.C.2.a.1.b",
    outdoors_exposed     = "4.C.2.a.1.c",
    outdoors_undercover  = "4.C.2.a.1.d"
  ),
  
  # BLS OES national release year. Industry-specific file is `oesm{yy}in4.zip`.
  # Update if BLS publishes a newer year you want to use.
  oes_year = 2023,
  
  # URLs — kept here so it's obvious where data comes from.
  onet_wc_url = "https://www.onetcenter.org/dl_files/database/db_29_2_excel/Work%20Context.xlsx",
  oes_in4_url = "https://www.bls.gov/oes/special-requests/oesm23in4.zip",
  
  # Paths — all under the Cowork workspace folder, not the R project root.
  cache_dir  = file.path(.workspace, "data_cache"),
  data_in    = file.path(.workspace, "data"),
  output_dir = file.path(.workspace, "output")
)

purrr::walk(
  c(config$cache_dir, config$output_dir),
  dir.create, recursive = TRUE, showWarnings = FALSE
)


# ---- 1. LIBRARIES --------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
p_load(tidyverse, sf, tidycensus, tigris, readxl, janitor, glue, stringr)

options(tigris_use_cache = TRUE, scipen = 999)


# ---- 2. HELPERS ----------------------------------------------------------------

# Download a URL to the cache directory if not already present.
cache_download <- function(url, filename, cache_dir = config$cache_dir,
                           overwrite = FALSE, timeout = 600) {
  path <- file.path(cache_dir, filename)
  if (!file.exists(path) || overwrite) {
    message(glue::glue("Downloading {filename} ..."))
    old_to <- getOption("timeout"); on.exit(options(timeout = old_to), add = TRUE)
    options(timeout = timeout)
    utils::download.file(url, path, mode = "wb", quiet = FALSE)
  } else {
    message(glue::glue("Using cached {filename}"))
  }
  path
}


# ---- 3. O*NET WORK CONTEXT -----------------------------------------------------
# Source: O*NET 29.2 Work Context (the version used by Laskaris 2025).
# Scale: 1–5 mean frequency ("CT" = Context subscale); converted to 0–100 via
#        score = (value - 1) * 25   (Laskaris §2.1).

onet_wc_path <- cache_download(config$onet_wc_url, "onet_work_context_29_2.xlsx")

onet_wc_raw <- readxl::read_xlsx(onet_wc_path) |>
  janitor::clean_names()

onet_wc <- onet_wc_raw |>
  dplyr::filter(
    element_id %in% config$onet_elements,
    scale_id == "CX"
  ) |>
  dplyr::mutate(
    # Map each element_id back to the short name from config$onet_elements
    element_short = names(config$onet_elements)[match(element_id, config$onet_elements)],
    score_0_100   = (data_value - 1) * 25
  ) |>
  dplyr::select(onet_soc_code = o_net_soc_code, element_short, score_0_100) |>
  tidyr::pivot_wider(names_from = element_short, values_from = score_0_100)


# ---- 4. COLLAPSE O*NET-SOC TO 2018 SOC-6 --------------------------------------
# O*NET-SOC codes look like "11-1011.00" or "11-1011.03". Multiple can share a
# SOC-6 parent; per Laskaris we impute the mean across them.

soc_exposure <- onet_wc |>
  dplyr::mutate(soc_2018 = stringr::str_extract(onet_soc_code, "^[0-9]{2}-[0-9]{4}")) |>
  dplyr::group_by(soc_2018) |>
  dplyr::summarise(
    indoors_uncontrolled = mean(indoors_uncontrolled, na.rm = TRUE),
    outdoors_exposed     = mean(outdoors_exposed,     na.rm = TRUE),
    outdoors_undercover  = mean(outdoors_undercover,  na.rm = TRUE),
    n_onet_codes         = dplyr::n(),
    .groups = "drop"
  ) |>
  # Laskaris §2.1.1: heat-exposed = indoor OR outdoor score >= threshold.
  # "Outdoor" = max across the two outdoor items (most inclusive).
  dplyr::mutate(
    outdoor_score    = pmax(outdoors_exposed, outdoors_undercover, na.rm = TRUE),
    indoor_score     = indoors_uncontrolled,
    heat_exposed_in  = indoor_score  >= config$exposure_threshold,
    heat_exposed_out = outdoor_score >= config$exposure_threshold,
    heat_exposed_any = heat_exposed_in | heat_exposed_out
  )


# ---- 5. BLS OES NATIONAL EMPLOYMENT (occupation weighting) -------------------
# Source: BLS OES Report, national cross-industry totals by SOC.
# Expected at data_in/OES_Report.xlsx (included with this project).

oes_national_path <- file.path("occupational_exposure/data/OES_Report.xlsx") 

oes_national <- readxl::read_xlsx(oes_national_path, sheet = "OES Sheet") |>
  dplyr::rename(occupation_raw = 1, national_emp = 2) |>
  dplyr::filter(!is.na(occupation_raw), !is.na(national_emp)) |>
  dplyr::mutate(
    national_emp = suppressWarnings(as.numeric(national_emp)),
    soc_2018     = stringr::str_extract(occupation_raw, "[0-9]{2}-[0-9]{4}"),
    soc_title    = stringr::str_trim(
      stringr::str_remove(occupation_raw, "\\s*\\([0-9]{2}-[0-9]{4}\\)$")
    )
  ) |>
  dplyr::filter(!is.na(soc_2018), !is.na(national_emp)) |>
  # Keep only detailed SOC codes present in the O*NET-derived scores
  # (this drops major/broad/minor aggregate SOC rows automatically).
  dplyr::semi_join(soc_exposure, by = "soc_2018") |>
  dplyr::select(soc_2018, soc_title, national_emp)


# ---- 6. BLS OES INDUSTRY-SPECIFIC (industry weighting) ------------------------
# Source: BLS OES Industry-Specific file, NAICS 4-digit × SOC 6-digit.
# Downloads and caches automatically.

oes_in4_file <- file.path("occupational_exposure/data/raw/oesm23in4/nat4d_owner_M2023_dl.xlsx")


# The nat4d file holds NAICS-4 × SOC-6 national employment


oes_industry <- readxl::read_xlsx(oes_in4_file) |>
  janitor::clean_names() |>
  dplyr::filter(o_group == "detailed") |>
  dplyr::mutate(
    tot_emp  = suppressWarnings(as.numeric(tot_emp)),
    naics    = as.character(naics),
    naics_2  = stringr::str_sub(naics, 1, 2),
    soc_2018 = occ_code
  ) |>
  dplyr::filter(!is.na(tot_emp), !is.na(naics_2)) |>
  dplyr::select(naics, naics_2, soc_2018, tot_emp)


# ---- 7. ACS DP03 CROSSWALKS ---------------------------------------------------
# Both rollups encode *ACS published definitions* for the DP03 profile table.
# They are stable across recent ACS 5-year vintages. Sources in comments.
#
# Why not pull programmatically? The Census Industry Code List file *does* ship
# the mapping, but its layout changes between releases. Hard-coding the
# 2-digit rollup here makes the script robust to that and self-documenting.
# Swap-in from the Code List is trivial if you ever need a newer vintage.

# 7a) NAICS 2-digit -> ACS DP03 industry supersector
# Source: ACS Subject Definitions (DP03 Industry section) + Census Industry Code
# List 2022. https://www.census.gov/topics/employment/industry-occupation/
# guidance/code-lists.html
naics2_to_acs <- tibble::tribble(
  ~naics_2, ~acs_industry,
  "11", "Agriculture, forestry, fishing and hunting, and mining",
  "21", "Agriculture, forestry, fishing and hunting, and mining",
  "22", "Transportation and warehousing, and utilities",
  "23", "Construction",
  "31", "Manufacturing",
  "32", "Manufacturing",
  "33", "Manufacturing",
  "42", "Wholesale trade",
  "44", "Retail trade",
  "45", "Retail trade",
  "48", "Transportation and warehousing, and utilities",
  "49", "Transportation and warehousing, and utilities",
  "51", "Information",
  "52", "Finance and insurance, and real estate and rental and leasing",
  "53", "Finance and insurance, and real estate and rental and leasing",
  "54", "Professional, scientific, and management, and administrative and waste management services",
  "55", "Professional, scientific, and management, and administrative and waste management services",
  "56", "Professional, scientific, and management, and administrative and waste management services",
  "61", "Educational services, and health care and social assistance",
  "62", "Educational services, and health care and social assistance",
  "71", "Arts, entertainment, and recreation, and accommodation and food services",
  "72", "Arts, entertainment, and recreation, and accommodation and food services",
  "81", "Other services, except public administration",
  "92", "Public administration"
)

# 7b) SOC 2-digit -> ACS DP03 occupation major group
# Source: ACS Subject Definitions (DP03 Occupation section). Military (SOC 55)
# is excluded to match the ACS civilian-employed universe.
soc2_to_acs <- tibble::tribble(
  ~soc_2, ~acs_occupation,
  "11", "Management, business, science, and arts occupations",
  "13", "Management, business, science, and arts occupations",
  "15", "Management, business, science, and arts occupations",
  "17", "Management, business, science, and arts occupations",
  "19", "Management, business, science, and arts occupations",
  "21", "Management, business, science, and arts occupations",
  "23", "Management, business, science, and arts occupations",
  "25", "Management, business, science, and arts occupations",
  "27", "Management, business, science, and arts occupations",
  "29", "Management, business, science, and arts occupations",
  "31", "Service occupations",
  "33", "Service occupations",
  "35", "Service occupations",
  "37", "Service occupations",
  "39", "Service occupations",
  "41", "Sales and office occupations",
  "43", "Sales and office occupations",
  "45", "Natural resources, construction, and maintenance occupations",
  "47", "Natural resources, construction, and maintenance occupations",
  "49", "Natural resources, construction, and maintenance occupations",
  "51", "Production, transportation, and material moving occupations",
  "53", "Production, transportation, and material moving occupations"
)


# ---- 8. OCCUPATION ROLLUP -----------------------------------------------------
# For each ACS occupation major group, compute:
#   • employment-weighted mean exposure score (0–100) per O*NET item
#   • share of workers in heat-exposed occupations (indoor / outdoor / any)
# Weights = national SOC-6 employment (OES cross-industry).

occ_rollup <- soc_exposure |>
  dplyr::mutate(soc_2 = stringr::str_sub(soc_2018, 1, 2)) |>
  dplyr::inner_join(soc2_to_acs, by = "soc_2") |>
  dplyr::inner_join(oes_national |> dplyr::select(soc_2018, national_emp),
                    by = "soc_2018") |>
  dplyr::group_by(acs_occupation) |>
  dplyr::summarise(
    indoors_uncontrolled_wt = stats::weighted.mean(indoors_uncontrolled, national_emp, na.rm = TRUE),
    outdoors_exposed_wt     = stats::weighted.mean(outdoors_exposed,     national_emp, na.rm = TRUE),
    outdoors_undercover_wt  = stats::weighted.mean(outdoors_undercover,  national_emp, na.rm = TRUE),
    share_heat_any          = sum(national_emp * heat_exposed_any, na.rm = TRUE) /
      sum(national_emp, na.rm = TRUE),
    share_heat_indoor       = sum(national_emp * heat_exposed_in,  na.rm = TRUE) /
      sum(national_emp, na.rm = TRUE),
    share_heat_outdoor      = sum(national_emp * heat_exposed_out, na.rm = TRUE) /
      sum(national_emp, na.rm = TRUE),
    national_emp_total      = sum(national_emp, na.rm = TRUE),
    .groups = "drop"
  )


# ---- 9. INDUSTRY ROLLUP -------------------------------------------------------
# For each ACS industry supersector:
#   • employment-weighted mean exposure score per O*NET item, and
#   • share of workers in heat-exposed occupations.
# Weights = national SOC-6 employment *within each NAICS-4 industry* (OES
# industry-specific). Aggregating NAICS-4 -> NAICS-2 -> ACS supersector.

ind_rollup <- oes_industry |>
  dplyr::inner_join(naics2_to_acs, by = "naics_2") |>
  dplyr::inner_join(soc_exposure, by = "soc_2018") |>
  dplyr::group_by(acs_industry) |>
  dplyr::summarise(
    indoors_uncontrolled_wt = stats::weighted.mean(indoors_uncontrolled, tot_emp, na.rm = TRUE),
    outdoors_exposed_wt     = stats::weighted.mean(outdoors_exposed,     tot_emp, na.rm = TRUE),
    outdoors_undercover_wt  = stats::weighted.mean(outdoors_undercover,  tot_emp, na.rm = TRUE),
    share_heat_any          = sum(tot_emp * heat_exposed_any, na.rm = TRUE) /
      sum(tot_emp, na.rm = TRUE),
    share_heat_indoor       = sum(tot_emp * heat_exposed_in,  na.rm = TRUE) /
      sum(tot_emp, na.rm = TRUE),
    share_heat_outdoor      = sum(tot_emp * heat_exposed_out, na.rm = TRUE) /
      sum(tot_emp, na.rm = TRUE),
    industry_emp_total      = sum(tot_emp, na.rm = TRUE),
    .groups = "drop"
  )


# ---- 10. PULL ACS DP03 FOR STUDY TRACTS ---------------------------------------

dp03_vars <- tidycensus::load_variables(config$acs_year, "acs5/profile", cache = TRUE) |>
  dplyr::filter(grepl("^DP03_", name))

dp03_raw <- tidycensus::get_acs(
  geography   = "tract",
  state       = config$acs_state,
  county      = config$acs_county,
  table       = "DP03",
  year        = config$acs_year,
  survey      = config$acs_survey,
  geometry    = TRUE,
  cache_table = TRUE
) |>
  dplyr::left_join(dp03_vars, by = c("variable" = "name")) |>
  dplyr::filter(GEOID %in% config$study_tracts) |>
  # Drop percent rows; keep estimates only
  dplyr::filter(!stringr::str_ends(variable, "P"))

# Parse the hierarchical label into its levels
dp03_parsed <- dp03_raw |>
  tidyr::separate(
    col    = label,
    into   = c("measure", "indicator", "metric", "d1", "d2", "d3", "d4"),
    sep    = "!!",
    remove = FALSE,
    fill   = "right"
  )


# ---- 11. TRACT-LEVEL TOTAL EMPLOYED -------------------------------------------
# DP03 includes "EMPLOYMENT STATUS!!Civilian labor force!!Employed" — that's
# the denominator. We filter by label rather than hard-coding DP03_0004E so the
# script stays robust to ACS variable renumbering across vintages.

tract_employed <- dp03_parsed |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    indicator == "EMPLOYMENT STATUS",
    stringr::str_detect(label, "Civilian labor force!!Employed$")
  ) |>
  dplyr::select(GEOID, total_employed = estimate)


# ---- 12. TRACT-LEVEL EXPOSURE: OCCUPATION SIDE --------------------------------
# For each tract: share of employed residents in heat-exposed occupations =
#   Σ_g (tract_emp_in_group_g × national_share_heat_exposed_g) / total_employed

tract_occ <- dp03_parsed |>
  dplyr::filter(indicator == "OCCUPATION", !is.na(d1)) |>
  dplyr::select(GEOID, NAME, acs_occupation = d1, estimate, moe) |>
  sf::st_drop_geometry() |>
  dplyr::left_join(occ_rollup, by = "acs_occupation") |>
  dplyr::left_join(tract_employed, by = "GEOID")

tract_occ_summary <- tract_occ |>
  dplyr::group_by(GEOID, NAME, total_employed) |>
  dplyr::summarise(
    occ_heat_any_n       = sum(estimate * share_heat_any,     na.rm = TRUE),
    occ_heat_indoor_n    = sum(estimate * share_heat_indoor,  na.rm = TRUE),
    occ_heat_outdoor_n   = sum(estimate * share_heat_outdoor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    occ_heat_any_share      = occ_heat_any_n     / total_employed,
    occ_heat_indoor_share   = occ_heat_indoor_n  / total_employed,
    occ_heat_outdoor_share  = occ_heat_outdoor_n / total_employed
  )


# ---- 13. TRACT-LEVEL EXPOSURE: INDUSTRY SIDE ---------------------------------

tract_ind <- dp03_parsed |>
  dplyr::filter(indicator == "INDUSTRY", !is.na(d1)) |>
  dplyr::select(GEOID, NAME, acs_industry = d1, estimate, moe) |>
  sf::st_drop_geometry() |>
  dplyr::left_join(ind_rollup, by = "acs_industry") |>
  dplyr::left_join(tract_employed, by = "GEOID")

tract_ind_summary <- tract_ind |>
  dplyr::group_by(GEOID, NAME, total_employed) |>
  dplyr::summarise(
    ind_heat_any_n      = sum(estimate * share_heat_any,     na.rm = TRUE),
    ind_heat_indoor_n   = sum(estimate * share_heat_indoor,  na.rm = TRUE),
    ind_heat_outdoor_n  = sum(estimate * share_heat_outdoor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    ind_heat_any_share     = ind_heat_any_n     / total_employed,
    ind_heat_indoor_share  = ind_heat_indoor_n  / total_employed,
    ind_heat_outdoor_share = ind_heat_outdoor_n / total_employed
  )


# ---- 14. TRACT TABLE + GEOMETRY -----------------------------------------------

tract_geom <- dp03_raw |>
  dplyr::group_by(GEOID) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(GEOID, NAME)

tract_exposure <- tract_geom |>
  dplyr::left_join(
    tract_occ_summary |> dplyr::select(-NAME, -total_employed),
    by = "GEOID"
  ) |>
  dplyr::left_join(
    tract_ind_summary |> dplyr::select(-NAME, -total_employed),
    by = "GEOID"
  ) |>
  dplyr::left_join(tract_employed, by = "GEOID")


# ---- 15. WRITE OUTPUTS --------------------------------------------------------

readr::write_csv(occ_rollup, file.path(config$output_dir, "occ_exposure_by_acs_group.csv"))
readr::write_csv(ind_rollup, file.path(config$output_dir, "ind_exposure_by_acs_group.csv"))
sf::st_write(
  tract_exposure,
  file.path(config$output_dir, "tract_occupational_exposure.gpkg"),
  delete_dsn = TRUE
)

message("Wrote outputs to: ", config$output_dir)


# ---- 16. QUICK VALIDATION & MAP -----------------------------------------------
# Run these interactively to sanity-check. Comment out for batch runs.

if (interactive()) {
  # Rank ACS occupation groups by heat-exposed share
  print(occ_rollup |> dplyr::arrange(dplyr::desc(share_heat_any)))
  
  # Rank ACS industry supersectors by heat-exposed share
  print(ind_rollup |> dplyr::arrange(dplyr::desc(share_heat_any)))
  
  # Top-line tract numbers
  print(
    tract_exposure |>
      sf::st_drop_geometry() |>
      dplyr::select(GEOID, total_employed,
                    occ_heat_any_share, ind_heat_any_share) |>
      dplyr::arrange(dplyr::desc(occ_heat_any_share))
  )
  
  # Quick choropleth
  if (requireNamespace("tmap", quietly = TRUE)) {
    tmap::tmap_mode("plot")
    print(
      tmap::tm_shape(tract_exposure) +
        tmap::tm_polygons(
          "occ_heat_any_share",
          palette = "YlOrRd",
          title   = "Share of employed residents\nin heat-exposed occupations"
        ) +
        tmap::tm_layout(frame = FALSE)
    )
  }
}