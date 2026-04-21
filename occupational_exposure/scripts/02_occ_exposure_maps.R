# ==============================================================================
# SEAMAAC HVI — Occupational Heat Exposure Maps (v1)
# ------------------------------------------------------------------------------
# Produces choropleth maps at two scales:
#   (a) Philadelphia city level — all census tracts, SEAMAAC area outlined
#   (b) SEAMAAC study area — 8-tract zoomed view with tract labels
#
# Depends on outputs from 01_occupational_exposure.R (run that script first):
#   occupational_exposure/output/occ_exposure_by_acs_group.csv
#   occupational_exposure/output/ind_exposure_by_acs_group.csv
#   occupational_exposure/output/tract_occupational_exposure.gpkg
#
# For the city-level maps the script re-derives exposure for all Philadelphia
# tracts using those saved crosswalk CSVs + a fresh DP03 pull — no hand edits.
#
# Outputs (occupational_exposure/output/maps/):
#   occ_any_city.png            occ_any_study.png
#   occ_indoor_study.png        occ_outdoor_study.png
#   ind_any_city.png            ind_any_study.png
#   occ_combined_city.png       occ_combined_study.png
# ==============================================================================


# ---- 0. CONFIG ---------------------------------------------------------------

# Path to the Cowork workspace folder ("SEAMAAC HVI") on this machine.
# Derived automatically — replace if this guess is wrong.
.workspace <- tryCatch(
  normalizePath(file.path(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)),
                          "..", "..")),
  error = function(e) {
    file.path(path.expand("~"), "Documents", "Claude", "Projects", "SEAMAAC HVI")
  }
)

config <- list(
  
  study_tracts = c(
    "42101004001", "42101004002", "42101004101", "42101004103",
    "42101004104", "42101004201", "42101004202", "42101037200"
  ),
  
  acs_year   = 2024,
  acs_survey = "acs5",
  state      = "PA",
  county     = "101",
  
  data_dir   = file.path(.workspace, "occupational_exposure", "output"),
  output_dir = file.path(.workspace, "occupational_exposure", "output", "maps")
)

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)


# ---- 1. PACKAGES -------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
p_load(tidyverse, sf, tidycensus, ggrepel, showtext, scales, glue)

font_add_google("Exo 2",     "Exo 2")
font_add_google("Open Sans", "Open Sans")
showtext_auto()
showtext_opts(dpi = 150)

options(tigris_use_cache = TRUE, scipen = 999)


# ---- 2. PALETTE & THEME ------------------------------------------------------

pal <- list(
  navy  = "#1f3b57",
  steel = "#4a7ca0",
  sage  = "#7aa88c",
  sand  = "#e7d9b8",
  coral = "#d97757",
  rust  = "#a83c2b"
)

occ_gradient <- c(pal$sage, pal$sand, pal$coral, pal$rust)

hvi_theme <- function() {
  theme_void(base_family = "Open Sans") +
    theme(
      plot.title    = element_text(family = "Exo 2", face = "bold",
                                   size = 16, color = pal$navy,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10, color = "grey35",
                                   margin = margin(b = 6)),
      plot.caption  = element_text(size = 8, color = "grey45", hjust = 0,
                                   margin = margin(t = 6)),
      legend.position   = "bottom",
      legend.title      = element_text(family = "Exo 2", size = 9,
                                       color = pal$navy),
      legend.text       = element_text(size = 8),
      plot.margin       = margin(12, 12, 12, 12)
    )
}


# ---- 3. LOAD CROSSWALK TABLES (from script 01) -------------------------------

occ_rollup_path <- file.path(config$data_dir, "occ_exposure_by_acs_group.csv")
ind_rollup_path <- file.path(config$data_dir, "ind_exposure_by_acs_group.csv")
gpkg_path       <- file.path(config$data_dir, "tract_occupational_exposure.gpkg")

if (!file.exists(occ_rollup_path) || !file.exists(ind_rollup_path) || !file.exists(gpkg_path)) {
  stop(
    "Output files from 01_occupational_exposure.R not found at:\n  ",
    config$data_dir,
    "\nPlease run that script first."
  )
}

occ_rollup <- readr::read_csv(occ_rollup_path, show_col_types = FALSE)
ind_rollup <- readr::read_csv(ind_rollup_path, show_col_types = FALSE)


# ---- 4. STUDY AREA DATA (from script 01 .gpkg) -------------------------------

study_exposure <- sf::st_read(gpkg_path, quiet = TRUE) |>
  dplyr::mutate(
    # Short name for labels: "Census Tract 40.01, Philadelphia..."  → "CT 40.01"
    tract_short = stringr::str_extract(NAME, "Census Tract [0-9.]+") |>
      stringr::str_replace("Census Tract ", "CT ")
  )


# ---- 5. PULL DP03 FOR ALL PHILADELPHIA TRACTS --------------------------------
# We re-derive occ/industry exposure for every Philly tract by applying the
# same national crosswalk tables from script 01.

message("Pulling DP03 for all Philadelphia tracts — may take a moment ...")

dp03_vars_meta <- tidycensus::load_variables(config$acs_year, "acs5/profile",
                                             cache = TRUE) |>
  dplyr::filter(grepl("^DP03_", name))

dp03_city_raw <- tidycensus::get_acs(
  geography   = "tract",
  state       = config$state,
  county      = config$county,
  table       = "DP03",
  year        = config$acs_year,
  survey      = config$acs_survey,
  geometry    = TRUE,
  cache_table = TRUE
) |>
  dplyr::left_join(dp03_vars_meta, by = c("variable" = "name")) |>
  # Drop percent variants
  dplyr::filter(!stringr::str_ends(variable, "P"))

dp03_city_parsed <- dp03_city_raw |>
  tidyr::separate(
    col    = label,
    into   = c("measure", "indicator", "metric", "d1", "d2", "d3", "d4"),
    sep    = "!!",
    remove = FALSE,
    fill   = "right"
  )

# 5a. City total civilian employed (denominator)
city_employed <- dp03_city_parsed |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    indicator == "EMPLOYMENT STATUS",
    stringr::str_detect(label, "Civilian labor force!!Employed$")
  ) |>
  dplyr::select(GEOID, total_employed = estimate)

# 5b. City occupation crosswalk
city_occ_summary <- dp03_city_parsed |>
  dplyr::filter(indicator == "OCCUPATION", !is.na(d1)) |>
  dplyr::select(GEOID, NAME, acs_occupation = d1, estimate) |>
  sf::st_drop_geometry() |>
  dplyr::left_join(occ_rollup, by = "acs_occupation") |>
  dplyr::left_join(city_employed, by = "GEOID") |>
  dplyr::group_by(GEOID, NAME, total_employed) |>
  dplyr::summarise(
    occ_heat_any_n      = sum(estimate * share_heat_any,     na.rm = TRUE),
    occ_heat_indoor_n   = sum(estimate * share_heat_indoor,  na.rm = TRUE),
    occ_heat_outdoor_n  = sum(estimate * share_heat_outdoor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    occ_heat_any_share     = occ_heat_any_n     / total_employed,
    occ_heat_indoor_share  = occ_heat_indoor_n  / total_employed,
    occ_heat_outdoor_share = occ_heat_outdoor_n / total_employed
  )

# 5c. City industry crosswalk
city_ind_summary <- dp03_city_parsed |>
  dplyr::filter(indicator == "INDUSTRY", !is.na(d1)) |>
  dplyr::select(GEOID, NAME, acs_industry = d1, estimate) |>
  sf::st_drop_geometry() |>
  dplyr::left_join(ind_rollup, by = "acs_industry") |>
  dplyr::left_join(city_employed, by = "GEOID") |>
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

# 5d. Combine with geometry
city_geom <- dp03_city_raw |>
  dplyr::group_by(GEOID) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(GEOID, NAME)

city_exposure <- city_geom |>
  dplyr::left_join(
    city_occ_summary |> dplyr::select(-NAME, -total_employed), by = "GEOID"
  ) |>
  dplyr::left_join(
    city_ind_summary |> dplyr::select(-NAME, -total_employed), by = "GEOID"
  ) |>
  dplyr::left_join(city_employed, by = "GEOID") |>
  dplyr::mutate(
    # Combined: average of occupation-side and industry-side any-exposure
    combined_exposure = (occ_heat_any_share + ind_heat_any_share) / 2
  )


# ---- 6. MAP HELPERS ----------------------------------------------------------

# City-scale choropleth with SEAMAAC study area outlined in navy
draw_city_map <- function(sf_df, fill_col, title, subtitle, caption,
                          pct_labels = TRUE) {
  study_outline <- sf_df |> dplyr::filter(GEOID %in% config$study_tracts)
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = sf_df,
      aes(fill = .data[[fill_col]]),
      color = "white", linewidth = 0.15
    ) +
    ggplot2::geom_sf(
      data  = study_outline,
      fill  = NA,
      color = pal$navy,
      linewidth = 1.0
    ) +
    ggplot2::scale_fill_gradientn(
      colors   = occ_gradient,
      name     = "Share of employed residents",
      labels   = scales::percent_format(accuracy = 1),
      na.value = "grey80",
      guide    = ggplot2::guide_colorbar(
        barwidth = 12, barheight = 0.7, title.position = "top"
      )
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      caption  = caption
    ) +
    hvi_theme()
  p
}

# Study-area choropleth with tract labels (CT number + value)
draw_study_map <- function(sf_df, fill_col, title, subtitle, caption,
                           pct_format = TRUE) {
  fmt_fn <- if (pct_format) scales::percent_format(accuracy = 1) else identity
  
  sf_labeled <- sf_df |>
    dplyr::mutate(
      lbl = paste0(tract_short, "\n", fmt_fn(.data[[fill_col]]))
    )
  
  ggplot2::ggplot(sf_labeled) +
    ggplot2::geom_sf(
      aes(fill = .data[[fill_col]]),
      color = "white", linewidth = 0.6
    ) +
    ggplot2::scale_fill_gradientn(
      colors   = occ_gradient,
      name     = "Share of employed residents",
      labels   = scales::percent_format(accuracy = 1),
      na.value = "grey80",
      guide    = ggplot2::guide_colorbar(
        barwidth = 10, barheight = 0.7, title.position = "top"
      )
    ) +
    ggrepel::geom_label_repel(
      aes(geometry = geometry, label = lbl),
      stat               = "sf_coordinates",
      size               = 2.8,
      fill               = "white",
      alpha              = 0.85,
      family             = "Open Sans",
      color              = pal$navy,
      label.size         = 0.2,
      min.segment.length = 0,
      max.overlaps       = Inf
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    hvi_theme()
}

save_map <- function(plot, filename, width = 9, height = 7, dpi = 150) {
  path <- file.path(config$output_dir, filename)
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi,
                  bg = "white")
  message(glue::glue("  ✓ {filename}"))
  invisible(path)
}


# ---- 7. PRODUCE MAPS ---------------------------------------------------------

message("\nSaving maps to: ", config$output_dir, "\n")

# ---- 7a. Occ Any — city
save_map(
  draw_city_map(
    city_exposure, "occ_heat_any_share",
    title    = "Occupational Heat Exposure — Philadelphia (2024 ACS 5-Year)",
    subtitle = "Share of employed residents in heat-exposed occupations",
    caption  = paste0(
      "Derived from O*NET 29.2 Work Context × ACS DP03 occupation groups following Laskaris (2025).\n",
      "Heat-exposed = indoor uncontrolled or outdoor score ≥ 75 on 0–100 frequency scale.\n",
      "Navy borders = SEAMAAC study area (8 tracts)."
    )
  ),
  "occ_any_city.png"
)

# ---- 7b. Occ Any — study area
save_map(
  draw_study_map(
    study_exposure, "occ_heat_any_share",
    title    = "Occupational Heat Exposure — SEAMAAC Study Area",
    subtitle = "Share of employed residents in heat-exposed occupations (any: indoor or outdoor)",
    caption  = paste0(
      "Derived from O*NET 29.2 Work Context × ACS DP03 occupation groups following Laskaris (2025).\n",
      "Heat-exposed = indoor uncontrolled or outdoor score ≥ 75 on 0–100 frequency scale."
    )
  ),
  "occ_any_study.png"
)

# ---- 7c. Occ Indoor — study area
save_map(
  draw_study_map(
    study_exposure, "occ_heat_indoor_share",
    title    = "Indoor Occupational Heat Exposure — SEAMAAC Study Area",
    subtitle = "Share of employed residents in indoor, non-climate-controlled occupations",
    caption  = paste0(
      "O*NET element 4.C.2.a.1.b ('Indoors, Not Environmentally Controlled') score ≥ 75.\n",
      "Captures manufacturing, food processing, laundry, and similar environments."
    )
  ),
  "occ_indoor_study.png"
)

# ---- 7d. Occ Outdoor — study area
save_map(
  draw_study_map(
    study_exposure, "occ_heat_outdoor_share",
    title    = "Outdoor Occupational Heat Exposure — SEAMAAC Study Area",
    subtitle = "Share of employed residents working outdoors (exposed or under cover)",
    caption  = paste0(
      "Max of O*NET elements 4.C.2.a.1.c (outdoors, exposed) and 4.C.2.a.1.d (outdoors, covered) ≥ 75.\n",
      "Captures construction, landscaping, delivery, agriculture, and similar occupations."
    )
  ),
  "occ_outdoor_study.png"
)

# ---- 7e. Industry Any — city
save_map(
  draw_city_map(
    city_exposure, "ind_heat_any_share",
    title    = "Industry Heat Exposure — Philadelphia (2024 ACS 5-Year)",
    subtitle = "Share of employed residents working in heat-exposed industries",
    caption  = paste0(
      "Derived from BLS OES industry-specific employment (NAICS-4) × ACS DP03 industry supersectors.\n",
      "Navy borders = SEAMAAC study area (8 tracts)."
    )
  ),
  "ind_any_city.png"
)

# ---- 7f. Industry Any — study area
save_map(
  draw_study_map(
    study_exposure, "ind_heat_any_share",
    title    = "Industry Heat Exposure — SEAMAAC Study Area",
    subtitle = "Share of employed residents working in heat-exposed industries",
    caption  = paste0(
      "Derived from BLS OES industry-specific employment (NAICS-4) × ACS DP03 industry supersectors.\n",
      "Industry heat exposure reflects the occupational mix within each industry, not just the industry label."
    )
  ),
  "ind_any_study.png"
)

# ---- 7g. Combined Occ + Industry — city
save_map(
  draw_city_map(
    city_exposure, "combined_exposure",
    title    = "Combined Occupational Heat Exposure — Philadelphia (2024 ACS 5-Year)",
    subtitle = "Average of occupation-side and industry-side heat exposure shares",
    caption  = paste0(
      "Combines two independent exposure pathways: ACS occupation-group rollup and ",
      "ACS industry-supersector rollup.\n",
      "Both derived from O*NET Work Context + BLS OES national employment data.\n",
      "Navy borders = SEAMAAC study area (8 tracts)."
    )
  ),
  "occ_combined_city.png"
)

# ---- 7h. Combined Occ + Industry — study area
# Use the city_exposure table for the study tracts (includes combined_exposure)
# so the scale is consistent with the city map.
study_combined <- city_exposure |>
  dplyr::filter(GEOID %in% config$study_tracts) |>
  dplyr::mutate(
    tract_short = stringr::str_extract(NAME, "Census Tract [0-9.]+") |>
      stringr::str_replace("Census Tract ", "CT ")
  )

save_map(
  draw_study_map(
    study_combined, "combined_exposure",
    title    = "Combined Occupational Heat Exposure — SEAMAAC Study Area",
    subtitle = "Average of occupation-side and industry-side heat exposure shares",
    caption  = paste0(
      "Combines two independent exposure pathways: ACS occupation-group rollup and ",
      "ACS industry-supersector rollup.\n",
      "Both derived from O*NET Work Context + BLS OES national employment data."
    )
  ),
  "occ_combined_study.png"
)

message("\nAll 8 occupational exposure maps saved to:\n  ", config$output_dir)