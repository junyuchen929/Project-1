rm(list = ls())
setwd("/Users/beauniverse/Desktop/project 1/data")
library(raster)
library(sf)
library(dplyr)
library(parallel)
library(tigris)
library(tidyr)
library(readr)

# 1. NLCD Land types
extract_landcover_summary <- function(nlcd_path, year, states = c("DC", "MD", "VA")) {
  # nlcd_path = "Annual_NLCD_LndCov_2020_CU_C1V0.tif"
  # year = 2020
  
  # Step 1: Load NLCD raster
  nlcd_raster <- raster(nlcd_path)
  
  # Step 2: Get county boundaries and match CRS
  counties <- counties(state = states, year = year, cb = TRUE)
  counties <- st_transform(counties, crs = crs(nlcd_raster))
  
  # Step 3: Extract LSAD_TRANS info before sf loses it in parallel processing
  county_info <- counties %>%
    st_drop_geometry() %>%
    select(NAME, STATEFP, LSAD) %>% # little difference***
    rename(county = NAME)
  
  # Step 4: Define helper to extract land cover percentage from one county
  extract_land_cover_pct <- function(county_geom, raster_layer) {
    values <- raster::extract(raster_layer, county_geom)[[1]]
    values <- values[!is.na(values)]
    if (length(values) == 0) return(data.frame())
    
    total <- length(values)
    tab <- table(values)
    pct <- round(100 * as.numeric(tab) / total, 2)
    landcover <- as.integer(names(tab))
    
    data.frame(landcover_class = landcover, percent = pct)
  }
  
  # Step 5: Parallel processing for all counties
  process_one_county <- function(i) {
    county_name <- counties$NAME[i]
    STATEFP <- counties$STATEFP[i]
    county_geom <- counties[i, ]
    
    lc_pct <- extract_land_cover_pct(county_geom, nlcd_raster)
    if (nrow(lc_pct) == 0) return(NULL)
    
    lc_pct$county <- county_name
    lc_pct$STATEFP <- STATEFP
    return(lc_pct)
  }
  
  num_cores <- detectCores() - 1
  results <- mclapply(1:nrow(counties), process_one_county, mc.cores = num_cores)
  
  landcover_summary <- do.call(rbind, results)
  
  # Step 6: Add LSAD_TRANS
  landcover_summary <- landcover_summary %>%
    left_join(county_info, by = c("STATEFP", "county"))
  
  # Step 7: Aggregate by land use category
  landcover_summary <- landcover_summary %>%
    mutate(category = case_when(
      landcover_class %in% 21:24 ~ "builtup",
      landcover_class %in% 41:43 ~ "forest",
      landcover_class %in% 81:82 ~ "agriculture",
      TRUE ~ "other"
    )) %>%
    group_by(STATEFP, county, LSAD, category) %>% # ***
    summarise(percent = sum(percent), .groups = "drop") %>%
    mutate(year = year)
  
  return(landcover_summary)
}



# Combine three years of land cover summaries
lc_2000 <- extract_landcover_summary("Annual_NLCD_LndCov_2000_CU_C1V0.tif", year = 2000)
# df <- df %>%
#   rename(state = STATEFP)

lc_2010 <- extract_landcover_summary("Annual_NLCD_LndCov_2010_CU_C1V0.tif", year = 2010)
lc_2020 <- extract_landcover_summary("Annual_NLCD_LndCov_2020_CU_C1V0.tif", year = 2020)


# 2000: LSAD_TRANS, 2010: LSAD, 2020: NAMELSAD
# 2. Majority type
prepare_landcover_wide <- function(df) {
  
  state_lookup <- tibble::tibble(
    STATEFP = c("11", "24", "51"),
    STATE_NAME = c("District of Columbia", "Maryland", "Virginia")
  )
  
  # add Majority type
  df_majority <- df %>%
    group_by(STATEFP, county) %>%
    mutate(majority_type = category[which.max(percent)]) %>%
    ungroup()
  
  # # merge name
  # df_majority <- df_majority %>%
  #   left_join(state_lookup, by = "STATEFP") %>%
  #   mutate(
  #     NAME_LSAD = if_else(
  #       is.na(LSAD),
  #       county,
  #       paste0(county, " ", LSAD)
  #     )
  #   )
  df_majority <- df_majority %>%
    left_join(state_lookup, by = "STATEFP") %>%
    rename(NAME_LSAD = NAMELSAD)
  
  # transfer to wide data
  df_wide <- df_majority %>%
    pivot_wider(
      names_from = category,
      values_from = percent
    ) %>%
    select(STATE_NAME, NAME_LSAD, county, majority_type, year, builtup, forest, agriculture, other)
  
  return(df_wide)
}

lc_2000_wide <- prepare_landcover_wide(lc_2000)
lc_2010_wide <- prepare_landcover_wide(lc_2010)
lc_2020_wide <- prepare_landcover_wide(lc_2020)


write_csv(lc_2000_wide, "landcover_2000_wide.csv")
write_csv(lc_2010_wide, "landcover_2010_wide.csv")
write_csv(lc_2020_wide, "landcover_2020_wide.csv")


# 3. Impervious surface
extract_impervious_summary <- function(imperv_path, year, states = c("DC", "MD", "VA")) {
  # imperv_path = "Annual_NLCD_FctImp_2020_CU_C1V0.tif"
  # year = 2020
  # states = c("DC", "MD", "VA")

  # Step 1: Load raster
  imperv_raster <- raster(imperv_path)
  
  # Step 2: Load and transform counties
  counties <- counties(state = states, cb = TRUE, year = year)
  counties <- st_transform(counties, crs = crs(imperv_raster))
  
  county_info <- counties %>%
    st_drop_geometry() %>%
    select(STATEFP, NAME, NAMELSAD) %>% # **
    rename(county = NAME)
  
  # Step 3: Extract mean impervious % per county
  process_one_county <- function(i) {
    county_name <- counties$NAME[i]
    STATEFP <- counties$STATEFP[i]
    county_geom <- counties[i, ]
    
    values <- raster::extract(imperv_raster, county_geom)[[1]]
    values <- values[!is.na(values)]
    if (length(values) == 0) return(NULL)
    
    mean_imperv <- round(mean(values), 2)
    
    data.frame(
      STATEFP = STATEFP,
      county = county_name,
      impervious = mean_imperv,
      year = year
    )
  }
  
  results <- mclapply(1:nrow(counties), process_one_county, mc.cores = detectCores() - 1)
  imperv_summary <- do.call(rbind, results)
  
  imperv_summary <- imperv_summary %>%
    left_join(county_info, by = c("STATEFP", "county"))
    # left_join(county_info, by = c("STATEFP", "county")) %>%
    # mutate(
    #   NAME_LSAD = if_else(
    #     is.na(NAMELSAD), # **
    #     county,
    #     paste0(county, " ", NAMELSAD)
    #   )
    # )
  return(imperv_summary)
}

imp_2000 <- extract_impervious_summary("Annual_NLCD_FctImp_2000_CU_C1V0.tif", year = 2000)
imp_2010 <- extract_impervious_summary("Annual_NLCD_FctImp_2010_CU_C1V0.tif", year = 2010)
imp_2020 <- extract_impervious_summary("Annual_NLCD_FctImp_2020_CU_C1V0.tif", year = 2020)

prepare_impervious_summary_by_year <- function(df, year) {
  df_prepared <- df %>%
    mutate(STATE_NAME = case_when(
      STATEFP == "11" ~ "District of Columbia",
      STATEFP == "24" ~ "Maryland",
      STATEFP == "51" ~ "Virginia",
      TRUE ~ NA_character_
    )) %>%
    rename(NAME_LSAD = NAMELSAD) %>%
    select(STATE_NAME, NAME_LSAD, county, year, impervious)
  
  return(df_prepared)
}

imp_2000_prepared <- prepare_impervious_summary_by_year(imp_2000, 2000)
imp_2010_prepared <- prepare_impervious_summary_by_year(imp_2010, 2010)
imp_2020_prepared <- prepare_impervious_summary_by_year(imp_2020, 2020)

write_csv(imp_2000_prepared, "impervious_2000.csv")
write_csv(imp_2010_prepared, "impervious_2010.csv")
write_csv(imp_2020_prepared, "impervious_2020.csv")


