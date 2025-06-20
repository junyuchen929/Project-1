rm(list = ls())
setwd("/Users/beauniverse/Desktop/project 1/data")
library(raster)
library(sf)
library(dplyr)
library(parallel)
library(tigris)
library(tidyr)
library(readr)
library(tidycensus)
library(stringr)
library(purrr)

census_api_key("071477ea86c9f788dd3d8fb07cabb331fbc41221") 

# 1. population & income
clean_county_name <- function(name) {
  name %>%
    str_remove(regex(",\\s*(Virginia|Maryland|District of Columbia)$", ignore_case = TRUE)) %>%
    str_to_title() %>%
    str_trim()
}

get_demographics <- function(state, year) {
  # 1. B01003_001: Total population
  # 2. B19013_001: MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTH
  acs_data <- get_acs(
    geography = "county",
    state = state,
    variables = c(total_pop = "B01003_001", median_income = "B19013_001"),
    year = year,
    geometry = FALSE
  ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    tidyr::pivot_wider(names_from = variable, values_from = estimate)
  
  # 3. geo info for population density
  geo_data <- get_acs(
    geography = "county",
    state = state,
    variables = c(pop = "B01003_001"),
    year = year,
    geometry = TRUE
  ) %>%
    st_transform(crs = 32618)  
  
  geo_data$area_km2 <- as.numeric(st_area(geo_data)) / 10^6
  geo_data$pop_density <- geo_data$estimate / geo_data$area_km2

  result <- geo_data %>%
    st_drop_geometry() %>%
    select(GEOID, county = NAME, area_km2, pop_density) %>%
    left_join(acs_data, by = c("GEOID", "county" = "NAME")) %>%
    mutate(county = clean_county_name(county)) %>%
    select(GEOID, county, total_pop, median_income, area_km2, pop_density)
  
  return(result)
}


states <- c("DC", "VA", "MD")
demographics_all_2010 <- purrr::map_dfr(states, function(st) {
  df <- get_demographics(st, 2010)
  df$state <- st
  return(df)
})
demographics_all_2020 <- purrr::map_dfr(states, function(st) {
  df <- get_demographics(st, 2020)
  df$state <- st
  return(df)
})

write_csv(demographics_all_2010, "demographics_all_2010.csv")
write_csv(demographics_all_2020, "demographics_all_2020.csv")


# 2. Exposed population

cz_to_name_lsad <- function(cz_name_vec) {
  cz_name_vec <- toupper(cz_name_vec)
  
  final <- dplyr::case_when(
    cz_name_vec == "DISTRICT OF COLUMBIA" ~ "District of Columbia",
    str_detect(cz_name_vec, "\\(C\\)") ~ paste0(str_to_title(str_replace(cz_name_vec, " \\(C\\)", "")), " city"),
    str_detect(cz_name_vec, "CITY$") ~ str_to_title(cz_name_vec),
    TRUE ~ paste0(str_to_title(cz_name_vec), " County")
  )
  
  return(final)
}


estimate_exposed_population <- function(state_abbr, year) {

  if (state_abbr == "VA") {
    state_full_name <- "VIRGINIA"
  } else if (state_abbr == "MD") {
    state_full_name <- "MARYLAND"
  } else if (state_abbr == "DC") {
    state_full_name <- "DISTRICT OF COLUMBIA"
  } else {
    stop("Unsupported state abbreviation.")
  }
  
  
  # Read and filter flood data for the state
  flood_df <- read_csv("dc_md_va_flash_floods_1996_present.csv") %>%
    filter(STATE == state_full_name, !is.na(BEGIN_LAT), !is.na(BEGIN_LON)) %>%
    mutate(NAME_LSAD = cz_to_name_lsad(CZ_NAME))
  
  # unique(flood_df$NAME_LSAD)
  
  
  
  # Get ACS population data with geometry for specified state and year
  pop_data <- get_acs(
    geography = "county", 
    state = state_abbr, 
    variables = c(pop = "B01003_001"), 
    year = year, 
    geometry = TRUE
  )
  
  # Project to UTM Zone 18N for spatial operations
  pop_data <- st_transform(pop_data, crs = 32618)
  pop_data$area_km2 <- st_area(pop_data) / 10^6  # County area in km²
  
  exposed_summary <- data.frame()
  
  clean_county_name <- function(name) {
    name %>%
      str_remove(",\\s*(Virginia|Maryland|District of Columbia)$") %>%
      str_to_title() %>%
      str_trim()
  }
  
  # Iterate through each county to calculate flood-exposed population
  for (i in 1:nrow(pop_data)) {
    county_geom <- pop_data[i, ]
    county_name_clean <- clean_county_name(county_geom$NAME)
    county_geoid <- county_geom$GEOID
    total_pop <- county_geom$estimate
    county_area <- county_geom$area_km2
    
    # Match flood points within the county
    floods_in_county_df <- flood_df %>%
      filter(toupper(NAME_LSAD) == toupper(county_name_clean))
    
    message(paste0("→ ", county_name_clean, ": ", nrow(floods_in_county_df), " flood points"))
    
    if (nrow(floods_in_county_df) == 0) {
      exposed_pop <- 0
    } else {
      # Convert to sf and apply 1km buffer
      floods_in_county <- st_as_sf(floods_in_county_df, coords = c("BEGIN_LON", "BEGIN_LAT"), crs = 4326) %>%
        st_transform(crs = 32618)
      
      flood_zone <- st_union(st_buffer(floods_in_county, dist = 1000))  # 1 km buffer
      
      # Compute spatial intersection
      exposed_region <- st_intersection(county_geom, flood_zone)
      
      if (nrow(exposed_region) == 0) {
        exposed_pop <- 0
      } else {
        intersect_area <- st_area(exposed_region) / 10^6  # km²
        exposed_pop <- total_pop * as.numeric(intersect_area / county_area)
      }
    }
    
    exposed_summary <- rbind(exposed_summary, data.frame(
      GEOID = county_geoid,
      county = county_name_clean,
      total_population = round(total_pop, 0),
      exposed_population = round(exposed_pop, 0),
      pct_exposed = round(as.numeric(exposed_pop / total_pop) * 100, 2)
    ))
  }
  
  return(exposed_summary)
}

states <- c("DC", "VA", "MD")

exposed_all_2010 <- purrr::map_dfr(states, function(st) {
  df <- estimate_exposed_population(st, 2010)
  df$state <- st
  return(df)
})

exposed_all_2020 <- purrr::map_dfr(states, function(st) {
  df <- estimate_exposed_population(st, 2020)
  df$state <- st
  return(df)
})

write_csv(exposed_all_2010, "exposed_population_2010.csv")
write_csv(exposed_all_2020, "exposed_population_2020.csv")


# 3. added socio demographics

get_socio_demographics <- function(state, year) {
  age65_vars <- c(
    paste0("B01001_0", 20:25),  # Male 65+
    paste0("B01001_0", 44:49)   # Female 65+
  )
  age65_named <- setNames(age65_vars, paste0("age65_", seq_along(age65_vars)))
  
  education_vars <- c(
    edu_less_hs = "B06009_002"
  )
  
  poverty_vars <- c(
    below_poverty = "B17001_002"
  )
  
  race_vars <- c(
    hispanic = "B03003_003",
    nh_black = "B03002_004"
  )
  
  all_vars <- c(
    total_pop = "B01003_001",
    age65_named,
    education_vars,
    poverty_vars,
    race_vars
  )
  
  acs_data <- get_acs(
    geography = "county",
    state = state,
    variables = all_vars,
    year = year,
    geometry = FALSE
  )
  
  acs_summary <- acs_data %>%
    select(GEOID, NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(
      county = clean_county_name(NAME),
      pop_65plus = rowSums(across(starts_with("age65_"))),
      pct_age_65plus = 100 * pop_65plus / total_pop,
      pct_less_hs = 100 * edu_less_hs / total_pop,
      pct_below_poverty = 100 * below_poverty / total_pop,
      pct_hispanic = 100 * hispanic / total_pop,
      pct_nh_black = 100 * nh_black / total_pop
    ) %>%
    select(GEOID, county, total_pop,
           pct_age_65plus, pct_less_hs, pct_below_poverty,
           pct_hispanic, pct_nh_black)
  
  return(acs_summary)
}

states <- c("DC", "VA", "MD")

socio_2023 <- purrr::map_dfr(states, function(st) {
  df <- get_socio_demographics(st, 2023)
  df$state <- st
  return(df)
})

write_csv(socio_2023, "socio_2023.csv")

