install.packages(c("terra", "sf", "tigris", "httr", "stringi", "lubridate"))
library(terra)
library(sf)
library(tigris)
library(httr)
library(stringi)
library(lubridate)
library(readr)
library(dplyr)
library(stringr)

#Lets let the file download slap it in the same file
for(i in 1:1) {
  setwd(getwd())
  if (!dir.exists("NLCD")) {dir.create("NLCD")}
  options(tigris_use_cache = TRUE)
  
  df <- read_csv("dc_md_va_flash_floods_1996_present.csv")
  
  df <- df %>% 
    filter(str_starts(as.character(BEGIN_YEARMONTH), "2024", negate = TRUE)) %>%
    filter(str_starts(as.character(BEGIN_YEARMONTH), "2025", negate = TRUE))
  
  # Step 1: Identify non-(C) base names
  non_c_names <- df %>%
    filter(!str_ends(CZ_NAME, " \\(C\\)")) %>%
    pull(CZ_NAME)
  non_c_names <- c(non_c_names, "MANASSAS")
  
  # Step 2: Apply transformation with special case
  df <- df %>%
    mutate(CZ_NAME = case_when(
      CZ_NAME %in% c("DISTRICT OF COLUMBIA", "SALEM", "EMPORIA", "POQUOSON") ~ CZ_NAME,
      str_ends(CZ_NAME, " \\(C\\)") ~ {
        base <- str_remove(CZ_NAME, " \\(C\\)$")
        ifelse(base %in% non_c_names, paste0(base, " City"), base)
      },
      str_ends(CZ_NAME, "County") ~ CZ_NAME,
      TRUE ~ paste0(CZ_NAME, " County")
    ))
  states <- unique(df$STATE)
  years <- 1996:2023
}



#SCRAPPING PART TAKES SOME TIME
# Uses ncss server
base_url <- "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_LndCov_"

# Originally used multiple dates but this should be fine
for (year in years) {
  dest <- file.path("NLCD", paste0("Annual_NLCD_LndCov_", year, "_CU_C1V0.tif"))
  
  if (!file.exists(dest)) {
    url <- paste0(base_url, year, "_CU_C1V0.tif")
    
    message("Downloading: ", url)
    GET(url, write_disk(dest, overwrite = TRUE), progress())
  } 
  {
    message("File already exists: ", dest)
  }
}



#Setup
for(i in 1:1) {
  output <- data.frame(matrix(ncol = 8, nrow = 0))
  
  # Provide column names
  colnames(output) <- c("State", "County", "Year", "Forest", "Agriculture", "Urban", "Suburban", "Rural")
  
  features <- list(
    forest = c(41, 42, 43, 90),
    agriculture = c(81, 82),
    urban = c(24),
    suburban = c(23, 22),
    rural = c(21)
    )
}

for(year in years) {
  r <- rast(paste0("NLCD/Annual_NLCD_LndCov_", year, "_CU_C1V0.tif"))
  for (inpState in states) {
    stateDF = df %>% filter(STATE == inpState)
    for (inpCounty in unique(stateDF$CZ_NAME)) {
      co_shape <- county_subdivisions(state = inpState, county = inpCounty, cb = TRUE, class = "sf")
      bbox <- st_bbox(co_shape$geometry[1])
      if (is.na(bbox[1]) + is.na(bbox[2]) + is.na(bbox[3]) + is.na(bbox[4]) == 0) {
        featOut <- data.frame(matrix(ncol = 5, nrow = 1))
        
        # Provide column names
        colnames(featOut) <- c("forest", "agriculture", "urban", "suburban", "rural")
        
        co_shape_proj <- st_transform(co_shape, crs(r))
        co_vect <- vect(co_shape_proj)
        
        # Clip raster to VA
        try(r_co <- crop(r, co_vect) |> mask(co_vect), silent = TRUE)
        i = 0
        for(feature in features) {
          i <- i + 1
          target_values <- feature
          
          # Get all raster values
          vals <- values(r_co)
          
          # Count how many are 41, 42, or 43 (excluding NAs)
          match_count <- sum(vals %in% target_values, na.rm = TRUE)
          
          # Count total number of non-NA cells
          total_count <- sum(!is.na(vals))
          
          # Calculate percentage
          percent <- (match_count / total_count) * 100
          featOut[[names(features[i])]] <- round(percent, 3)
        }
        output <- rbind(output, 
                        data.frame("State" = inpState, 
                                   "County" = inpCounty, 
                                   "Year" = year, 
                                   "Forest" = featOut$forest, 
                                   "Agriculture" = featOut$agriculture, 
                                   "Urban" = featOut$urban, 
                                   "Suburban" = featOut$suburban, 
                                   "Rural" = featOut$rural))
      }
      else {
        output <- rbind(output, 
                        data.frame("State" = inpState, 
                                   "County" = inpCounty, 
                                   "Year" = year, 
                                   "Forest" = NA, 
                                   "Agriculture" = NA, 
                                   "Urban" = NA, 
                                   "Suburban" = NA, 
                                   "Rural" = NA))
        message("Geometry does not exist")
        }
      }
  }
}

write.csv(output, "NLCDOutput.csv")



