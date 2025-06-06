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
setwd(getwd())
if (!dir.exists("Daymet")) {dir.create("Daymet")}
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
df$CZ_NAME


#SCRAPPING PART TAKES SOME TIME
# Uses ncss server
base_url <- "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/2129"

# Originally used multiple dates but this should be fine
for (i in 1:length(df$CZ_NAME)) {
  date = (df$BEGIN_YEARMONTH[i] * 100) + df$BEGIN_DAY[i]
  # Splitting date into usable chunks for link
  year = substr(date, start = 1, stop = 4)
  month = substr(date, start = 5, stop = 6)
  day = substr(date, start = 7, stop = 8)
  earlyDate = as.Date(paste0(year, "-", month, "-", day)) - 2
  earlyyear = substr(earlyDate, start = 1, stop = 4)
  earlymonth = substr(earlyDate, start = 6, stop = 7)
  earlyday = substr(earlyDate, start = 9, stop = 10)
  
  co_shape <- county_subdivisions(state = df$STATE[i], county = df$CZ_NAME[i], cb = TRUE, class = "sf")
  bbox <- st_bbox(co_shape$geometry[1])
  nBound = bbox[4] + 0.000492857143
  wBound = bbox[1] - 0.000492857143
  sBound = bbox[2] - 0.000492857143
  eBound = bbox[3] + 0.000492857143
  
  # Have some funny link documentation =w= https://github.com/ornldaac/gridded_subset_example_script
  # This link is why I cut up all those strings and dates
  dest <- file.path("Daymet", paste0("daymet_prcp_", date, "_", df$STATE[i], "_", df$CZ_NAME[i], ".nc"))
  
  if (!file.exists(dest)) {
    url <- paste0(base_url, "/daymet_v4_daily_na_prcp_", year, ".nc?var=lat&var=lon&var=prcp&north=", 
                  nBound, "&west=", wBound, "&east=", eBound, "&south=", sBound,
                  "&disableProjSubset=on&horizStride=1&time_start=", earlyyear, "-", 
                  earlymonth, "-", earlyday, "T00:00:00Z&time_end=", year, "-", month, "-", 
                  day, "T23:59:59Z&timeStride=1")
    
    message("Downloading: ", url)
    GET(url, write_disk(dest, overwrite = TRUE), progress())
  } 
  {
    message("File already exists: ", dest)
  }
}

output <- data.frame(matrix(ncol = 12, nrow = 0))

# Provide column names
colnames(output) <- c("State", "County", "Date", "min-2", "max-2", "mean-2", "min-1", "max-1", "mean-1", "min", "max", "mean")

# Calculate
for (i in 1:length(df$CZ_NAME)) {
  co_shape <- county_subdivisions(state = df$STATE[i], county = df$CZ_NAME[i], cb = TRUE, class = "sf")
  bbox <- st_bbox(co_shape$geometry[1])
  if (is.na(bbox[1]) + is.na(bbox[2]) + is.na(bbox[3]) + is.na(bbox[4]) == 0) {
    date = (df$BEGIN_YEARMONTH[i] * 100) + df$BEGIN_DAY[i]
    # Splitting date into usable chunks for link
    year = substr(date, start = 1, stop = 4)
    month = substr(date, start = 5, stop = 6)
    day = substr(date, start = 7, stop = 8)
    earlyDate = as.Date(paste0(year, "-", month, "-", day)) - 2
    earlyyear = substr(earlyDate, start = 1, stop = 4)
    earlymonth = substr(earlyDate, start = 6, stop = 7)
    earlyday = substr(earlyDate, start = 9, stop = 10)
    
    # Load Daymet raster
    raster_file <- paste0("Daymet/daymet_prcp_", date, "_", df$STATE[i], "_", df$CZ_NAME[i], ".nc")
    r <- rast(raster_file)
    
    # Get VA boundary and reproject to match raster CRS
    co_shape_proj <- st_transform(co_shape, crs(r))
    co_vect <- vect(co_shape_proj)
    
    # Clip raster to VA
    try(r_co <- crop(r, co_vect) |> mask(co_vect), silent = TRUE)
    
    # Compute min, max, and mean for each day
    stats <- global(r_co, fun = c("min", "max", "mean"), na.rm = TRUE)
    
    
    # Use the transposed matrix directly
    observed_matrix <-as.data.frame(stats)
    
    # output is in form min, max, mean
    output <- rbind(output, 
                    data.frame(State = df$STATE[i], 
                               County = df$CZ_NAME[i], 
                               Date = (earlyDate + 2), 
                               "min-2" = observed_matrix[1, "min"], 
                               "max-2" = observed_matrix[1, "max"], 
                               "mean-2" = observed_matrix[1, "mean"], 
                               "min-1" = observed_matrix[2, "min"], 
                               "max-1" = observed_matrix[2, "max"], 
                               "mean-1" = observed_matrix[2, "mean"], 
                               "min" = observed_matrix[3, "min"], 
                               "max" = observed_matrix[3, "max"], 
                               "mean" = observed_matrix[3, "mean"]))
  }
  else {
    output <- rbind(output, 
                    data.frame(State = df$STATE[i], 
                               County = df$CZ_NAME[i], 
                               Date = (earlyDate + 2), 
                               "min-2" = NA, 
                               "max-2" = NA, 
                               "mean-2" = NA, 
                               "min-1" = NA, 
                               "max-1" = NA, 
                               "mean-1" = NA, 
                               "min" = NA, 
                               "max" = NA, 
                               "mean" = NA))
    message("Geometry does not exist")
  }
}

write.csv(output, "daymetOutput.csv")



