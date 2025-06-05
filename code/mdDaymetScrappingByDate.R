# Load required libraries
library(terra)    # For raster data processing
library(sf)       # For spatial vector data
library(tigris)   # For downloading US county boundaries
library(httr)     # For HTTP requests (downloading files)
library(lubridate) # For date manipulation

# Enable caching of tigris downloads to avoid repeated downloads
options(tigris_use_cache = TRUE)

# Set the target date for analysis
currDate = "2020-04-30"

# Base URL for Daymet precipitation data
base_url <- "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/2129"

# Download data for the specified date range --------------------------------
for (date in currDate) {
  # Extract year, month, day from current date
  year = substr(date, start = 1, stop = 4)
  month = substr(date, start = 6, stop = 7)
  day = substr(date, start = 9, stop = 10)
  
  # Calculate date 3 days before current date
  earlyDate = as.Date(date) - 3
  earlyyear = substr(earlyDate, start = 1, stop = 4)
  earlymonth = substr(earlyDate, start = 6, stop = 7)
  earlyday = substr(earlyDate, start = 9, stop = 10)
  
  # Construct the download URL with parameters:
  # - Bounding box for Maryland
  # - Time range (3 days before to current date)
  # - Precipitation variable (prcp)
  url <- paste0(base_url, "/daymet_v4_daily_na_prcp_", year, 
                ".nc?var=lat&var=lon&var=prcp",
                "&north=39.72284225191251&west=-79.48700299202991",
                "&east=-75.05063561675617&south=37.91709878227782",
                "&disableProjSubset=on&horizStride=1",
                "&time_start=", earlyyear, "-", earlymonth, "-", earlyday, "T00:00:00Z",
                "&time_end=", year, "-", month, "-", day, "T23:59:59Z&timeStride=1")
  
  # Destination path for downloaded file
  dest <- file.path("/Users/oqueivehorne/Downloads/Daymet", 
                    paste0("daymet_prcp_", date, ".nc"))
  
  # Download the file with progress indicator
  message("Downloading: ", url)
  GET(url, write_disk(dest, overwrite = TRUE), progress())
}

# Process the downloaded data ----------------------------------------------
for (date in currDate) {
  # Re-extract date components (could be stored from previous loop)
  year = substr(date, start = 1, stop = 4)
  month = substr(date, start = 6, stop = 7)
  day = substr(date, start = 9, stop = 10)
  earlyDate = as.Date(date) - 3
  
  # Load the downloaded NetCDF file as a raster
  r <- rast(paste0("Daymet/daymet_prcp_", date, ".nc"))
  
  # Get Maryland county boundaries and reproject to match raster CRS
  md <- counties(state = "MD", cb = TRUE, class = "sf")
  md_proj <- st_transform(md, crs(r))
  
  # Clip and mask the raster to Maryland boundaries
  r_clipped <- mask(crop(r, vect(md_proj)), vect(md_proj))
  
  # Plot mean precipitation for the date range
  plot(mean(r_clipped), 
       main = paste0("Mean Daily Precip – ", earlyDate, " - ", date, " (MD)"), 
       col = terrain.colors(20))
  
  # Calculate statistics ---------------------------------------------------
  # Reload the raster (redundant - could use r_clipped from above)
  raster_file <- paste0("Daymet/daymet_prcp_", date, ".nc")
  r <- rast(raster_file)
  
  # Get MD boundaries again (redundant - could reuse md_proj)
  md_shape <- counties(state = "MD", cb = TRUE, class = "sf")
  md_shape_proj <- st_transform(md_shape, crs(r))
  md_vect <- vect(md_shape_proj)
  
  # Clip the raster to MD boundaries
  r_md <- crop(r, md_vect) |> mask(md_vect)
  
  # Calculate min, max, and mean precipitation for each day
  stats <- global(r_md, fun = c("min", "max", "mean"), na.rm = TRUE)
  
  # Convert statistics to data frame
  observed_matrix <- as.data.frame(stats)
  
  # Create a clean data frame with proper dates and statistics
  stats_df_clean <- data.frame(
    Date = seq(from = earlyDate, to = as.Date(currDate), by = "day"),
    Min = observed_matrix[, "min"],
    Max = observed_matrix[, "max"],
    Mean = observed_matrix[, "mean"]
  )
  
  # Print the results
  print(stats_df_clean)
}




