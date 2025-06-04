library(terra)
library(sf)
library(tigris)
library(httr)

library(lubridate)
options(tigris_use_cache = TRUE)

currDate = "2020-04-30"

#SCRAPPING PART TAKES SOME TIME
base_url <- "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/2129"

for (date in currDate) {
  year = substr(date, start = 1, stop = 4)
  month = substr(date, start = 6, stop = 7)
  day = substr(date, start = 9, stop = 10)
  earlyDate = as.Date(date) - 3
  earlyyear = substr(earlyDate, start = 1, stop = 4)
  earlymonth = substr(earlyDate, start = 6, stop = 7)
  earlyday = substr(earlyDate, start = 9, stop = 10)
  url <- paste0(base_url, "/daymet_v4_daily_na_prcp_", year, ".nc?var=lat&var=lon&var=prcp&north=39.46598340442404&west=-83.67570908179081&east=-75.24086819838197&south=36.540885157941574&disableProjSubset=on&horizStride=1&time_start=", earlyyear, "-", earlymonth, "-", earlyday, "T00:00:00Z&time_end=", year, "-", month, "-", day, "T23:59:59Z&timeStride=1")
  dest <- file.path("/Users/wetfr/Downloads/Daymet", paste0("daymet_prcp_", date, ".nc"))
  
  message("Downloading: ", url)
  GET(url, write_disk(dest, overwrite = TRUE), progress())
}



# Example for one year:
for (date in currDate) {
  year = substr(date, start = 1, stop = 4)
  month = substr(date, start = 6, stop = 7)
  day = substr(date, start = 9, stop = 10)
  earlyDate = as.Date(date) - 3
  endyear = substr(earlyDate, start = 1, stop = 4)
  endmonth = substr(earlyDate, start = 6, stop = 7)
  endday = substr(earlyDate, start = 9, stop = 10)
  
  r <- rast(paste0("Daymet/daymet_prcp_", date, ".nc"))
  
  # Load and reproject DC boundary
  va <- counties(state = "VA", cb = TRUE, class = "sf")
  va_proj <- st_transform(va, crs(r))
  
  # Crop + mask to DC
  r_clipped <- mask(crop(r, vect(va_proj)), vect(va_proj))
  
  # nah
  r_jan <- r_clipped
  
  # Plot mean daily precip for January
  plot(mean(r_jan), main = paste0("Mean Daily Precip – ", earlyDate, " - ", date, " (VA)"), col = terrain.colors(20))
  
  ##########
  
  # Load Daymet raster
  raster_file <- paste0("Daymet/daymet_prcp_", date, ".nc")
  r <- rast(raster_file)
  
  #  nah
  r_days <- r
  
  # Get VA boundary and reproject to match raster CRS
  va_shape <- counties(state = "VA", cb = TRUE, class = "sf")
  va_shape_proj <- st_transform(va_shape, crs(r_days))
  va_vect <- vect(va_shape_proj)
  
  # Clip raster to VA
  r_va <- crop(r_days, va_vect) |> mask(va_vect)
  
  # Compute min, max, and mean for each day
  stats <- global(r_va, fun = c("min", "max", "mean"), na.rm = TRUE)
  
  
  # Use the transposed matrix directly
  observed_matrix <-as.data.frame(stats)
  
  # Create a clean data frame with proper date labels
  stats_df_clean <- data.frame(
    Date = seq(from = earlyDate, to = as.Date(currDate), by = "day"),
    Min = observed_matrix[, "min"],
    Max = observed_matrix[, "max"],
    Mean = observed_matrix[, "mean"]
  )
  
  # View the result
  print(stats_df_clean)
}





