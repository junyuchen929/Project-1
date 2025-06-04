library(terra)
library(sf)
library(tigris)
library(httr)

library(lubridate)
options(tigris_use_cache = TRUE)

#SCRAPPING PART TAKES SOME TIME
years <- 2020:2023
base_url <- "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/2129"

for (year in years) {
  url <- paste0(base_url, "/daymet_v4_daily_na_prcp_", year, ".nc?var=lat&var=lon&var=prcp&north=39.46598340442404&west=-83.67570908179081&east=-75.24086819838197&south=36.540885157941574&disableProjSubset=on&horizStride=1&time_start=", year, "-01-01T00:00:00Z&time_end=", year, "-01-31T23:59:59Z&timeStride=1")
  dest <- file.path("/Users/wetfr/Downloads/Daymet", paste0("daymet_prcp_", year, ".nc"))
  
  message("Downloading: ", url)
  GET(url, write_disk(dest, overwrite = TRUE), progress())
}



# Example for one year:
for (year in years) {
  r <- rast(paste0("Daymet/daymet_prcp_", year, ".nc"))
  
  # Load and reproject DC boundary
  va <- counties(state = "VA", cb = TRUE, class = "sf")
  va_proj <- st_transform(va, crs(r))
  
  # Crop + mask to DC
  r_clipped <- mask(crop(r, vect(va_proj)), vect(va_proj))
  
  # January = layers 1–31
  r_jan <- r_clipped[[1:31]]
  
  # Plot mean daily precip for January
  plot(mean(r_jan), main = paste0("Mean Daily Precip – Jan ", year, " (VA)"), col = terrain.colors(20))
  
  ##########
  
  # Load Daymet raster
  raster_file <- paste0("Daymet/daymet_prcp_", year, ".nc")
  r <- rast(raster_file)
  
  #  Extract June 2 and June 3, 2020
  dates <- as.Date(paste0(year,"-01-01")) + 0:(nlyr(r) - 1)
  idx <- which(dates %in% as.Date(c(paste0(year,"-06-02"), paste0(year,"-06-07"))))
  r_june2_3 <- r[[idx]]
  
  # Get VA boundary and reproject to match raster CRS
  va_shape <- counties(state = "VA", cb = TRUE, class = "sf")
  va_shape_proj <- st_transform(va_shape, crs(r_june2_3))
  va_vect <- vect(va_shape_proj)
  
  # Clip raster to VA
  r_va <- crop(r_june2_3, va_vect) |> mask(va_vect)
  
  # Compute min, max, and mean for each day
  stats <- global(r_va, fun = c("min", "max", "mean"), na.rm = TRUE)
  
  
  # Use the transposed matrix directly
  observed_matrix <-as.data.frame(stats)
  
  # Create a clean data frame with proper date labels
  stats_df_clean <- data.frame(
    Date = as.Date(c(paste0(year,"-06-02"), paste0(year,"-06-03"))),
    Min = observed_matrix[, "min"],
    Max = observed_matrix[, "max"],
    Mean = observed_matrix[, "mean"]
  )
  
  # View the result
  print(stats_df_clean)
}





