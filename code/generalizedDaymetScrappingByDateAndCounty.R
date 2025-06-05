library(terra)
library(sf)
library(tigris)
library(httr)
library(stringi)

library(lubridate)
#Lets let the file download slap it in the same file
setwd(getwd())
if (!dir.exists("Daymet")) {dir.create("Daymet")}
options(tigris_use_cache = TRUE)

# info you input =w=
currDate = "1996-01-20"
inpState = "VIRGINIA"
inpCounty = "FAIRFAX county"
# Found the bounding boxes at https://observablehq.com/@rdmurphy/u-s-state-bounding-boxes,
# it is in the format West, South, East, North

# Pasted the bounding boxes to a dataframe then cut up them to a usable
# format for the scraping link
boundingPaste$VIRGINIA = "-83.67570908179081,36.540885157941574,-75.24086819838197,39.46598340442404"
boundingPaste$MARYLAND = "-79.48700299202991,37.91709878227782,-75.05063561675617,39.72284225191251"
boundingPaste$DC = "-77.11806895668957,38.79162154730547,-76.90988990509905,38.99435963428633"
dirList = stri_split_fixed(boundingPaste[[inpState]], ",")
nBound = dirList[[1]][4]
wBound = dirList[[1]][1]
sBound = dirList[[1]][2]
eBound = dirList[[1]][3]



#SCRAPPING PART TAKES SOME TIME
# Uses ncss server
base_url <- "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/2129"

# Originally used multiple dates but this should be fine
for (date in currDate) {
  # Splitting date into usable chunks for link
  year = substr(date, start = 1, stop = 4)
  month = substr(date, start = 6, stop = 7)
  day = substr(date, start = 9, stop = 10)
  earlyDate = as.Date(date) - 2
  earlyyear = substr(earlyDate, start = 1, stop = 4)
  earlymonth = substr(earlyDate, start = 6, stop = 7)
  earlyday = substr(earlyDate, start = 9, stop = 10)
  
  # Have some funny link documentation =w= https://github.com/ornldaac/gridded_subset_example_script
  # This link is why I cut up all those strings and dates
  dest <- file.path("Daymet", paste0("daymet_prcp_", date, "_", inpCounty, ".nc"))
  
  if (!file.exists(dest)) {
    url <- paste0(base_url, "/daymet_v4_daily_na_prcp_", year, ".nc?var=lat&var=lon&var=prcp&north=", 
                  nBound, "&west=", wBound, "&east=", eBound, "&south=", sBound,
                  "&disableProjSubset=on&horizStride=1&time_start=", earlyyear, "-", 
                  earlymonth, "-", earlyday, "T00:00:00Z&time_end=", year, "-", month, "-", 
                  day, "T23:59:59Z&timeStride=1")
    
    message("Downloading: ", url)
    GET(url, write_disk(dest, overwrite = TRUE), progress())
  } else {
    message("File already exists: ", dest)
  }
}



# Example for one year:
for (date in currDate) {
  
  r <- rast(paste0("Daymet/daymet_prcp_", date, "_", inpCounty, ".nc"))
  
  # Load and reproject DC boundary
  co <- county_subdivisions(state = inpState, county = inpCounty, cb = TRUE, class = "sf")
  co_proj <- st_transform(co, crs(r))
  
  # Crop + mask to DC
  r_clipped <- mask(crop(r, vect(co_proj)), vect(co_proj))
  
  # Plot mean daily precip for January  
  plot(mean(r_clipped), main = paste0("Mean Daily Precip – ", earlyDate, " - ", date, " (", inpCounty, ")"), col = terrain.colors(20))
  
  ##########
  
  # Load Daymet raster
  raster_file <- paste0("Daymet/daymet_prcp_", date, "_", inpCounty, ".nc")
  r <- rast(raster_file)
  
  # Get VA boundary and reproject to match raster CRS
  co_shape <- county_subdivisions(state = inpState, county = inpCounty, cb = TRUE, class = "sf")
  co_shape_proj <- st_transform(co_shape, crs(r))
  co_vect <- vect(co_shape_proj)
  
  # Clip raster to VA
  r_co <- crop(r, co_vect) |> mask(co_vect)
  
  # Compute min, max, and mean for each day
  stats <- global(r_co, fun = c("min", "max", "mean"), na.rm = TRUE)
  
  
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
min = min(observed_matrix[, "min"])
max = max(observed_matrix[, "max"])
mean = mean(observed_matrix[, "mean"])
print(min)
print(max)
print(mean)


