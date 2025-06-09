Data and code for Project 1

About the generalizedDaymetData things:
 - Will create a Daymet folder for you and dump all the downloaded data there
 - Must receive State, County, and Date to caluclate its min, max, and mean.
 - Counties with city counterparts must specify whether they are city or county (e.g. "Baltimore County" and "Baltimore City" but "Baltimore" and "Baltimore (C)" will not work.)
 - Does not redownload if it finds that the file for the county and date already exist in the Daymet Folder
 - I have no idea how to make the python dataframe run the arguments to the R code or add it back on

About daymetOutput
 - "max-2" represents max for the day 2 days before that day, "max-1" for the previous day, and "max" for that day.
 - Date is also in the form yyyy-mm-dd
 - The "Duplicates" are due to multiple events on the same day and in the same county. They have different EventIDs but every other aspect is or likely is the same.

# Abstract <\br>

# Introduction <\br>

# Methods <\br>
## 2.1 Data Span <\br>
In this study we used data from the [NLCD](https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover), [NOAA](https://www.ncdc.noaa.gov/stormevents/listevents.jsp?eventType=%28C%29+Flash+Flood&beginDate_mm=01&beginDate_dd=01&beginDate_yyyy=1996&endDate_mm=12&endDate_dd=31&endDate_yyyy=2023&county=ALL&hailfilter=0.00&tornfilter=0&windfilter=000&sort=DT&submitbutton=Search&statefips=11%2CDISTRICT+OF+COLUMBIA), [Daymet](https://daymet.ornl.gov/), [ACS](https://data.census.gov/table?q=acs&g=040XX00US51), tigris

# Results <\br>

# Discussion/Conclusion <\br>
