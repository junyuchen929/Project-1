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
