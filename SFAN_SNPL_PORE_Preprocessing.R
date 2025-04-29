### Original Authors: James Brown, James_Brown@partner.nps.gov, and Alexa Ron, alexa_ron@partner.nps.gov v12/05/2024
## Script has been further modified for an annual workflow by Kirk Sherrill

## Abstract: Preprocessing script for Snowy Plover Point Reyes Datasets.  The
## preprocessed datasets will subsequently be added to a SNPL PORE data package
## with accompanying Ecological Metadata Language (EML) standard XML metadata
## via the 'SFAN_SNPL_PORE_EML_Procesing.Rmd' srcipt.


## Updates:


#clear the console
cat("\014")

#clear the environment
rm(list = ls())

#####################
# Variables to Define
#####################
# SNPL PORE Backend Database with the Datasets to be preprocssed
db_name <- "C:/Users/KSherrill/OneDrive - DOI/SFAN/VitalSigns/SnowyPlovers_PORE/SNPLOVER/SNPL_IM/Data/Deliverable/2024/Database_2310672/PORE_SNPL_BE_20250429.accdb"
# Directory where output preprocessed .csv dataset files will be exported
outPutDir <-'C:/Users/KSherrill/OneDrive - DOI/SFAN/VitalSigns/SnowyPlovers_PORE/Scripts/SNPL_PORE_DataPackage/Data/Input'
#############################################

packages <- c(
  "tidyverse",
  "terra",
  "geosphere",
  "readxl",
  "readr",
  "janitor",
  "here",
  "stringr",
  "lubridate",
  "taxize",
  "devtools",
  "sf",
  "furrr",
  "NPSdataverse",
  "RODBC"
)

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
rm(package.check)

getDataExport_Access.function <- function(db_name, inQuery) {
  DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  PATH <- paste0(DRIVERINFO, "DBQ=", db_name)

  if (!require("pacman")) install.packages("pacman")

  pacman::p_load("RODBC", "svDialogs", "getPass", "dplyr", "ggplot2")

  # Connect to Access DB

  channel <- odbcDriverConnect(PATH)
  on.exit(odbcClose(channel))

  # Import to a DataFrame

  dfOut <- sqlQuery(channel, inQuery)
}

writeFile <- function(df) {
  write.csv(df, paste0("SFAN_SNPL_", substring(deparse(substitute(df)), 3), ".csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")
}

# Preparation ---- Extracting from AccessDB to variables within R
query <- list(events = "SELECT tblEventsDataset.* FROM tblEventsDataset", observations = "SELECT tblSNPLObservationsDataset.* FROM tblSNPLObservationsDataset", preds = "SELECT tblPredatorDataset.* FROM tblPredatorDataset", banded = "SELECT tblSNPLBandedDataset.* FROM tblSNPLBandedDataset", nestMaster = "SELECT tblNestMasterDataset.* FROM tblNestMasterDataset", chicknband = "SELECT tblChickBandDataset.* FROM tblChickBandDataset")

df_Tables <- lapply(query, getDataExport_Access.function, db_name = db_name)

dfEvents <- df_Tables[[1]]
dfObservations <- df_Tables[[2]]
dfPredators <- df_Tables[[3]]
dfBands <- df_Tables[[4]]
dfNesting <- df_Tables[[5]]
dfChickBands <- df_Tables[[6]]

rm(df_Tables)

# Processing ----
#################
## Events Dataset
#################
### Operating on times within the dataset
dfEvents <- dfEvents |> dplyr::mutate(
  Start_Time_Local = lubridate::ymd_hms(paste0(as.character(Start_Date), " ", format(Start_Time, "%H:%M:%S")), tz = "America/Los_Angeles"),
  End_Time_Local = lubridate::ymd_hms(paste0(as.character(Start_Date), " ", format(End_Time, "%H:%M:%S")), tz = "America/Los_Angeles"),
  Local_Time_Zone = format(Start_Time_Local, "%Z"),
  Start_Time = format(lubridate::with_tz(Start_Time_Local, "UTC"), "%H:%M:%S%z"),
  End_Time = format(lubridate::with_tz(End_Time, "UTC"), "%H:%M:%S%z"),
  Start_Time_Local = format(Start_Time_Local, "%H:%M:%S"),
  End_Time_Local = format(End_Time_Local, "%H:%M:%S"),
  DataProcessingLevelDate = format(lubridate::force_tz(DataProcessingLevelDate, "America/Los_Angeles"), "%Y-%m-%d %H:%M:%S"),
  Tide_Cond = stringr::str_to_title(Tide_Cond),
  Updated_Date = format(lubridate::with_tz(Updated_Date, "America/Los_Angeles"), "%Y-%m-%d %H:%M:%S%z"),
  Wind_Dir = stringr::str_to_upper(Wind_Dir)
)

# Dropping the Created and Verified fields
dfEvents <- dfEvents[, !(names(dfEvents) %in% c("Created_Date", "Created_By", "Verified_Date", "Verified_By"))]

# Replacing ";" with "|" in concatenated Observers field
# Adding Darwing Core fields Type as 'event' and Basis_of_Record as 'HumanObservation
dfEvents <- dfEvents |> dplyr::mutate(
  Observers = dplyr::if_else(
    grepl(";", Observers),
    gsub(";", " | ", Observers),
    Observers
  ),
  Type = "event",
  Basis_Of_Record = "HumanObservation"
)

#Move field locations in dataframe
dfEvents <- dfEvents |> dplyr::relocate(
  Start_Time_Local,
  .after = Start_Time
)

dfEvents <- dfEvents |> dplyr::relocate(
  c(End_Time_Local, Local_Time_Zone),
  .after = End_Time
)

dfEvents <- dfEvents |> dplyr::relocate(
  c(Type, Basis_Of_Record, Unit_Code),
  .after = Event_ID
)
#######################
## Dataset Observations 
#######################

### Dates and times
dfObservations <- dfObservations |> dplyr::mutate(
  temp = lubridate::ymd_hms(paste0(Start_Date, " ", format(SNPL_Time, "%H:%M:%S")), tz = "America/Los_Angeles"),
  Local_Time_Zone = format(temp, "%Z"),
  SNPL_Time = dplyr::if_else(
    !is.na(temp),
    format(lubridate::with_tz(temp, "UTC"), "%H:%M:%S%z"),
    NA,
    missing = NA
  ),
  SNPL_Time_Local = dplyr::if_else(
    !is.na(temp),
    format(temp, "%H:%M:%S"),
    NA,
    missing = NA
  ),
  Scientific_Name = "Charadrius nivosus",
  Type = "event",
  Basis_Of_Record = "HumanObservation",
  Unit_Code = "PORE"
)

### Geography - Create Lat/Lon fields - Note starting in 2024 Coordinates in X_Coord and Y_Coord at Lat/Lon
dfObservations <- dfObservations |> QCkit::generate_ll_from_utm(
  X_Coord,
  Y_Coord,
  UTM_Zone,
  Datum
)

# For Records that are Lat/Lon in X_Coord|Y_Coord add to the decimalLatitude and decimalLongitude fields
dfObservations <- dfObservations %>%
  mutate(
    decimalLongitude = if_else(Coord_System == "GCS", X_Coord, decimalLongitude),
    decimalLatitude  = if_else(Coord_System == "GCS", Y_Coord, decimalLatitude)
  )


# Removing superfluous columns
dfObservations <- dfObservations[, !(names(dfObservations) %in% c("temp", "LatLong_CRS"))]

dfObservations <- dfObservations |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .before = X_Coord
)

# Remove the Native Geographic Fields - X_Coord, Y_Coord, Est_H_Error, UTM_Zone 
dfObservations <- dfObservations[, !(names(dfObservations) %in% c("X_Coord", "Y_Coord", "Est_H_Error", "UTM_Zone"))]

# Define the 'Coord_System' value to 'GCS'
dfObservations <- dfObservations %>%
  mutate(Coord_System = if_else(is.na(decimalLatitude), NA_character_, 'GCS'))


dfObservations <- dfObservations |> dplyr::relocate(
  Scientific_Name,
  .before = SNPL_Male
)

dfObservations <- dfObservations |> dplyr::relocate(
  c(SNPL_Time_Local, Local_Time_Zone),
  .after = SNPL_Time
)

dfObservations <- dfObservations |> dplyr::relocate(
  c(Type, Basis_Of_Record, Unit_Code),
  .after = Event_ID
)

dfObservations <- dfObservations |> dplyr::rename(
  Decimal_Latitude = decimalLatitude,
  Decimal_Longitude = decimalLongitude
)

####################
## Dataset Predators 
####################

dfPredators <- dfPredators |> dplyr::mutate(
  temp.zone = dplyr::if_else(!is.na(X_Coord) & !is.na(Y_Coord), "10N", NA),
  Type = "event",
  Basis_Of_Record = "HumanObservation",
  Unit_Code = "PORE"
)

# Derive the Lat/Lon values where native coordinates are UTM:
# Identify which rows are UTM - Series
is_utm <- dfPredators$Coord_System == "UTM"

# Make a copy of just the UTM rows
utm_rows <- dfPredators[is_utm, ]

# Run the conversion only on UTM rows
utm_rows <- utm_rows |> QCkit::generate_ll_from_utm(
  X_Coord,
  Y_Coord,
  temp.zone,
  Datum
)

# Define the Lat/Lon fields where is native UTM coordinate.
dfPredators[is_utm, c("decimalLongitude", "decimalLatitude")] <- utm_rows[, c("decimalLongitude", "decimalLatitude")]

rm(utm_rows)
rm(is_utm)


# For Records that are Lat/Lon in X_Coord|Y_Coord add to the decimalLatitude and decimalLongitude fields
dfPredators <- dfPredators %>%
  mutate(
    decimalLongitude = if_else(Coord_System == "GCS", X_Coord, decimalLongitude),
    decimalLatitude  = if_else(Coord_System == "GCS", Y_Coord, decimalLatitude)
  )


# Removing superfluous columns
dfPredators <- dfPredators[, !(names(dfPredators) %in% c("temp.zone"))]

dfPredators <- dfPredators |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .before = X_Coord
)

# Remove the Native Geographic Fields - X_Coord, Y_Coord, Est_H_Error, UTM_Zone 
dfPredators <- dfPredators[, !(names(dfPredators) %in% c("X_Coord", "Y_Coord", "UTM_Zone"))]

# Define the 'Coord_System' value to 'GCS'
dfPredators <- dfPredators %>%
  mutate(Coord_System = if_else(is.na(decimalLatitude), NA_character_, 'GCS'),
         Datum = if_else(is.na(decimalLatitude), NA_character_, Datum))


dfPredators <- dfPredators |> dplyr::relocate(
  c(Type, Basis_Of_Record, Unit_Code),
  .after = Event_ID
)

dfPredators <- dfPredators |> dplyr::rename(
  Decimal_Latitude = decimalLatitude,
  Decimal_Longitude = decimalLongitude
)

# Removing columns that are no longer necessary
dfPredators <- dfPredators[, !(names(dfPredators) %in% c("temp.zone", "LatLong_CRS"))]

################
## Bands Dataset
################
dfBands <- dfBands |> dplyr::mutate(
  Type = "event",
  Basis_Of_Record = "HumanObservation",
  Unit_Code = "PORE"
)

dfBands <- dfBands |> dplyr::relocate(
  c(Type, Basis_Of_Record, Unit_Code),
  .after = Event_ID
)

##################
## Dataset Nesting
#################

dfNesting <- dfNesting |> dplyr::mutate(
  Type = "event",
  Basis_Of_Record = "HumanObservation",
  Unit_Code = "PORE",
  GPSunit = dplyr::if_else(
    GPSunit == "Garmin\r\nG",
    "Garmin G",
    stringr::str_to_title(GPSunit)
  ),
  ChickLoss_Age1 = as.integer(ChickLoss_Age1),
  ChickLoss_Age2 = as.integer(ChickLoss_Age2),
  Updated_Date = format(Updated_Date, "%Y-%m-%d %H:%M:%S"),
  ChickLoss_Age4 = NA
)


# Derive the Lat/Lon values where native coordinates are UTM:
# Identify which rows are UTM and has a coordinate- Series
is_utm <- dfNesting$Coord_System == "UTM" & !is.na(dfNesting$X_Coord)


# Make a copy of just the UTM rows
utm_rows <- dfNesting[is_utm, ]

# Run the conversion only on UTM rows - (93 Missing Coordinates)
utm_rows <- utm_rows |> QCkit::generate_ll_from_utm(
  X_Coord,
  Y_Coord,
  UTM_Zone,
  Datum
)

# Define the Lat/Lon fields where is native UTM coordinate.
dfNesting[is_utm, c("decimalLongitude", "decimalLatitude")] <- utm_rows[, c("decimalLongitude", "decimalLatitude")]

rm(utm_rows)
rm(is_utm)


# For Records that are Lat/Lon in X_Coord|Y_Coord add to the decimalLatitude and decimalLongitude fields
dfNesting <- dfNesting %>%
  mutate(
    decimalLongitude = if_else(Coord_System == "GCS", X_Coord, decimalLongitude),
    decimalLatitude  = if_else(Coord_System == "GCS", Y_Coord, decimalLatitude)
  )


# Removing superfluous columns

dfNesting <- dfNesting |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .before = X_Coord
)

# Remove the Native Geographic Fields - X_Coord, Y_Coord, Est_H_Error, UTM_Zone 
dfNesting <- dfNesting[, !(names(dfNesting) %in% c("X_Coord", "Y_Coord", "UTM_Zone"))]

# Define the 'Coord_System' value to 'GCS'
dfNesting <- dfNesting %>%
  mutate(Coord_System = if_else(is.na(decimalLatitude), NA_character_, 'GCS'),
         Datum = if_else(is.na(decimalLatitude), NA_character_, Datum))

# Move Type, Basis and Unit code field locations
dfNesting <- dfNesting |> dplyr::relocate(
  c(Type, Basis_Of_Record, Unit_Code),
  .after = Nest_ID
)

dfNesting <- dfNesting |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .after = QCNotes
)

dfNesting <- dfNesting |> dplyr::rename(
  Decimal_Latitude = decimalLatitude,
  Decimal_Longitude = decimalLongitude
)

dfNesting <- dfNesting[, !(names(dfNesting) %in% c("Verified_Date", "Verified_By"))]

##############
## Chick Bands
##############
dfChickBands <- dfChickBands |> dplyr::mutate(
  Scientific_Name = "Charadrius nivosus",
  Type = "event",
  Basis_Of_Record = "HumanObservation",
  Unit_Code = "PORE"
)

dfChickBands <- dfChickBands |> dplyr::relocate(
  c(Type, Basis_Of_Record, Unit_Code),
  .after = SNPL_Data_ID
)

dfChickBands <- dfChickBands |> dplyr::relocate(
  Scientific_Name,
  .after = Year
)

#########################################
# Export cleaned datasets to CSV in the 'Input' directory of the Data Package Script location
#########################################

outPath <- paste0(outPutDir, "/SFAN_SNPL_Events.csv")
utils::write.csv(dfEvents, outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_Bands.csv")
utils::write.csv(dfBands, outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_ChickBands.csv")
utils::write.csv(dfChickBands, outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_Nesting.csv")
utils::write.csv(dfNesting, outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_Observations.csv")
utils::write.csv(dfObservations, outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_Predators.csv")
utils::write.csv(dfPredators, outPath, na = "", row.names = FALSE)

