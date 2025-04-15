### Original Authors: James Brown, James_Brown@partner.nps.gov, and Alexa Ron, alexa_ron@partner.nps.gov v12/05/2024
## Script has been further modified for an annual workflow by Kirk Sherrill

## Abstract: Preprocessing script for Snowy Plover Point Reyes Datasets.  The
## preprocessed datasets will subsequently be added to a SNPL PORE data package
## with accompanying Ecological Metadata Language (EML) standard XML metadata
## via the 'SFAN_SNPL_PORE_EML_Procesing.Rmd' srcipt.


## Updates:


rm(list = ls())

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

db_name <- "..\\..\\01_raw_data_and_documents\\Database\\PORE_SNPL_BE_20240802.accdb"
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
## Events ----
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

dfEvents <- dfEvents[, !(names(dfEvents) %in% c("Created_Date", "Created_By", "Verified_Date", "Verified_By"))]

dfEvents <- dfEvents |> dplyr::mutate(
  Observers = dplyr::if_else(
    grepl(";", Observers),
    gsub(";", " | ", Observers),
    Observers
  ),
  Type = "event",
  Basis_Of_Record = "HumanObservation"
)

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

## Dataset with Coordinates: Observations ----
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

### Geography
dfObservations <- dfObservations |> QCkit::generate_ll_from_utm(
  X_Coord,
  Y_Coord,
  UTM_Zone,
  Datum
)

# Removing superfluous columns
dfObservations <- dfObservations[, !(names(dfObservations) %in% c("temp", "LatLong_CRS"))]

dfObservations <- dfObservations |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .before = X_Coord
)

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

## Dataset with Coordinates: Predators ----

dfPredators <- dfPredators |> dplyr::mutate(
  temp.zone = dplyr::if_else(!is.na(X_Coord) & !is.na(Y_Coord), "10N", NA),
  Type = "event",
  Basis_Of_Record = "HumanObservation",
  Unit_Code = "PORE"
)

dfPredators <- dfPredators |> QCkit::generate_ll_from_utm(
  X_Coord,
  Y_Coord,
  temp.zone,
  Datum
)

dfPredators <- dfPredators |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .before = "X_Coord"
)

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
## Bands ----

dfBands <- dfBands |> dplyr::mutate(
  Type = "event",
  Basis_Of_Record = "HumanObservation",
  Unit_Code = "PORE"
)

dfBands <- dfBands |> dplyr::relocate(
  c(Type, Basis_Of_Record, Unit_Code),
  .after = Event_ID
)

## Dataset with Coordinates: Nesting ----

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

dfNesting <- dfNesting |> QCkit::generate_ll_from_utm(
  X_Coord,
  Y_Coord,
  UTM_Zone,
  Datum
)

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

dfNesting <- dfNesting[, !(names(dfNesting) %in% c("Verified_Date", "Verified_By", "LatLong_CRS"))]

## Chick Bands ----
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

# EML Formatted Datasets -------------------------------------------------------
# EML cannot handle empty categorical or date columns, make one row a value for temporary datasets to produce EML
# Nesting
## dfNesting2 <- dfNesting
## dfNesting2$ChickLoss_Weekend4[1] <- "Yes"
## dfNesting2$ChickLoss_Date4[1] <- "2001-01-01"

# Observations
## dfObservations2 <- dfObservations
## dfObservations2[1, 35:46] <- "Yes" # BehaviorTerritoryCC, BehaviorTerritoryLW, BehaviorTerritoryMD, BehaviorTerritorySC, BehaviorTerritoryCP, BehaviorNestCP, BehaviorNestDC, BehaviorNestAI, BehaviorNestFN, BehaviorChicksAC, BehaviorChicksNA, BehaviorOtherFG

# Output
writeFile(dfEvents)
writeFile(dfBands)
writeFile(dfChickBands)
writeFile(dfNesting)
writeFile(dfObservations)
writeFile(dfPredators)
