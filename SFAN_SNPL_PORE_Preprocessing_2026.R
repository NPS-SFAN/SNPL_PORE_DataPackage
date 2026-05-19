### Original Authors: James Brown, James_Brown@partner.nps.gov, and Alexa Ron, alexa_ron@partner.nps.gov 12/05/2024
## Script has been further modified for an annual workflow by Kirk Sherrill - April 2025.
## Script modified for a more efficient workflow by Daniel Woods - April 2026.

## Abstract: Preprocessing script for Snowy Plover Point Reyes Datasets.  The
## preprocessed datasets will subsequently be added to a SNPL PORE data package
## with accompanying Ecological Metadata Language (EML) standard XML metadata
## via the 'SFAN_SNPL_PORE_EML_Procesing.Rmd' srcipt.


## Updates:

# Updates made to account for an annual data package creation workflow - KRS 4/29/2025.
# Updates made to make editing more efficient - DJW 04/24/2026


#clear the console
cat("\014")

#clear the environment
rm(list = ls())

#####################
# Variables to Define
#####################
# SNPL PORE Backend Database with the Datasets to be preprocssed
db_name <- "//INPPORE07/Resources/Natural/SNPLOVER/SNPL_IM/DATA/Database/Dbase_BE/PORE_SNPL_BE_20260514v2.accdb"
# Directory where output preprocessed .csv dataset files will be exported
outPutDir <-'C:/Users/dwoods/GitHub/SFAN/SNPL_PORE_DataPackage/Data/Input'
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

tablenames <- c("Events","Observations","Predators","Bands","Nesting","ChickBands")
names(df_Tables) <- tablenames

# Checkpoint Temp DF To Skip Access Usage
saveRDS(df_Tables,"temp_tables.RDS")

# Reading from Checkpoint
# df_Tables <- readRDS("temp_tables.RDS")

# Processing ----

#################
## Events Dataset
#################
### Operating on times within the dataset

df_Tables[["Events"]] <- df_Tables[["Events"]] |> dplyr::mutate(
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
df_Tables[["Events"]] <- df_Tables[["Events"]][, !(names(df_Tables[["Events"]]) %in% c("Created_Date", "Created_By", "Verified_Date", "Verified_By"))]

# Replacing ";" with "|" in concatenated Observers field

df_Tables[["Events"]] <- df_Tables[["Events"]] |> dplyr::mutate(
  Observers = dplyr::if_else(
    grepl(";", Observers),
    gsub(";", " | ", Observers),
    Observers
  ))


# Move field locations in dataframe
df_Tables[["Events"]] <- df_Tables[["Events"]] |> dplyr::relocate(
  Start_Time_Local,
  .after = Start_Time
)

df_Tables[["Events"]] <- df_Tables[["Events"]] |> dplyr::relocate(
  c(End_Time_Local, Local_Time_Zone),
  .after = End_Time
)

# Converting some col names to DWC standard, removing Updated By & Date

df_Tables <- purrr::modify_at(
  df_Tables,
  "Events", 
  ~ {
    .x |>  
      dplyr::rename(
        cloudCoverPercent = Cloud_Cover,
        tideCondition = Tide_Cond,
        windDirection = Wind_Dir,
        windSpeedMPH = Wind_Spd_mph,
        airTemperatureDegreesF = Air_Temp_degF,
        relativeHumidityPercent = Rel_Hum_per
      ) |> 
      dplyr::select(
        !"Updated_Date":"Updated_By"
      )
  }
)

#######################
## Dataset Observations 
#######################

### Dates and times

df_Tables[["Observations"]] <- df_Tables[["Observations"]] |> dplyr::mutate(
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
  )
)

### Geography - Create Lat/Lon fields - Note starting in 2024 Coordinates in X_Coord and Y_Coord at Lat/Lon
df_Tables[["Observations"]] <- df_Tables[["Observations"]] |> QCkit::generate_ll_from_utm(
  X_Coord,
  Y_Coord,
  UTM_Zone,
  Datum
)

# For Records that are Lat/Lon in X_Coord|Y_Coord add to the decimalLatitude and decimalLongitude fields
df_Tables[["Observations"]] <- df_Tables[["Observations"]] %>%
  mutate(
    decimalLongitude = if_else(Coord_System == "GCS", X_Coord, decimalLongitude),
    decimalLatitude  = if_else(Coord_System == "GCS", Y_Coord, decimalLatitude)
  )


# Removing superfluous columns
df_Tables[["Observations"]] <- df_Tables[["Observations"]][, !(names(df_Tables[["Observations"]]) %in% c("temp", "LatLong_CRS"))]

df_Tables[["Observations"]] <- df_Tables[["Observations"]] |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .before = X_Coord
)

# Remove the Native Geographic Fields - X_Coord, Y_Coord, Est_H_Error, UTM_Zone 
df_Tables[["Observations"]] <- df_Tables[["Observations"]][, !(names(df_Tables[["Observations"]]) %in% c("X_Coord", "Y_Coord", "Est_H_Error", "UTM_Zone"))]

# Define the 'Coord_System' value to 'GCS'
df_Tables[["Observations"]] <- df_Tables[["Observations"]] %>%
  mutate(Coord_System = if_else(is.na(decimalLatitude), NA_character_, 'GCS'))


df_Tables[["Observations"]] <- df_Tables[["Observations"]] |> dplyr::relocate(
  c(SNPL_Time_Local, Local_Time_Zone),
  .after = SNPL_Time
)


####################
## Dataset Predators 
####################

df_Tables[["Predators"]] <- df_Tables[["Predators"]] |> dplyr::mutate(
  temp.zone = dplyr::if_else(!is.na(X_Coord) & !is.na(Y_Coord), "10N", NA)
)

# Derive the Lat/Lon values where native coordinates are UTM:
# Identify which rows are UTM - Series
is_utm <- df_Tables[["Predators"]]$Coord_System == "UTM"

# Make a copy of just the UTM rows
utm_rows <- df_Tables[["Predators"]][is_utm, ]

# Run the conversion only on UTM rows
utm_rows <- utm_rows |> QCkit::generate_ll_from_utm(
  X_Coord,
  Y_Coord,
  temp.zone,
  Datum
)

# Define the Lat/Lon fields where is native UTM coordinate.
df_Tables[["Predators"]][is_utm, c("decimalLongitude", "decimalLatitude")] <- utm_rows[, c("decimalLongitude", "decimalLatitude")]

rm(utm_rows)
rm(is_utm)


# For Records that are Lat/Lon in X_Coord|Y_Coord add to the decimalLatitude and decimalLongitude fields
df_Tables[["Predators"]] <- df_Tables[["Predators"]] %>%
  mutate(
    decimalLongitude = if_else(Coord_System == "GCS", X_Coord, decimalLongitude),
    decimalLatitude  = if_else(Coord_System == "GCS", Y_Coord, decimalLatitude)
  )


# Removing superfluous columns
df_Tables[["Predators"]] <- df_Tables[["Predators"]][, !(names(df_Tables[["Predators"]]) %in% c("temp.zone"))]

df_Tables[["Predators"]] <- df_Tables[["Predators"]] |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .before = X_Coord
)

# Remove the Native Geographic Fields - X_Coord, Y_Coord, Est_H_Error, UTM_Zone 
df_Tables[["Predators"]] <- df_Tables[["Predators"]][, !(names(df_Tables[["Predators"]]) %in% c("X_Coord", "Y_Coord", "UTM_Zone"))]

# Define the 'Coord_System' value to 'GCS'
df_Tables[["Predators"]] <- df_Tables[["Predators"]] %>%
  mutate(Coord_System = if_else(is.na(decimalLatitude), NA_character_, 'GCS'),
         Datum = if_else(is.na(decimalLatitude), NA_character_, Datum))


df_Tables[["Predators"]] <- df_Tables[["Predators"]] |> dplyr::rename(
  Decimal_Latitude = decimalLatitude,
  Decimal_Longitude = decimalLongitude
)

# Removing columns that are no longer necessary
df_Tables[["Predators"]] <- df_Tables[["Predators"]][, !(names(df_Tables[["Predators"]]) %in% c("temp.zone", "LatLong_CRS"))]


##################
## Dataset Nesting
#################

df_Tables[["Nesting"]] <- df_Tables[["Nesting"]] |> dplyr::mutate(
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
is_utm <- df_Tables[["Nesting"]]$Coord_System == "UTM" & !is.na(df_Tables[["Nesting"]]$X_Coord)


# Make a copy of just the UTM rows
utm_rows <- df_Tables[["Nesting"]][is_utm, ]

# Run the conversion only on UTM rows - (93 Missing Coordinates)
utm_rows <- utm_rows |> QCkit::generate_ll_from_utm(
  X_Coord,
  Y_Coord,
  UTM_Zone,
  Datum
)

# Define the Lat/Lon fields where is native UTM coordinate.
df_Tables[["Nesting"]][is_utm, c("decimalLongitude", "decimalLatitude")] <- utm_rows[, c("decimalLongitude", "decimalLatitude")]

rm(utm_rows)
rm(is_utm)


# For Records that are Lat/Lon in X_Coord|Y_Coord add to the decimalLatitude and decimalLongitude fields
df_Tables[["Nesting"]] <- df_Tables[["Nesting"]] %>%
  mutate(
    decimalLongitude = if_else(Coord_System == "GCS", X_Coord, decimalLongitude),
    decimalLatitude  = if_else(Coord_System == "GCS", Y_Coord, decimalLatitude)
  )

# Adding NA to microhabitat columns where code is NA

df_Tables[["Nesting"]] <- df_Tables[["Nesting"]] |> 
  dplyr::mutate(
    MicroSand = dplyr::if_else(stringr::str_detect(MicroCodes,"S"),"Yes","No"),
    MicroCoarse = dplyr::if_else(stringr::str_detect(MicroCodes,"R"),"Yes","No"),
    MicroSeaweedKelp = dplyr::if_else(stringr::str_detect(MicroCodes,"K"),"Yes","No"),
    MicroWoody = dplyr::if_else(stringr::str_detect(MicroCodes,"W"),"Yes","No"),
    MicroHumanTrash = dplyr::if_else(stringr::str_detect(MicroCodes,"T"),"Yes","No"),
    MicroVegetation = dplyr::if_else(stringr::str_detect(MicroCodes,"V"),"Yes","No"),
    MicroSmooth = dplyr::if_else(stringr::str_detect(MicroCodes,"E"),"Yes","No"),
    MicroSteep = dplyr::if_else(stringr::str_detect(MicroCodes,"D"),"Yes","No"),
    MicroHumanDogPrints = dplyr::if_else(stringr::str_detect(MicroCodes,"P"),"Yes","No"),
    MicroHorsePrints = dplyr::if_else(stringr::str_detect(MicroCodes,"H"),"Yes","No"),
    MicroVehicleTracks = dplyr::if_else(stringr::str_detect(MicroCodes,"A"),"Yes","No"),
  )

#####
# Check for Null fields - ChickLoss_4 fields are all null as of 5/1/2025
################

# If Chickloss_Date4 values are null delete these fields
if (all(is.na(df_Tables[["Nesting"]]$Chickloss_Date4))) {
  df_Tables[["Nesting"]] <- df_Tables[["Nesting"]][, !(names(df_Tables[["Nesting"]]) %in% c("ChickLoss_Date4", "ChickLoss_Age4", "ChickLoss_Weekend4"))]
  
  print(paste0(
    "Deleted Null fields: Chickloss_Date4, Chickloss_Age4, and ChickLoss_Weekend4 fields."))
  
} else {
  
  print(paste0(
    "WARNING - There are now records in the: Chickloss_Date4, Chickloss_Age4, and ChickLoss_Weekend4 fields.\n\n",
    "Fields have not been deleted. Please add these fields to the attributes_SFAN_SNPL_Nesting.txt field attribute template"
  ))
}


# Removing superfluous columns
df_Tables[["Nesting"]] <- df_Tables[["Nesting"]] |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .before = X_Coord
)

df_Tables <- purrr::modify_at(
  df_Tables,
  "Nesting", 
  ~ {
    .x |>  
      dplyr::select(
        !"Created_By":"Updated_By"
      )
  }
)

# Remove the Native Geographic Fields - X_Coord, Y_Coord, Est_H_Error, UTM_Zone 
df_Tables[["Nesting"]] <- df_Tables[["Nesting"]][, !(names(df_Tables[["Nesting"]]) %in% c("X_Coord", "Y_Coord", "UTM_Zone"))]

# Define the 'Coord_System' value to 'GCS'
df_Tables[["Nesting"]] <- df_Tables[["Nesting"]] %>%
  mutate(Coord_System = if_else(is.na(decimalLatitude), NA_character_, 'GCS'),
         Datum = if_else(is.na(decimalLatitude), NA_character_, Datum))


df_Tables[["Nesting"]] <- df_Tables[["Nesting"]] |> dplyr::relocate(
  tidyselect::starts_with("decimal"),
  .after = QCNotes
)

# Renaming columns
df_Tables[["Nesting"]] <- df_Tables[["Nesting"]] |> dplyr::rename(
  Decimal_Latitude = decimalLatitude,
  Decimal_Longitude = decimalLongitude,
  femaleBand = F_Band,
  maleBand = M_Band
)

df_Tables[["Nesting"]] <- df_Tables[["Nesting"]][, !(names(df_Tables[["Nesting"]]) %in% c("Verified_Date", "Verified_By","Created_Date"))]

#################
## Chick Bands Dataset
#################

# Renaming Columns for readability

df_Tables[["ChickBands"]] <- df_Tables[["ChickBands"]] |> dplyr::rename(
  drynessInPercent = PctDryness,
  chickWeightInGrams = ChickWeight_g
)

# Fixing semicolon error 2024-2025 and/or extra spaces
df_Tables[["ChickBands"]] <- df_Tables[["ChickBands"]] |> 
  dplyr::mutate(
    BandCombination = stringr::str_replace(BandCombination,";",":")
  ) |> 
  dplyr::mutate(
    BandCombination = stringr::str_replace(BandCombination," ","")
  )

##################### 
# Taxonomy Processing
#####################

### Taxize Workflow and Global Names Resolver not up - Commented out Service not working 1/23/2025
### Blocked out code below not setup for SNPL PORE process 

# In place of the taxize -  Global Names Resolver workflow, reading in the 'taxonomy_preprocessing.csv'
# taxonomy table and check for any new realized taxon in need of definition.  
# Realized taxon most likely will be stable after initial year so can re-use existing 
# taxonomic coverage template from preivous year.

# Get the realized taxon codes in the Predator dataset
uniqueTaxon_DF <- df_Tables[["Predators"]] %>%
  distinct(SpeciesCode)

# Remove rows where SpeciesCode is NA or empty
uniqueTaxon_DF <- subset(uniqueTaxon_DF, !is.na(SpeciesCode) & SpeciesCode != "")


# Sort on the Species Code field
uniqueTaxon_DF <- uniqueTaxon_DF %>%
  arrange(SpeciesCode)

# Add in the Snowy Plover Record which is not in the Predator Dataset
uniqueTaxon_DF <- rbind(uniqueTaxon_DF, data.frame(SpeciesCode = "SNPL"))

# Get Count of Records
countTaxon_DF <- dim(uniqueTaxon_DF)[1]

# Read in Taxonomic Preprocessing Template 
taxonPrePath <- here::here(gsub(" ", "", paste("Data", "/Metadata_Template", "/taxonomy_preprocessing.csv")))
taxonTemplate_df <- readr::read_csv(taxonPrePath, lazy = FALSE)


# Get Unique template values
taxonTemplate_df_distinct <- taxonTemplate_df %>%
  distinct(SpeciesCode)


#First Outer Join of all realized on SpeciesCode
uniqueTaxonCodes_DF_Both <- uniqueTaxon_DF %>%
  left_join(
    taxonTemplate_df_distinct %>% mutate(name_TaxonTemplate = SpeciesCode),
    by = c("SpeciesCode" = "SpeciesCode")
  )

# Check if count of joined records equals number of records in uniqueBirds_DFCount if equal Taxonomic Template has a definition per taxon
countNotNull <- sum(!is.na(uniqueTaxonCodes_DF_Both$name_TaxonTemplate))

print(paste("Number of Matching Taxon in Dataset and Taxonomic Coverage Template is -", countNotNull))

if (countTaxon_DF == countNotNull) {
  print("Number of Realized Taxon and Matching in Taxonomic Template is equal - Taxonomic Coverage doesn't need update all Realized Taxon are defined"
  )
  
  } else {
  
  #Subset to Taxon in need of definition in Taxonomic Coverages Template
  uniqueTaxonCodes_ToDefine <- uniqueTaxonCodes_DF_Both %>%
    filter(is.na(name_TaxonTemplate))
  
  outDFPath <- here::here(paste0("Input", "/TaxonToDefine.csv"))
  if (file.exists(outDFPath)) {
    file.remove(outDFPath)
    print(paste("Existing File - ", outDFPath, " - has been deleted."))
  }
  
  write.csv(uniqueTaxonCodes_ToDefine, outDFPath)
  countNull <- sum(is.na(uniqueTaxonCodes_ToDefine$name_TaxonTemplate))
  
  print(paste0("WARNING - there are - ", countNull, " - Records in need of definition in the Taxonomic Coverage Template before proceeding"))
  print(paste0("See Exported dataframe with Taxon to be defined in Tempalte at: ", outDFPath))
} 

#################
## All Datasets
#################

# Adding Plover SciName, Common Name, TSN to all tables except predators

df_Tables <- purrr::modify_at(
  df_Tables,
  c("Events","Observations","Bands","Nesting","ChickBands"), 
  ~ {
    .x |>  
      dplyr::mutate(
        scientificName = "Charadrius nivosus",
        commonName = "Western Snowy Plover",
        TSN = "824030"
      ) |> 
      dplyr::relocate(scientificName,commonName,TSN,.after = dplyr::contains("QCNotes"))
  }
)

# Fixing Exception in Events Table

df_Tables <- purrr::modify_at(
  df_Tables,
  "Events", 
  ~ {
    .x |>  
      dplyr::relocate(scientificName,commonName,TSN,.after = "EventQCNotes")
  }
)

# Removing arbitrary database-only ID columns

df_Tables <- purrr::imap(
  df_Tables,
  function(df, nm) {
    df |> 
      dplyr::select(!"ID")
    
  }
)

# Adding DarwinCore Type and Basis of Record, Park Unit, Scientific Name

df_Tables <- purrr::imap(
  df_Tables,
  function(df, nm) {
    df |> 
      dplyr::mutate(Unit_Code = "PORE",
                    unitName = "Point Reyes National Seashore",
                    type = "Event",
                    basisOfRecord = "HumanObservation") |>  
      dplyr::relocate(Unit_Code, unitName) |> 
      dplyr::rename(siteID = Loc_Code,
                    siteName = Loc_Name)
      
  }
)

# Converting column names to camel case

# Function to standardize column names to DWC 
# Note, define columns to ignore in the 'ignore' argument
# Acronyms within column names can also be ignored, but function only accepts 
## acronyms at the end of the name (i.e., "EventID")

acronyms <- c("CC","LW","MD","SC","CP","DC","AI","FN","GPS",
              "AC","NA","FG","QC","ID","MPH","TSN","ID")

to_camel_case_with_exception <- function(x, ignore = c("SNPL","TSN","QC"), acronym = acronyms) {
  # Convert to lower camel case
  cc <- snakecase::to_lower_camel_case(x)
  
  # Preserve any ignored names exactly as they arrived
  keep_idx <- x %in% ignore
  cc[keep_idx] <- x[keep_idx]
  
  # Promote acronyms using word boundaries
  for (ac in acronym) {
    # Convert lowerCamel '...Id' to '...ID' at word boundary
    pattern <- paste0(stringr::str_to_title(ac), "\\b")  # e.g., "Id\\b"
    cc <- stringr::str_replace_all(cc, pattern, ac)
  }
  cc
}

camelize_df_cols <- function(df, ignore = c("SNPL","TSN","QC")) {
  names(df) <- to_camel_case_with_exception(names(df), ignore = c("SNPL","TSN","QC"))
  df
}

# Executing function, altering for missed acronyms
df_Tables <- purrr::modify_if(
  df_Tables, is.data.frame,
  ~ camelize_df_cols(.x, ignore = c("SNPL","TSN","QC"))
)

df_Tables <- purrr::imap(
  df_Tables,
  function(df, nm) {
    df |> 
      dplyr::rename_with(~ stringr::str_replace(.x, "Qc", "QC"),
                         dplyr::contains("Qc")) |>  
      dplyr::rename_with(~ stringr::str_replace(.x, "^qc", "QC"),
                         dplyr::contains("qc")) |>  
      dplyr::rename_with(~ stringr::str_replace(.x, "^gpS", "GPS"),
                         dplyr::contains("GPS")) |>  
      dplyr::rename_with(~ stringr::str_replace(.x, "^snpl", "SNPL"),
                         dplyr::contains("snpl")) |> 
      dplyr::rename_with(~ stringr::str_replace(.x, "Id", "ID"),
                         dplyr::contains("Id"))
  }
)

#########################################
# Export cleaned datasets to CSV in the 'Input' directory of the Data Package Script location
#########################################

outPath <- paste0(outPutDir, "/SFAN_SNPL_Events.csv")
utils::write.csv(df_Tables[["Events"]], outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_Bands.csv")
utils::write.csv(df_Tables[["Bands"]], outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_ChickBands.csv")
utils::write.csv(df_Tables[["ChickBands"]], outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_Nesting.csv")
utils::write.csv(df_Tables[["Nesting"]], outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_Observations.csv")
utils::write.csv(df_Tables[["Observations"]], outPath, na = "", row.names = FALSE)

outPath <- paste0(outPutDir, "/SFAN_SNPL_Predators.csv")
utils::write.csv(df_Tables[["Predators"]], outPath, na = "", row.names = FALSE)

