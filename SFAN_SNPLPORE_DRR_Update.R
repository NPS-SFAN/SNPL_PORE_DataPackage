#######################################################################
## Project: SFAN PORE DRR Update.R
## Script Updates the Snowy Plover PORE Data Release Report for the annual pinnipeds
## data package
## Date: 2025-05-01 
## Authors: Kirk Sherrill
## R version 4.4.0 
#######################################################################

#clear the environment
rm(list = ls())
cat("\014")
# use for installing packages when connected to network/VPN
options(download.file.method = "wininet")

library(officer)
#install.packages("magrittr") # Install if not already installed
library(magrittr)           # Load for the pipe operator `%>%`

##################
# Hard coded values Below
##################

# Data Package Data Store Reference Code
referencecodeDP <- 2310749

# Database harvested for the Datasets/Data Packages Reference Code 
referencecodeDB <- 2310672

# Data Release Report Data Store Reference Code
referencecodeDRR <- 2310766

# Data Processing Script Reference Code
referencecodeDPS <- 2311067

# Processing Date of the Data Package
processingDate <- '2025-05-01'

# Year Published
publishYear <- 2025

# SNPL PORE Backend Database with the Datasets to be preprocssed
db_name <- "C:/Users/KSherrill/OneDrive - DOI/SFAN/VitalSigns/SnowyPlovers_PORE/SNPLOVER/SNPL_IM/Data/Database/Dbase_BE/PORE_SNPL_BE_20250430.accdb"

# Dataset/Data Package Files Path
dsFilePath <- "C:/Users/KSherrill/OneDrive - DOI/SFAN/VitalSigns/SnowyPlovers_PORE/Scripts/SNPL_PORE_DataPackage/Data/SNPLPORE_2024_2310749"

# Template File Full Path
templateFileFull <- "C:/Users/KSherrill/OneDrive - DOI/SFAN/VitalSigns/SnowyPlovers_PORE/Scripts/SNPL_PORE_DataPackage/SFAN_SNPLPORE_DRR_Template.docx"

# Output Directory this is where the updated DRR Template will be exported
outDir <- "C:/Users/KSherrill/OneDrive - DOI/SFAN/VitalSigns/SnowyPlovers_PORE/SNPLOVER/SNPL_IM/Data/Deliverable/2024/DRR_2310766"

##################
# Hard coded values End
##################

###################
# Load Data or Data Package
###################

devtools::install_github('nationalparkservice/NPSutils')
## In Theory the NPSutils tool will import from a data package. Not Working will do via manual upload
# # Import the Data Package from IRMA
# NPSutils::get_data_packages(<<referencecodeDP>>, secure=TRUE, path=outDir)
# 
# dataDir = paste0(outDir, '/data')
# # Read in the Data sets
# dat <- NPSutils::load_data_package(<<referencecodeDP>>, directory=dataDir)
# list2env(dat, envir = .GlobalEnv)


data_files_wSuffix <-list.files(dsFilePath,pattern = "\\.csv$")
data_files <- sub("\\.csv$", "", data_files_wSuffix)

data_filepaths <- list.files(dsFilePath, pattern = "\\.csv$", full.names = TRUE)
dataframes <- lapply(data_filepaths,function(i){read.csv(i, header=TRUE)})
# Assign names to the dataframes list for easier access
names(dataframes) <-data_files


# Access individual dataframes
bands_df <- dataframes[["SFAN_SNPL_Bands"]]
chickbands_df <- dataframes[["SFAN_SNPL_ChickBands"]]
events_df <- dataframes[["SFAN_SNPL_Events"]]
nesting_df <- dataframes[["SFAN_SNPL_Nesting"]]
observations_df <- dataframes[["SFAN_SNPL_Observations"]]
predators_df <- dataframes[["SFAN_SNPL_Predators"]]

# Extract min and max dates from the Dataframes in the dataframes list
all_dates <- unlist(lapply(dataframes, function(df) {
  # Convert StartDate to Date if not already
  as.Date(df$Start_Date)
}))

# Ensure all_dates is of Date class
all_dates <- as.Date(all_dates)

# Compute overall min and max dates
beginYear <- format(min(all_dates, na.rm = TRUE), "%Y")
endYear <- format(max(all_dates, na.rm = TRUE), "%Y")

paste ('Start Year All Datasets:', beginYear)
paste ('End Year All Datasets:', endYear)

# Define individual Min and Max Yearly Values
#bandsStart
#bandsEnd
#chickbandsStart
#chickbandsEnd
#eventStart
#eventEnd
#nestingStart
#nestingEnd
#observationStart
#observationEnd
#predatorStart
#predatorEnd

# Bands Start - End
bands_df$Start_Date <- as.Date(bands_df$Start_Date, format = "%Y-%m-%d")
bandsStart <- format(min(bands_df$Start_Date, na.rm = TRUE), "%Y")
bandsEnd <- format(max(bands_df$Start_Date, na.rm = TRUE), "%Y")

# ChickBands Start - End
chickBandsStart <- min(chickbands_df$Year, na.rm = TRUE)
chickBandsEnd <- max(chickbands_df$Year, na.rm = TRUE)

# Events Start - End
events_df$Start_Date <- as.Date(events_df$Start_Date, format = "%Y-%m-%d")
eventsStart <- format(min(events_df$Start_Date, na.rm = TRUE), "%Y")
eventsEnd <- format(max(events_df$Start_Date, na.rm = TRUE), "%Y")

# Nesting Start - End
nesting_df$Date_Found <- as.Date(nesting_df$Date_Found, format = "%Y-%m-%d")
nestingStart <- format(min(nesting_df$Date_Found, na.rm = TRUE), "%Y")
nestingEnd <- format(max(nesting_df$Date_Found, na.rm = TRUE), "%Y")

# Observation Start - End
observations_df$Start_Date <- as.Date(observations_df$Start_Date, format = "%Y-%m-%d")
obsStart <- format(min(observations_df$Start_Date, na.rm = TRUE), "%Y")
obsEnd <- format(max(observations_df$Start_Date, na.rm = TRUE), "%Y")

# Predator Start - End
predators_df$Start_Date <- as.Date(predators_df$Start_Date, format = "%Y-%m-%d")
predStart <- format(min(predators_df$Start_Date, na.rm = TRUE), "%Y")
predEnd <- format(max(predators_df$Start_Date, na.rm = TRUE), "%Y")


# Print individual variables
paste("Bands Start:", bandsStart, "End:", bandsEnd)
paste("Chick Band Start:", chickBandsStart, "End:", chickBandsEnd)
paste("Events Start:", eventsStart, "End:", eventsEnd)
paste("Nesting Start:", nestingStart, "End:", nestingEnd)
paste("Observations Start:", obsStart, "End:", obsEnd)
paste("Predator Start:", predStart, "End:", predEnd)

#############################################
# Calculate the Number of Records per dataset

# Get record count for each dataset
record_counts <- sapply(dataframes, nrow)

# Print record counts for each dataset
record_counts

# Optionally, assign to individual variables
BandsNum <- record_counts["SFAN_SNPL_Bands"]
ChickBandsNum <- record_counts["SFAN_SNPL_ChickBands"]
EventNum <- record_counts["SFAN_SNPL_Events"]
NestingNum <- record_counts["SFAN_SNPL_Nesting"]
ObsNum <- record_counts["SFAN_SNPL_Observations"]
PredNum <- record_counts["SFAN_SNPL_Predators"]


# Print individual record counts
paste("Band Records:", BandsNum)
paste("Chick Band Records:", ChickBandsNum)
paste("Event Records:", EventNum)
paste("Nesting Records:", NestingNum)
paste("Observation Records:", ObsNum)
paste("Predator Records:", PredNum)


#############################################
# Get List of Realized QC Flags from datasets
# Check tlu_DataFlags - and field DefinedDRR to see if new fields
# need to added to the Table 2 Table in the DRR.
#############################################

# Combine all QC flag fields into one character vector
all_flags_raw <- c(
  as.character(bands_df$QCFlag),
  as.character(chickbands_df$QCFlag),
  as.character(events_df$EventDetailsQCFlag),
  as.character(events_df$EventQCFlag),
  as.character(nesting_df$QCFlag),
  as.character(observations_df$QCFlag),
  as.character(predators_df$QCFlag)
)

# Remove NA values and ""
all_flags_raw <- all_flags_raw[!is.na(all_flags_raw) & all_flags_raw != ""]

# Split concatenated flags by ";" and flatten the list
all_flags_split <- unlist(strsplit(all_flags_raw, ";"))

# Trim whitespace (optional, in case flags like " DFO" exist)
all_flags_split <- trimws(all_flags_split)

# Get unique flags
unique_flags <- unique(all_flags_split)

# Export to dataframe
realizedFlags_df <- data.frame(QCFlags = unique_flags)

# Count of Realized Flags
countrealizedFlags_df <- dim(realizedFlags_df)[1]


# Import the Flag Lookup table
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


query <- list(events = "SELECT tlu_DataFlags.* FROM tlu_DataFlags")

df_Tables <- lapply(query, getDataExport_Access.function, db_name = db_name)

df_luFlags <- df_Tables[[1]]

# Subset to only DefineDRR = 'Yes' - These are the Flags defined already in the DRR
df_luFlags_DRR <- df_luFlags[df_luFlags$DefinedDRR == "Yes", ]

#Check for 
#First Outer Join of all realized on SpeciesCode
flags_DF_Both <- realizedFlags_df %>%
  left_join(
    df_luFlags_DRR %>% mutate(DRR_FlagsDefined = FlagCode),
    by = c("QCFlags" = "FlagCode")
  )

# Check if count of joined records equals number of records in uniqueBirds_DFCount if equal Taxonomic Template has a definition per taxon
countNotNull <- sum(!is.na(flags_DF_Both$DRR_FlagsDefined))

print(paste("Number of Matching Realized QC Flags in Dataset and the Data Release Report Table 2 is -", countNotNull))

if (countNotNull == countrealizedFlags_df) {
  print("Realized QC Flags are already defined in the DRR Table as defined in the tlu_DataFlags$DefinedDRR field attribute - no need to add new QC Flags they are already defined."
  )
  
} else {
  
  #Subset to Taxon in need of definition in Taxonomic Coverages Template
  flags_ToDefine <- flags_DF_Both %>%
    filter(is.na(DRR_FlagsDefined))
  
  outDFPath <- here::here(paste0("Input", "/FlagsToDefine.csv"))
  if (file.exists(outDFPath)) {
    file.remove(outDFPath)
    print(paste("Existing File - ", outDFPath, " - has been deleted."))
  }
  
  write.csv(flags_ToDefine, outDFPath)
  
  # Get Count of records that are Null - i.e. in need of definition
  countNull <- sum(is.na(flags_DF_Both$DRR_FlagsDefined))
  
  print(paste0("WARNING - there are - ", countNull, " - QC Records in need of definition in the DRR Table 2."))
  print(paste0("See Exported dataframe with Flags to be defined in the DRR Table 2 at: ", outDFPath))
} 

####################
# Extract File Sizes
####################

# Extract File Sizes
file_sizes <- sapply(data_filepaths, function(x) file.info(x)$size)

# File Size dataframe
file_sizes_df <- data.frame(
  filepath = data_filepaths,
  size_bytes = file_sizes,
  size_MB = round(file_sizes / (1024^2), 2)  # size in megabytes
)

bandsSize <- file_sizes_df$size_MB[1]
chickSize <- file_sizes_df$size_MB[2]
eventSize <- file_sizes_df$size_MB[3]
nestingSize <- file_sizes_df$size_MB[4]
obsSize <- file_sizes_df$size_MB[5]
predSize <- file_sizes_df$size_MB[6]

########################
# Apply Harvested values back to the DRR Template
########################

#Copy Template file to output folder
templateName <- paste0('SFAN_SNPL_DRR_', beginYear, '-', endYear, '.docx')

# Full path to the copied template
outTemplateFull <- paste0(outDir, '/', templateName) 


# Check if the file exists and delete it
if (file.exists(outTemplateFull)) {
  file.remove(outTemplateFull)
  cat("File deleted successfully.\n")
} else {
  cat("File does not exist.\n")
}


file.copy(from = templateFileFull, to = outTemplateFull)

# Verify copy success
if (file.exists(file.path(outDir, templateName))) {
  print("File copied successfully!")
} else {
  print("File copy failed.")}


###########################
# Open the copied template - replacing value's in order of occurence.
doc <- read_docx(outTemplateFull)

# Replace placeholders with harvested values

referencecodeDRRFull <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",referencecodeDRR)
doc <- body_replace_all_text(doc, "REFERENCECODEDRR", as.character(referencecodeDRRFull))

doc <- body_replace_all_text(doc, "BEGINYEAR", as.character(beginYear))
doc <- body_replace_all_text(doc, "ENDYEAR", as.character(endYear))

referencecodeDPFull <- paste0("https://doi.org/10.57830/",referencecodeDP)
doc <- body_replace_all_text(doc, "CODEDP", as.character(referencecodeDPFull))

referencecodeDBFull <- paste0("https://doi.org/10.57830/",referencecodeDB)
doc <- body_replace_all_text(doc, "CODEDB", as.character(referencecodeDBFull))

# File Size
doc <- body_replace_all_text(doc, "SBANDS", as.character(bandsSize))
doc <- body_replace_all_text(doc, "SCHICKS", as.character(chickSize))
doc <- body_replace_all_text(doc, "SEVENT", as.character(eventSize))
doc <- body_replace_all_text(doc, "SNEST", as.character(nestingSize))
doc <- body_replace_all_text(doc, "SOBS", as.character(obsSize))
doc <- body_replace_all_text(doc, "SPRED", as.character(predSize))

# Dataset Start - End Date
doc <- body_replace_all_text(doc, "BANDSSTART", as.character(bandsStart))
doc <- body_replace_all_text(doc, "BANDEND", as.character(bandsEnd))

doc <- body_replace_all_text(doc, "#2CS", as.character(chickBandsStart))
doc <- body_replace_all_text(doc, "#3CE", as.character(chickBandsEnd))

doc <- body_replace_all_text(doc, "EVENTSTART", as.character(eventsStart))
doc <- body_replace_all_text(doc, "EVENTEND", as.character(eventsEnd))

doc <- body_replace_all_text(doc, "NESTINGSTART", as.character(nestingStart))
doc <- body_replace_all_text(doc, "NESTINGEND", as.character(nestingEnd))

doc <- body_replace_all_text(doc, "OBSSTART", as.character(obsStart))
doc <- body_replace_all_text(doc, "OBSEND", as.character(obsEnd))

doc <- body_replace_all_text(doc, "PREDSTART", as.character(predStart))
doc <- body_replace_all_text(doc, "PREDEND", as.character(predEnd))


# Record Counts by Dataset
doc <- body_replace_all_text(doc, "NBANDS", as.character(BandsNum))
doc <- body_replace_all_text(doc, "NCHICKS", as.character(ChickBandsNum))
doc <- body_replace_all_text(doc, "NEVENTS", as.character(EventNum))
doc <- body_replace_all_text(doc, "NNESTS", as.character(NestingNum))
doc <- body_replace_all_text(doc, "NOBS", as.character(ObsNum))
doc <- body_replace_all_text(doc, "NPRED", as.character(PredNum))

# Data Package Code Only
doc <- body_replace_all_text(doc, "DPCODEONLY", as.character(referencecodeDP))

# Data processing script
referencecodeDPSFull <- paste0("https://doi.org/10.57830/",referencecodeDPS)
doc <- body_replace_all_text(doc, "#1DPS", as.character(referencecodeDPSFull))

# Save the updated document
print(doc, target = outTemplateFull)

# Verify if the document was saved successfully
if (file.exists(outTemplateFull)) {
  print("Template updated successfully!")
} else {
  print("Failed to update the template.")
}