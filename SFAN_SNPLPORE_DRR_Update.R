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

# Processing Date of the Data Package
processingDate <- '2025-05-01'

#Year Published
publishYear <- 2025

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



# Temporary Fix for Non-ISO date format in Resights DF
#dataframes$NPS_IMD_SFAN_Pinniped_Resights$StartDate <- as.Date(dataframes$NPS_IMD_SFAN_Pinniped_Resights$StartDate, format = "%m/%d/%Y")

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
#DistubranceStart
#DistubranceEnd
#ElephantStart
#ElephantEnd
#HarborStart
#HarborEnd
#RedSharkStart
#RedSharkEnd
#ResightStart
#ResightEnd


# Compute min and max years for each dataset
min_max_years <- lapply(dataframes, function(df) {
  # Convert StartDate to Date if not already
  df$StartDate <- as.Date(df$StartDate, format = "%Y-%m-%d")
  
  # Get min and max years
  min_year <- format(min(df$StartDate, na.rm = TRUE), "%Y")
  max_year <- format(max(df$StartDate, na.rm = TRUE), "%Y")
  
  # Return as a named list
  list(StartYear = min_year, EndYear = max_year)
})

# Assign names to the min_max_years list
names(min_max_years) <- names(dataframes)

# Print min and max years for each dataset
min_max_years


# Assign individual variables for each dataset
DistubranceStart <- min_max_years[["NPS_IMD_SFAN_Pinniped_Disturbance"]][["StartYear"]]
DistubranceEnd <- min_max_years[["NPS_IMD_SFAN_Pinniped_Disturbance"]][["EndYear"]]

ElephantStart <- min_max_years[["NPS_IMD_SFAN_Pinniped_ElephantSeal"]][["StartYear"]]
ElephantEnd <- min_max_years[["NPS_IMD_SFAN_Pinniped_ElephantSeal"]][["EndYear"]]

HarborStart <- min_max_years[["NPS_IMD_SFAN_Pinniped_HarborSeal"]][["StartYear"]]
HarborEnd <- min_max_years[["NPS_IMD_SFAN_Pinniped_HarborSeal"]][["EndYear"]]

RedSharkStart <- min_max_years[["NPS_IMD_SFAN_Pinniped_HarborSealRedShark"]][["StartYear"]]
RedSharkEnd <- min_max_years[["NPS_IMD_SFAN_Pinniped_HarborSealRedShark"]][["EndYear"]]

ResightStart <- min_max_years[["NPS_IMD_SFAN_Pinniped_Resights"]][["StartYear"]]
ResightEnd <- min_max_years[["NPS_IMD_SFAN_Pinniped_Resights"]][["EndYear"]]

# Print individual variables
paste("Disturbance Start:", DistubranceStart, "End:", DistubranceEnd)
paste("Elephant Seal Start:", ElephantStart, "End:", ElephantEnd)
paste("Harbor Seal Start:", HarborStart, "End:", HarborEnd)
paste("Red Shark Start:", RedSharkStart, "End:", RedSharkEnd)
paste("Resights Start:", ResightStart, "End:", ResightEnd)


#############################################
# Calculate the Number of Records per dataset
# DistNum, ElephantNum, HarborNum, RedSharkNum, ResightNum
# Get record count for each dataset
record_counts <- sapply(dataframes, nrow)

# Print record counts for each dataset
record_counts

# Optionally, assign to individual variables
DistNum <- record_counts["NPS_IMD_SFAN_Pinniped_Disturbance"]
ElephantNum <- record_counts["NPS_IMD_SFAN_Pinniped_ElephantSeal"]
HarborNum <- record_counts["NPS_IMD_SFAN_Pinniped_HarborSeal"]
RedSharkNum <- record_counts["NPS_IMD_SFAN_Pinniped_HarborSealRedShark"]
ResightNum <- record_counts["NPS_IMD_SFAN_Pinniped_Resights"]

# Print individual record counts
paste("Disturbance Records:", DistNum)
paste("Elephant Seal Records:", ElephantNum)
paste("Harbor Seal Records:", HarborNum)
paste("Red Shark Records:", RedSharkNum)
paste("Resight Records:", ResightNum)


########################
# Apply Harvested values back to the DRR Template
########################

#Copy Template file to output folder
templateName <- paste0('SFAN_Pinniped_DRR_', beginYear, '-', endYear, '.docx')

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
# Open the copied template
doc <- read_docx(outTemplateFull)

# Replace placeholders with harvested values

referencecodeDRRFull <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",referencecodeDRR)
doc <- body_replace_all_text(doc, "REFERENCECODEDRR", as.character(referencecodeDRRFull))

doc <- body_replace_all_text(doc, "DISTNUM", as.character(DistNum))
doc <- body_replace_all_text(doc, "ELEPHANTNUM", as.character(ElephantNum))
doc <- body_replace_all_text(doc, "HARBORNUM", as.character(HarborNum))
doc <- body_replace_all_text(doc, "REDSHARKNUM", as.character(RedSharkNum))
doc <- body_replace_all_text(doc, "RESIGHTNUM", as.character(ResightNum))
doc <- body_replace_all_text(doc, "BEGINYEAR", as.character(beginYear))
doc <- body_replace_all_text(doc, "ENDYEAR", as.character(endYear))

referencecodeDBFull <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",referencecodeDB)
doc <- body_replace_all_text(doc, "CODEDB", as.character(referencecodeDBFull))

doc <- body_replace_all_text(doc, "PUBYEAR", as.character(publishYear))

referencecodeDPFull <- paste0("https://doi.org/10.57830/",referencecodeDP)
doc <- body_replace_all_text(doc, "CODEDP", as.character(referencecodeDPFull))

doc <- body_replace_all_text(doc, "PROCESSINGDATE", processingDate)
doc <- body_replace_all_text(doc, "DISTURBANCESTART", as.character(DistubranceStart))
doc <- body_replace_all_text(doc, "DISTURBANCEEND", as.character(DistubranceEnd))
doc <- body_replace_all_text(doc, "ELEPHANTSTART", as.character(ElephantStart))
doc <- body_replace_all_text(doc, "ELEPHANTEND", as.character(ElephantEnd))
doc <- body_replace_all_text(doc, "HARBORSTART", as.character(HarborStart))
doc <- body_replace_all_text(doc, "HARBOREND", as.character(HarborEnd))
doc <- body_replace_all_text(doc, "REDSHARKSTART", as.character(RedSharkStart))
doc <- body_replace_all_text(doc, "REDSHARKEND", as.character(RedSharkEnd))
doc <- body_replace_all_text(doc, "RESIGHTSTART", as.character(ResightStart))
doc <- body_replace_all_text(doc, "RESIGHTEND", as.character(ResightEnd))


# Save the updated document
print(doc, target = outTemplateFull)

# Verify if the document was saved successfully
if (file.exists(outTemplateFull)) {
  print("Template updated successfully!")
} else {
  print("Failed to update the template.")
}





