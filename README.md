# SNPL_PORE_DataPackage
Snowy Plover Point Reyes Data Package processing scripts repository

## SFAN_SNPL_PORE_Preprocesing.R
Preprocessing script for Snowy Plover Point Reyes Datasets.  The preprocessed datasets will subsequently be added to a SNPL PORE data package
with accompanying Ecological Metadata Language (EML) standard XML metadata via the 'SFAN_SNPL_PORE_EML_Procesing.Rmd' srcipt.

## SFAN_SNPL_PORE_EML_Processing.Rmd
Script creates an Ecological Monitoring Landuage (EML) metadata standard file and uploads as a NPS Data Store Data Package Reference. Processing is done on the SNPL datasets that have been preprocessed in the 'SFAN_SNPL_PORE_Preprocesing.R' script. Output from the preprocessing scripts is in the 'Data/Input' folder
