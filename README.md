# SNPL_PORE_DataPackage
San Francisco Bay Area Network Inventory and Monitoring Network Snowy Plover Monitoring at Point Reyes National Seashore Data Management Data Package processing scripts repository.

## SFAN_SNPL_PORE_Preprocesing.R
Preprocessing script for Snowy Plover Point Reyes Datasets.  The preprocessed datasets will subsequently be added to a SNPL PORE data package
with accompanying Ecological Metadata Language (EML) standard XML metadata via the 'SFAN_SNPL_PORE_EML_Procesing.Rmd' srcipt.

## SFAN_SNPL_PORE_EML_Processing.Rmd
Script creates an Ecological Monitoring Landuage (EML) metadata standard file and uploads as a NPS Data Store Data Package Reference. Processing is done on the SNPL datasets that have been preprocessed in the 'SFAN_SNPL_PORE_Preprocesing.R' script. Output from the preprocessing scripts will be exported to the 'Data/Input' folder.

## SFAN_SNPLPORE_DRR_Update.R
Script updates the Snowy Plover PORE Data Release Report Template (i.e. SFAN_SNPLPORE_DRR_Template.docx) for the annual Data Release Report that accompanies the annual Data Package release.

## SFAN_SNPLPORE_DRR_Template.docx
Data Release Report word document template. This template file is updated via the 'SFAN_SNPLPORE_DRR_Update.R' script to reflect the datasets information such as years, record counts, realized flags etc. released in the data package being released.
