The provided script is a part of the study titled "Improving Above Ground Biomass Estimation in Tropical Indian Forests" which has been submitted to "Ecological Informatics".

The script focuses on developing Height-Diameter (HD) models using field inventory data and comparing these models with existing Global H-D models. This is achieved by utilizing the BIOMASS R package.

After generating H-D models, tree heights are predicted using the tree diameter measured on the field. Then, the plot level biomass is estimated using various allometic models (FSI VOlume equation, and Pan Tropical models by Chave et al., 2014). 

Sample data sets are provided along with the script on the GitHub. Data are given in "India_height_dbh_8179.csv" & "India_Plot_AGB_Test.csv"  file for for HD modeling & Plot Biomass Estimation

Use the script "Script_India_HD_Model.R" for HD model and AGB Estimation. Files "TreeBiomass.R" & "PlotBiomass.R" will codes enable tree and plot biomass estimation using Forest Survey of India, FSI Volume equation. "Volume_Equations_Database_FSI.csv" file contains the volume equation database.
