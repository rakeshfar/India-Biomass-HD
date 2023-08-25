
# This code is generated as part of the study 
# "Improving Above Ground Biomass Estimation in Tropical Indian Forests"
# Submitted to Ecological Informatics

# Install the BIOMASS package for biomass analysis
#install.packages("BIOMASS") - Install if this package is not available

# Load the required packages
require(BIOMASS)

# Part-1 (H-D Model Development at Different Ecological Scales) ----

# Download the Shared Field Inventory data over 4 study sites covering , 51 sample plots and 8179 Height-Diameter Measurement.

# Data are given in "India_height_dbh_8179.csv" file for HD modelling

# Data are given in "India_Plot_AGB_Test.csv" for Plot biomass Estimation

# Load the field_inv_dataset dataset and display its structure

setwd("\\Github") ##Define Directory Here

HD_Data<-read.csv("India_height_dbh_8179.csv")



# HD Model - Pan Tropical India Model (all sites)
str(HD_Data)

HDmodel <- modelHD(D= HD_Data$DBH,  # Generate 4 different H-D models 
                   H = HD_Data$HT,
                   drawGraph=TRUE,
                   useWeight=TRUE)

# print the RSE and bias for differet form of H-D Model.
print(HDmodel)#select the HD model with lowest Bias (in this case "log2" model)



# Compute the HDmodel using the modelHD function with the provided dataset
HDmodel <- modelHD(
  D = HD_Data$DBH,
  H = HD_Data$HT,
  method = "log2",
  useWeight = TRUE
)
HDmodel

# Retrieve H values using the retrieveH function based on the HDmodel
Predicted_ht_India <- retrieveH(
  D = HD_Data$DBH,
  model = HDmodel
)


HD_Data$H_India=Predicted_ht_India$H
plot(HD_Data$HT, HD_Data$H_India, xlab="Observed HEight", ylab="Model Predicted Height")


# Site Specific Model HD Model ( Developed at 4 study areas of this study)

HDmodel_Site <- modelHD(D= HD_Data$DBH,
                   H = HD_Data$HT,
                   drawGraph=TRUE,
                   useWeight=TRUE,
                   plot=HD_Data$Site)           #selecting each site as separate entity
# print the RSE and bias for different form of H-D Model.
print(HDmodel_Site)

HDmodel_Site <- modelHD(D= HD_Data$DBH, #select the HD model with lowest Bias (in this case "log2" model)
                   H = HD_Data$HT,
                   useWeight=TRUE,
                   method = "log2",
                   plot=HD_Data$Site)

# Retrieve H values using the retrieveH function based on the Site specific H-D models
Predicted_ht_Site <- retrieveH(
  D = HD_Data$DBH,
  model = HDmodel_Site,
  plot=HD_Data$Site
)

HD_Data$H_Site=Predicted_ht_Site$H

# Type Specific Model

HDmodel_Type <- modelHD(D= HD_Data$DBH,
                   H = HD_Data$HT,
                   drawGraph=TRUE,
                   useWeight=TRUE,
                   plot=HD_Data$Type)     #selecting each forest as separate entity
# print the RSE and bias for differet form of H-D Model.
print(HDmodel_Type)

HDmodel_Type <- modelHD(D= HD_Data$DBH,#select the HD model with lowest Bias (in this case "log2" model)
                   H = HD_Data$HT,
                   method = "log2",
                   useWeight=TRUE,
                   plot=HD_Data$Type)



# Retrieve H values using the retrieveH function based on the Forest Type specific H-D models
Predicted_ht_Type <- retrieveH(
  D = HD_Data$DBH,
  model = HDmodel_Type,
  plot=HD_Data$Type
)

HD_Data$H_Type=Predicted_ht_Type$H


## Plot Specific Model, each plot should have at least 15 height measurement

HDmodel_Plot <- modelHD(D= HD_Data$DBH, # #select the HD model with lowest Bias (in this case "log2" model)
                        H = HD_Data$HT,
                        method = "log2",
                        useWeight=TRUE,
                        plot=HD_Data$Plot)

# Retrieve H values using the retrieveH function based on the plot specific H-D models
Predicted_ht_Plot <- retrieveH(
  D = HD_Data$DBH,
  model = HDmodel_Plot,
  plot=HD_Data$Plot
)

HD_Data$H_Plot=Predicted_ht_Plot$H


####------------------------------------------------------------------------------------------######

## Part 2- Comparing results with global HD Models
#Feldpausch and Chave H-D models are two models are are used by most of the researchers are compared with the our study H-D model over Indian region

#Feldpausch model Height retrievel

ht_feld <- retrieveH(
  D = HD_Data$DBH,
  region = "SEAsia"
)
HD_Data$Height_feld <- ht_feld$H

#Chave model Height retrievel
ht_chave  <- retrieveH(
  D = HD_Data$DBH,
  coord = HD_Data[, c("Site_Long", "Site_Lat")]
)
HD_Data$Height_Chave <- ht_chave$H

write.csv(HD_Data, "HD_Predicted_Height_allModel.csv")



## RSE of chave and feodpausch model over our study regions.
RSE <- function(data) {
  x <- data[[1]]
  y <- data[[2]]
  sqrt(mean((x - y)^2))
}

str(HD_Data)
#Site wise RMSE of Feldpausch model
rmse_Feld_Site <- by(HD_Data[c(4,13)], HD_Data$Site, RSE) #select the columns corresponding to measured and Feldpausch model height
data.frame(rmse_Feld_Site = unclass(rmse_Feld_Site))

#siteiwise RMSE of Chave Model
rmse_Chave_Site <- by(HD_Data[c(4,14)], HD_Data$Site, RSE) #select the columns corresponding t measured and Chave model height
data.frame(rmse_Chave_Site = unclass(rmse_Chave_Site))

#####-----------------------------------------------------------------------------######

## Part 3: Predicting Tree Height using current models

##Coefficint of HD model at differennt ecological scales

HD_Coefficient<-data.frame(a=c(0.81945926,-1.1627985,-1.0870371,0.76207650,0.16196203,-0.5865260,0.03213202,0.64929834),
                           b=c(0.69665526,1.8572892,1.6288981,0.73829211,1.12445731,1.5669076, 1.11368084,0.81435699),
                           c=c(-0.02450596,-0.1935178,-0.1373034,-0.03395961,-0.07737284,-0.1613784,-0.08312620,-0.03815683),
                           RSElog=c(0.2515676,0.1482391,0.2323825,0.2449285,0.2486913,0.1751584,0.2444775,0.2605924),
                           RSE=c(4.186436,2.376443,3.742279,3.905323,4.332427,2.792099,3.568174,4.492844),
                           row.names=c("Tropical India", "Betul", "Achanakmar", "Yellapur", "Uppangala", "Dry Deciduous", "Moist Deciduous", "Evergreen"))



HD_Coefficient


## H-D model for the tree height estimates using diameter measurements.

India_HD <- function(D, region) {             
  a <- HD_Coefficient[region, "a"]
  b <- HD_Coefficient[region, "b"]
  c <- HD_Coefficient[region, "c"]
  RSE <- HD_Coefficient[region, "RSE"]
  RSElog <- HD_Coefficient[region, "RSElog"]
  logD <- log(D)
  logH <- a + b * logD +c*(logD^2)
  H <- exp(logH + 0.5 * RSElog^2)
  output <- list(H = as.numeric(H), RSE = RSE)
}


# Predicting tree height for unknown trees
# select region for the model as per study area (India ,Betul, Achanakmar,Yellapur,Uppangala,Dry Deciduous,Moist Deciduous,Evergreen)

height_pred<-India_HD(D=HD_Data$DBH, region="Tropical India")

Predicted_height<-height_pred$H

plot(HD_Data$HT, height_pred$H)



####-----------------------------------------------------------------------------------#########

##### Part 4 : Estimating Tree Biomass using different AGB Models:

# input data
fielddata<-read.csv("India_Plot_AGB_Test.csv")
str(fielddata)


foo <- data.frame(do.call('rbind', strsplit(as.character(fielddata$SpeciesName),' ',fixed=TRUE)))
fielddata$genus = foo$X1
fielddata$Scientific = foo$X2


# Get wood density ------- From Web?
Taxo<-correctTaxo(genus=fielddata$genus,species=fielddata$Scientific)
fielddata$genusCorr<-Taxo$genusCorrected
fielddata$ScientificCorr<-Taxo$speciesCorrected

# Retrieving APG III Families and Orders from Genus names
APG<-getTaxonomy(fielddata$genusCorr, findOrder =T)
fielddata$familyAPG<-APG$family
fielddata$orderAPG<-APG$order

# RETRIEVE WOOD DENSITY
dataWD<-getWoodDensity(genus=fielddata$genusCorr,
                       species=fielddata$ScientificCorr,
                       stand=fielddata$PlotID,region="India")


# Get wood density ------- From Local sheet.
# update in volume equation DB and load the same to FSI.
VolEq_DB = read.csv("Volume_Equations_Database_FSI.csv")  ## Download the csv file containing the volume equation databse along with wood density data.
VolEq_DB[is.na(VolEq_DB)] <- 0

# FSI biomass ----
source("TreeBiomass.R") 
source("PlotBiomass.R") #These codes are provided for plot tree and plot level biomass estimation from FSI Volumetric equations

# Estimating tree biomass using FSI Volume equation and woood density
fsi_biomass = PlotBio(fielddata,VolEq_DB)
str(fsi_biomass)

fielddata$BA <- fsi_biomass$BA
fielddata$WD <- fsi_biomass$WD #from Indian database.

fielddata$wD1 <- dataWD$meanWD #from global database.
fielddata$sdwD1 <- dataWD$sdWD

fielddata$Vol_FSI <- fsi_biomass$Vol #Volume estiated using FSI volume equations
fielddata$AGB_FSI <- fielddata$Vol_FSI*fielddata$WD*1.575 # Provide wood densities for the species from available databases

# HD Model - Using Chave height based model for tree biomass estimation
# this require first estimating tree height for all tress for which height was not measured on ground.
# Here, either one can produce H-D model based on the Height estimated on the plots or use the H-D model developed above at different scale as part of this study.
# 


HDmodel_pred_Plot <- modelHD(D= fielddata$DBH,  #developing HD model using measured height
                        H = fielddata$Measured_H,
                        method = "log2",
                        useWeight=TRUE,
                        plot=fielddata$PlotID)

# Retrieve H values using the retrieveH function based on above model
Predicted_ht <- retrieveH(
  D = fielddata$DBH,
  model = HDmodel_pred_Plot,
  plot=fielddata$PlotID
)




fielddata$H_Plot<-Predicted_ht$H



# Predict height using our study model
height_pred<-India_HD(D=fielddata$DBH, region=fielddata$Forest_Type) ## India_HD model defined above section

fielddata$HT_TypeModel<-height_pred$H


#retaining measured height for the tree for which height was measured on field

fielddata$Ht_final_Plot <- fielddata$H_Plot
fielddata$Measured_H <- as.numeric(fielddata$Measured_H)
H_meas_check <- which(!is.na(fielddata$Measured_H))
fielddata$Ht_final_Plot[H_meas_check] <- fielddata$Measured_H[H_meas_check]


# Plot predicted height

plot(fielddata$Measured_H,fielddata$H_Plot,xlim=c(0,40),ylim=c(0,40),pch=20,col="gray",
     xlab="Height Field Measured [m]",ylab="Height Model Predicted [m]",main="H-D Model Predicted Height+")



# AGB Estimation ----
# Chave Biomass - without height
fielddata$AGB_CH_NH <- computeAGB(D=fielddata$DBH,WD=fielddata$WD,coord =cbind(fielddata$Long, fielddata$Lat)) #AGB_CH_NH is chave without height

# Chave Biomass - With height
fielddata$AGB_CH_H <- computeAGB(D=fielddata$DBH,WD=fielddata$WD,H=fielddata$Ht_final_Plot)

# Plot Level AGB ----
plotbiomass = setNames(aggregate(cbind(fielddata$BA,fielddata$AGB_FSI,fielddata$AGB_CH_NH,fielddata$AGB_CH_H),by=list(fielddata$PlotID),FUN="sum",na.rm=TRUE),
                       c("PlotID","BA","AGB_FSI","AGB_CH_NH","AGB_CH_H"))

#write.csv(plotbiomass,"AGB_output_betul.csv")
write.csv(fielddata,"AGB_output_treewise.csv",row.names=F)
write.csv(plotbiomass,"AGB_output_plotwise.csv",row.names=F)









