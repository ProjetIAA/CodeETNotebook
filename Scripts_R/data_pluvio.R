# R version 4.0.3
# Authors :
# Version: 0.0
# ======================================================================================
# =================== Projet d'intelligence artificielle pour l'actuariat ==============
# ======================================================================================


#========================================================================================
#================================ LOADING LIBRARIES =====================================
#========================================================================================
rm(list=ls(all=TRUE))

library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(mapdata)
library(dismo)
library(RColorBrewer)
library(adehabitatHS)
library(spatstat)
library(rasterVis)
library(colorspace)
library(gridExtra)
library(ggplot2)
library(latticeExtra)
library(tidyr)
library(gstat)
library(mgcv)

#========================================================================================
#===================================== LOADING CLIMATICAL Data ==========================
#========================================================================================

setwd("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project")

dem <- raster("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/DEM/modisdem250_sn.tif")
rr = raster(xmn=dem@extent[1],xmx=dem@extent[2],ymn=dem@extent[3],
            ymx=dem@extent[4],resolution=c(0.0025,0.0025))
dem <- resample(dem, rr, method = "ngb")


#===== CLIM DATA: WorldClim variables


annual_mean_temp<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_1_sn.tif")
annual_mean_temp <- resample(annual_mean_temp, rr, method = "ngb")

mean_diurnal_range<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_2_sn.tif")
mean_diurnal_range <- resample(mean_diurnal_range, rr, method = "ngb")

isothermality<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_3_sn.tif")
isothermality <- resample(isothermality, rr, method = "ngb")

temp_seasonality<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_4_sn.tif")
temp_seasonality <- resample(temp_seasonality, rr, method = "ngb")

max_temp_warmest_month<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_5_sn.tif")
max_temp_warmest_month <- resample(max_temp_warmest_month, rr, method = "ngb")

min_temp_coldest_month<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_6_sn.tif")
min_temp_coldest_month <- resample(min_temp_coldest_month, rr, method = "ngb")

temp_annual_range<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_7_sn.tif")
temp_annual_range <- resample(temp_annual_range, rr, method = "ngb")

mean_temp_wettest_quarter<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_8_sn.tif")
mean_temp_wettest_quarter <- resample(mean_temp_wettest_quarter, rr, method = "ngb")

mean_temp_driest_quarter<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_9_sn.tif")
mean_temp_driest_quarter <- resample(mean_temp_driest_quarter, rr, method = "ngb")

mean_temp_warmest_quarter<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_10_sn.tif")
mean_temp_warmest_quarter <- resample(mean_temp_warmest_quarter, rr, method = "ngb")

mean_temp_coldest_quarter<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_11_sn.tif")
mean_temp_coldest_quarter <- resample(mean_temp_coldest_quarter, rr, method = "ngb")

annual_precipitation<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_12_sn.tif")
annual_precipitation <- resample(annual_precipitation, rr, method = "ngb")

precipitation_of_wettest_month<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_13_sn.tif")
precipitation_of_wettest_month <- resample(precipitation_of_wettest_month, rr, method = "ngb")

precipitation_of_driest_month<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_14_sn.tif")
precipitation_of_driest_month <- resample(precipitation_of_driest_month, rr, method = "ngb")

precipitation_seansonality<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_15_sn.tif")
precipitation_seansonality <- resample(precipitation_seansonality, rr, method = "ngb")

precipitation_of_wettest_quarter<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_16_sn.tif")
precipitation_of_wettest_quarter <- resample(precipitation_of_wettest_quarter, rr, method = "ngb")

precipitation_of_driest_quarter<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_17_sn.tif")
precipitation_of_driest_quarter <- resample(precipitation_of_driest_quarter, rr, method = "ngb")

precipitation_of_warmest_quarter<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_18_sn.tif")
precipitation_of_warmest_quarter <- resample(precipitation_of_warmest_quarter, rr, method = "ngb")

precipitation_of_coldest_quarter<-stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/BIOCLIM_SN/edwc1k2000bio_19_sn.tif")
precipitation_of_coldest_quarter <- resample(precipitation_of_coldest_quarter, rr, method = "ngb")

covariables <- stack(annual_mean_temp,mean_diurnal_range,isothermality,temp_seasonality,
                     max_temp_warmest_month,min_temp_coldest_month,temp_annual_range,
                     mean_temp_wettest_quarter,mean_temp_driest_quarter,mean_temp_warmest_quarter,
                     mean_temp_coldest_quarter,annual_precipitation,precipitation_of_wettest_month,
                     precipitation_seansonality,
                     precipitation_of_wettest_quarter,precipitation_of_driest_quarter,
                     precipitation_of_warmest_quarter,precipitation_of_coldest_quarter)

names(covariables)=c("Clim01","Clim02","Clim03","Clim04",
                     "Clim05","Clim06","Clim07","Clim08",
                     "Clim09","Clim10","Clim11","Clim12",
                     "Clim13","Clim15","Clim16",
                     "Clim17","Clim18","Clim19")

plot(covariables)

# enregistrement du stack
writeRaster(covariables,
            filename = "C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/covars_clim_sn.grd",
            overwrite = TRUE)

#========================================================================================
#===================================== LOADING RETURNS Data ============================
#========================================================================================

rdt <-  read.csv2("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Returns/base_brutes.csv",stringsAsFactors = FALSE, dec = ".",sep="," )
rdt_arachide <- subset(rdt, culture=="Arachide")

rdt_arachide1 <- write.csv(x=rdt_arachide, file="C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Retruns/rdt_arachide1.csv")

# ==========================================================================================================
# ============================================= end of analysis ============================================
# ==========================================================================================================
