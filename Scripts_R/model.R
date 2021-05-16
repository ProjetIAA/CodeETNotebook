#========================================================================================
#================================ LOADING LIBRARIES =====================================
#========================================================================================

rm(list=ls(all=TRUE))

library(rgdal)
library(rJava)
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
library(ROCR)
library(vcd)
library(boot)
require(caTools)
library(dbplyr)
library(tmap)

#========================================================================================
#===================================== LOADING Data =====================================
#========================================================================================

#========================================================================================
#================================ RENDEMENT DATA ====================================
#========================================================================================
base_rdt <-  read.csv2("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 1/Catastrophic Risk/R/rdt_arachide1.csv",sep=",",dec = ".")
str(base_rdt)
colnames(base_rdt)
summary(base_rdt)

#========================================================================================
#================================ Predictors variables ==================================
#========================================================================================
covariables <- stack("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 2/IA for Actuarial/AI_for_Actuarial_project/Data/Predictors/covars_clim_sn.grd")
plot(covariables)
#========================================================================================
#================================ crop  density =========================================
#========================================================================================
rdt_arachide=base_rdt[,c("geopointlongitude","geopointlatitude","Rendement.kg.ha")]
vals_arachide<-raster::extract(covariables, rdt_arachide[,1:2])
head(vals_arachide)
vals_arachide=as.data.frame(vals_arachide)
vals_arachide$arachide=rdt_arachide[,3]


## Train and Test data for predictors
set.seed(10)
group <- kfold(vals_arachide, 5)
X_train <- vals_arachide[group != 1, ]
X_test <-  vals_arachide[group == 1, ]
###########################################################################################
library(caret)

########################## Random Forest Model (RF) #######################################

library(randomForest)
set.seed(1)

RF_arachide=randomForest(arachide~.,data=X_train,ntree=1000)
RF_arachide
y_pred=predict(RF_arachide,X_test)

RMSE_RF=sqrt(mean((log10(y_pred+1)-log10(X_test$arachide+1))^{2},na.rm = TRUE))
print(RMSE_RF)

RF_pred <- predict(covariables,RF_arachide)

varImpPlot(RF_arachide, sort=TRUE, n.var=19,
           type=NULL, class=NULL, scale=TRUE, 
           main="",font.main=4,pch = 19,font.lab=4,col="#8B0000") 


########################## Gradient Boosting Model (GBM) #######################################
set.seed(1)

# Set up the resampling, here repeated CV
tr <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# We use expand grid to create a table of all possible combinations
tg <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), 
                  interaction.depth = c(1, 3, 7, 10),
                  n.minobsinnode = c(2, 5, 10),
                  n.trees = c(100, 300, 500, 1000))

# Verbose is a parameter sent to the underlying modeling function
gbm_model <- train(arachide ~ ., data = X_train, 
                   method = "gbm", trControl = tr, tuneGrid =tg, verbose = FALSE)

y_gbm_pred=predict(gbm_model,X_test)

RMSE_gbm=sqrt(mean((log10(y_gbm_pred+1)-log10(X_test$arachide+1))^{2},na.rm = TRUE))
print(RMSE_gbm)
gbm_pred <- predict(covariables,gbm_model)


#### Mapping results of RF and gbm
abundance <- stack(gbm_pred,RF_pred)
names(abundance) <- c("Gradient Boosting","Random Forest")
tm_shape(abundance) + tm_raster( style="fixed",breaks = c(0,300,500,700,1000),title = "Legend",
                                 palette = colorRampPalette( c("red","orange","green"))(12)
)+ tm_legend(position = c("left", "bottom"),outside=TRUE)+  tm_layout(
  main.title.position = "center",
  main.title.color = "blue",
  scale=1,
  title.color = "red",
  panel.labels = c("Gradient Boosting","Random Forest"),
  panel.label.color = "black",
  legend.text.color = "black")

















