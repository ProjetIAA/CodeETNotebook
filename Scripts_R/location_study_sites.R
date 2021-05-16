
rm(list=ls(all=TRUE))

library(ggplot2)
library(reshape2)
library(dplyr)
library(rgdal)
library(scales)
library(stringr)
library(ggrepel)
library(broom)
library(ggsn)
library(sf)
library(ggpubr)

#========================================================================================
#================================ Cartographie de l'Afrique =============================
#========================================================================================

carte <- readOGR(dsn="C:/Users/Fallou NIAKH/Desktop/ordi_PaleBlu/STAGE_ISRA/R_memo/data/ShapeAfrica", layer = "Africa")
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# geographical, datum WGS84
# carte <- spTransform(carte, CRS=crs.geo)

basecarte <- tidy(carte)
carte@data$id <- rownames(carte@data)
basecarte <- left_join(basecarte, carte@data, by="id")
class(basecarte)
basecarte <- basecarte %>% 
mutate(estconcerne=ifelse(COUNTRY %in% c("Senegal"), 
                            "Non", 
                            "Oui"))

# max(basecarte$long)
# max(basecarte$lat)

C1 <- basecarte %>% 
  ggplot() +
  geom_polygon(aes(x=long, 
                   y=lat,
                   group=group,
                   fill=estconcerne),
               color="white", 
               size=0.75)+
  coord_fixed(1.2)+
  scale_fill_manual(values = c("darkorange", "gray"))+
  theme_bw()+
  north(basecarte,symbol=16) +
  scalebar(basecarte, dist = 1000, transform= TRUE,dist_unit = "km", model = 'WGS84',location = "bottomleft")+
  xlim(-18,51.41303)+ylim(-5,37.34962)+
theme(legend.position =  "none")# pour plus d'options voire===>la fonction northSymbols()

#========================================================================================
#================================ Cartographie du Senegal ===============================
#========================================================================================


carte <- readOGR(dsn="C:/Users/Fallou NIAKH/Desktop/ordi_PaleBlu/STAGE_ISRA/R_memo/data/SEN_adm", 
                 layer = "SEN_adm1")
plot(carte)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# geographical, datum WGS84
carte <- spTransform(carte, CRS=crs.geo)

basecarte <- tidy(carte)

carte@data$id <- rownames(carte@data)

basecarte <- left_join(basecarte, carte@data, by="id")

#Sélection des régions échantillonnées lors de la campagne de piégeage de 2012
basecarte <- basecarte %>% 
  mutate(estconcerne=ifelse(NAME_1 %in% c("Louga",
                                         "Diourbel","Fatick"), 
                            "Non", 
                            "Oui"))

C2 <- basecarte %>% 
  ggplot()+
  geom_polygon(aes(x=long, 
                   y=lat,
                   group=group,
                   fill=estconcerne),
               color="white", 
               size=0.75)+
  coord_fixed(1.2)+
  scale_fill_manual(values = c("darkorange", "gray"))+
  theme_bw()+
  north(basecarte,symbol=8)+
  scalebar(basecarte, dist = 100,transform = TRUE, model = "WGS84",location = "bottomright",dist_unit = "km")+theme(legend.position = "none")
# pour plus d'options voire===>la fonction northSymbols()

###Chargement des coordonnées GPS des sites échantillonnés
rdt <-  read.csv2("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 1/Catastrophic Risk/R/rdt_arachide1.csv",stringsAsFactors = FALSE, sep = "," ,dec = ".")

P1 <- C2+geom_point(data = rdt, aes(x = geopointlongitude, y=geopointlatitude),size=3)+
  geom_label_repel(data = rdt, aes(x = geopointlongitude, y=geopointlatitude, label=""),size=3)+
  coord_fixed(1.2)+
  #scale_fill_manual(values = c("darkorange", "wheat"))+
  theme_bw()+
  north(basecarte,symbol=8)+
  theme(legend.position = "none")

P1

figure <- ggarrange(C1,P1,
                    ncol = 2, nrow = 1)
figure

ggsave("C:/Users/Fallou NIAKH/Desktop/ENSAE_3A/Semestre 1/Catastrophic Risk/R/Map_paper1.png",plot=figure,dpi=300,width=30,height=20,units="cm")
