# Load packages
library(dplyr)
library(lubridate)
library(pastecs)
library(ggplot2)
library(ggplot2)
library(ggpubr)
library(pastecs)
library(cowplot)
library(pastecs)
library(grid)
library(gridExtra)
library(pastecs)
library(chron)
library(hms)
library(scales)
library(ggthemes)
library(EnvStats)
library(ggridges)

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 10")
data<- read.csv('Figure11ab.csv',header=TRUE, sep=",", dec=".")

head(data)

data$co2_mgCL<-data$CO2_umolL*12.0107/1000
data$ch4_mgCL<-data$CH4_umolL*12.0107/1000
data$FCO2_mmolm2d1<-ifelse(data$area_km2<0.001,data$CO2_umolL*0.36,ifelse(data$area_km2>=0.001&data$area_km2<0.01,data$CO2_umolL*0.48,ifelse(data$area_km2>=0.01&data$area_km2<0.1,data$CO2_umolL*0.57,NA)))
data$FCO2_mgCm2d1<-data$FCO2_mmolm2d1*12.0107

data$co2ch4<-data$co2_mgCL/data$ch4_mgCL
data<-subset(data,(data$area_km2<=0.1))

#building figures
n_fun <- function(x){
  return(data.frame(y = -2,
                    label = paste0("n = ",length(x))))
}
f1 <- function(x) {
 log10(mean(10 ^ x)) 
}
a<-ggplot(data, aes(x=factor(Category, level=c('Small ponds', 'Peatland pools*', 'Thermokarst ponds')), y=co2_mgCL,fill =factor(Category, level=c('Small ponds', 'Peatland pools*', 'Thermokarst ponds')))) +
  geom_violin(trim = FALSE, color = NA,alpha=0.8) +
  #scale_fill_viridis_d(option="turbo",alpha=0.8)+ 
  scale_fill_manual(values = c("grey","#482677FF","pink"))+ 
  geom_boxplot(width = 0.2,outlier.shape = NA)+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),axis.title.x = element_blank(),axis.text.y = element_text(color = "black",size=7,hjust = 1),
                       axis.title.y=element_text(size=8), legend.position = "none",
                       axis.text.x = element_text(color = "black",size = 7, angle = 25,hjust = 1)) +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=2)+
  ylab(expression(paste("CO"[2]," (mg C L"^"-1",")")))+annotation_logticks(sides = "l")+ggtitle("a)")+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )
a  

n_fun <- function(x){
  return(data.frame(y = -5,
                    label = paste0("n = ",length(x))))
}
f1 <- function(x) {
  log10(mean(10 ^ x)) 
}
b<-ggplot(data, aes(x=factor(Category, level=c('Small ponds', 'Peatland pools*', 'Thermokarst ponds')), y=ch4_mgCL,fill = factor(Category, level=c('Small ponds', 'Peatland pools*', 'Thermokarst ponds')))) +
geom_violin(trim = FALSE, color = NA,alpha=0.8) +
  geom_boxplot(width = 0.2,outlier.shape = NA)+
  scale_fill_manual(values = c("grey","#482677FF","pink"))+ 
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),axis.title.x = element_blank(),axis.text.y = element_text(color = "black",size=7,hjust = 1),
                       axis.title.y=element_text(size=8), legend.position = "none",
                       axis.text.x = element_text(color = "black",size = 7, angle = 25,hjust = 1)) +
#  stat_summary(fun.y=f1, geom="point", shape=20, size=7, color="black", fill="black")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=2)+
  ylab(expression(paste("CH"[4]," (mg C L"^"-1",")")))+annotation_logticks(sides = "l")+ggtitle("b)")+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )
b  

data<- read.csv('Figure11c.csv',header=TRUE, sep=",", dec=".")

skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Rivers"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Peatland pools"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Lakes"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Reservoirs"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Aquaculture"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Estuaries"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Salt marshes"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Mangroves"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Seagrasses"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Tidal flats"], method = "moment")
skewness(data$total_CH4_flux_mgCm.2d.1[data$WaterBody=="Continental shelf"], method = "moment")

data$WaterBody<-ifelse(data$WaterBody=="Peatland pools","Peatland pools*",data$WaterBody)

#building figures
n_fun <- function(x){
  return(data.frame(y = -3.5,
                    label = paste0("n = ",length(x))))
}
f1 <- function(x) {
  log10(mean(10 ^ x)) 
}
c<-ggplot(data, aes(x=factor(WaterBody, level=c('Rivers', 'Peatland pools*', 'Lakes','Reservoirs', "Aquaculture",
                                                "Estuaries","Salt marshes","Mangroves","Seagrasses","Tidal flats","Continental shelf")), y=total_CH4_flux_mgCm.2d.1,fill = factor(WaterBody, level=c('Rivers', 'Peatland pools*', 'Lakes','Reservoirs', "Aquaculture",
                                                                                                                                                                                                     "Estuaries","Salt marshes","Mangroves","Seagrasses","Tidal flats","Continental shelf")))) +geom_violin(trim = FALSE, color = NA) +
  geom_boxplot(width = 0.2,outlier.shape = NA)+
  scale_fill_viridis_d(alpha=0.8)+ 
  scale_y_continuous(trans='log10')+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),axis.title.x = element_blank(),axis.text.y = element_text(color = "black",size=7,hjust = 1),
                       axis.title.y=element_text(size=8), legend.position = "none",
                       axis.text.x = element_text(color = "black",size = 7, angle = 25,hjust = 1)) +
#  stat_summary(fun.y=f1, geom="point", shape=20, size=7, color="black", fill="black")+ggtitle("c)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=2)+
  ylab(expression(paste("Methane flux (mg C m"^"-2"," d"^"-1",")")))+annotation_logticks(sides = "l")+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )
c  

#offloading figures
tiff(("Figure10.tiff"), height = 20, width = 22, units = 'cm', compression = "lzw", res = 600)
ggarrange(ggarrange(a,b, ncol = 2),c,nrow = 2) 
dev.off()
