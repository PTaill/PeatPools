library(ggplot2)
library(ggpubr)
library(pastecs)
library(cowplot)
library(grid)
library(gridExtra)
library(ggpmisc)
library(extrafont)
library(dplyr)
library(lubridate)
library(ggbreak) 
library(patchwork)
library(scales)
library(ggplot2)
library(ggpubr)

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 9") #Pierre
data <- read.csv('LiteratureReview_OpenWaterPeatlandPools.csv',header=TRUE, sep=",", skip=0, dec=".")

data$FCO2_mean_mgCm2d<-data$FCO2_mean_gCm2d*1000
data$FCH4_diffusion_mean_mgCm2d<-data$FCH4_diffusion_mean_gCm2d*1000
data$Pool_Depth_m_log<-log(data$Pool_Depth_m)+1
data$CO2_mean_mgCL_log<-log(data$CO2_mean_mgCL)
data$CH4_mean_mgCL_log<-log(data$CH4_mean_mgCL)
data$FCO2_mean_mgCm2d_log<-log(data$FCO2_mean_mgCm2d)
data$FCH4_diffusion_mean_mgCm2d_log<-log(data$FCH4_diffusion_mean_mgCm2d)
data$FCH4_ebulition_mean_gCm2d_log<-log(data$FCH4_ebulition_mean_gCm2d)
data$CO2CH4<-data$CO2_mean_mgCL/data$CH4_mean_mgCL
data$CO2CH4_log<-log(data$CO2CH4)+1
data$Pool_Depth_m_log<-log(data$Pool_Depth_m)+1
data$CO2_mean_mgCL_log<-log(data$CO2_mean_mgCL)+1
data$CH4_mean_mgCL_log<-log(data$CH4_mean_mgCL)+1
data$FCO2_mean_mgCm2d_log<-log(data$FCO2_mean_mgCm2d)+1
data$FCH4_diffusion_mean_mgCm2d_log<-log(data$FCH4_diffusion_mean_mgCm2d)+1
data$FCH4_ebulition_mean_gCm2d_log<-log(data$FCH4_ebulition_mean_gCm2d)+1
data$Latitude_abs<-abs(data$Latitude)
data$Pool_surface_m2_log<-log(data$Pool_surface_m2)

#building figures
my.formula <- y ~ x
a<-ggplot(data,aes(x=Pool_surface_m2_log, y = CO2_mean_mgCL_log))+
  geom_point(data, mapping = aes(x =Pool_surface_m2_log, y = CO2_mean_mgCL_log), color="#0072B2")+
  geom_smooth(method = "lm", se=T, color="grey", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), label.x = 1,
               parse = TRUE)+
  ylab(expression(paste("Log CO"[2]," (mg C L"^"-1",")")))+
  xlab(expression(paste("Log Pool Surface (m"^"-2",")")))+theme_tufte() +
  theme_tufte() +theme(axis.line = element_line(colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),axis.title = element_text(size=10),axis.text = element_text(color = "black",size=10),
                      legend.position = "none") +ggtitle("a)")+
  geom_point(data[data$Reference=="This study",], mapping = aes(x =Pool_surface_m2_log, y = CO2_mean_mgCL),shape=17, fill="black",size=2)
a

my.formula <- y ~ x
b<-ggplot(data,aes(x =Pool_surface_m2_log, y = CH4_mean_mgCL_log))+
  geom_point(data, mapping = aes(x =Pool_surface_m2_log, y = CH4_mean_mgCL_log), color="#009E73")+
  geom_smooth(method = "lm", se=T, color="grey", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), label.x = 1,
               parse = TRUE)+
  ylab(expression(paste("Log CH"[4]," (mg C L"^"-1",")")))+
  xlab(expression(paste("Log Pool Surface (m"^"-2",")")))+
  theme_tufte() +theme(axis.line = element_line(colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),axis.title = element_text(size=10),axis.text = element_text(color = "black",size=10),
                       legend.position = "none") +ggtitle("b)")+
  geom_point(data[data$Reference=="This study",], mapping = aes(x=Pool_surface_m2_log, y = CH4_mean_mgCL_log),shape=17, fill="black",size=2)
b

my.formula <- y ~ x
c<-ggplot(data,aes(x =Pool_surface_m2_log, y = CO2CH4_log))+
  geom_point(data, mapping = aes(x =Pool_surface_m2_log, y = CO2CH4_log), color="#E69F00")+
  geom_smooth(method = "lm", se=T, color="grey", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), label.x = 1,
               parse = TRUE)+
  ylab(expression(paste("Log CO"[2],":CH"[4]," concentration ratio")))+
  xlab(expression(paste("Log Pool Surface (m"^"-2",")")))+
  theme_tufte() +theme(axis.line = element_line(colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),axis.title = element_text(size=10),axis.text = element_text(color = "black",size=10),
                       legend.position = "none") +ggtitle("c)")+
  geom_point(data[data$Reference=="This study",], mapping = aes(x=Pool_surface_m2_log, y = CO2CH4_log),shape=17, fill="black",size=2)
c


my.formula <- y ~ x
d<-ggplot(data,aes(x =Pool_surface_m2_log, y = FCO2_mean_mgCm2d_log))+
  geom_point(data, mapping = aes(x =Pool_surface_m2_log, y = FCO2_mean_mgCm2d_log),color="#0072B2")+
  geom_smooth(method = "lm", se=T, color="grey", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label..,..p.value.label..,sep = "~~~")),label.x = 1, 
               parse = TRUE)+
  ylab(expression(paste("Log CO"[2]," diffusion (mg C m"^"-2"," d"^"-1",")")))+
  xlab(expression(paste("Log Pool Surface (m"^"-2",")")))+
  theme_tufte() +theme(axis.line = element_line(colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),axis.title = element_text(size=10),axis.text = element_text(color = "black",size=10),
                       legend.position = "none") +ggtitle("d)")+
  geom_point(data[data$Reference=="This study",], mapping = aes(x =Pool_surface_m2_log, y = FCO2_mean_mgCm2d_log),shape=17, fill="black",size=2)
d

my.formula <- y ~ x
e<-ggplot(data,aes(x =Pool_surface_m2_log, y = FCH4_diffusion_mean_mgCm2d_log))+
  geom_point(data, mapping = aes(x =Pool_surface_m2_log, y = FCH4_diffusion_mean_mgCm2d_log), color="#009E73")+
  geom_smooth(method = "lm", se=T, color="grey", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), label.x = 1,
               parse = TRUE)+
  ylab(expression(paste("Log CH"[4]," diffusion (mg C m"^"-2"," d"^"-1",")")))+
  xlab(expression(paste("Log Pool Surface (m"^"-2",")")))+
  theme_tufte() +theme(axis.line = element_line(colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),axis.title = element_text(size=10),axis.text = element_text(color = "black",size=10),
                       legend.position = "none") +ggtitle("e)")+
  geom_point(data[data$Reference=="This study",], mapping = aes(x =Pool_surface_m2_log, y = FCH4_diffusion_mean_mgCm2d_log),shape=17, fill="black",size=2)
e

data$FCH4_ebulition_mean_gCm2d
my.formula <- y ~ x
f<-ggplot(data,aes(x =Pool_surface_m2_log, y = FCH4_ebulition_mean_gCm2d_log))+
  geom_point(data, mapping = aes(x =Pool_surface_m2_log, y = FCH4_ebulition_mean_gCm2d_log), color="#009E73")+
  geom_smooth(method = "lm", se=T, color="grey", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),label.x = 1,  
               parse = TRUE)+
  ylab(expression(paste("Log CH"[4]," ebullition (mg C m"^"-2"," d"^"-1",")")))+
  xlab(expression(paste("Log Pool Surface (m"^"-2",")")))+
  theme_tufte() +theme(axis.line = element_line(colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),axis.title = element_text(size=10),axis.text = element_text(color = "black",size=10),
                       legend.position = "none") +ggtitle("f)")+
  geom_point(data[data$Reference=="This study",], mapping = aes(x =Pool_surface_m2_log, y = FCH4_ebulition_mean_gCm2d_log),shape=17, fill="black",size=2)
f

#offloading figures
tiff(("Figure9.tiff"), height = 18, width = 26, units = 'cm', compression = "lzw", res = 600)
ggarrange(a,b,c,d,e,f, ncol=3, nrow=2, common.legend =F)
dev.off()

