library(ggplot2)
library(gridExtra)
library(ggpmisc)
library(extrafont)
library(ggbreak) 
library(patchwork)
library(dplyr)
library(lubridate)
library(pastecs)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(chron)
library(hms)
library(scales)
library(ggthemes)
library(svglite)

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 3")
data<- read.csv('Figure3_Figure7.csv',header=TRUE, sep=",", dec=".")

#building figures
a<-ggplot(data,aes(x = Pool_size_m2, y = CO2_mgCL,color= SiteID)) +geom_dotplot(binaxis='y', stackdir='center',stackratio=.8, dotsize=.8,fill = "grey70")+
  geom_pointrange(data, mapping = aes(x = Pool_size_m2, y = CO2_mgCL,color = SiteID),
                  stat = "summary",size=1,
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median)+ coord_trans(x="log2")+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),
                       text = element_text(family = "Arial", size = 14, color = "black"),axis.title.x = element_blank(),axis.text.y = element_text(color = "black",hjust = 1),
                       axis.text.x = element_blank(),axis.title.y=element_text(), legend.position = "right") +
  scale_color_manual("Site", values = c("#D55E00","#0072B2","#009E73","#E69F00","darkred"))+ggtitle("c)")+ylim(0,1)+
  ylab(expression(paste("CO"[2]," (mg C L"^"-1",")")))+xlab("")+ scale_x_continuous(breaks = c(0,100,250,500,1000,2000,4000,6000))
a

b<-ggplot(data,aes(x = Pool_size_m2, y = CH4_mgCL,color= SiteID)) +geom_dotplot(binaxis='y', stackdir='center',stackratio=.8, dotsize=.8,fill = "grey70")+
  geom_pointrange(data, mapping = aes(x = Pool_size_m2, y = CH4_mgCL,color = SiteID),
                  stat = "summary",size=1,
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median)+ coord_trans(x="log2")+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),
                       text = element_text(family = "Arial", size = 14, color = "black"),axis.title.x = element_blank(),axis.text.y = element_text(color = "black",hjust = 1),
                       axis.text.x = element_blank(),axis.title.y=element_text(), legend.position = "none") +
  scale_color_manual("Site", values = c("#D55E00","#0072B2","#009E73","#E69F00","darkred"))+ggtitle("d)")+ylim(0,0.05)+
  ylab(expression(paste("CH"[4]," (mg C L"^"-1",")")))+xlab("")+ scale_x_continuous(breaks = c(0,100,250,500,1000,2000,4000,6000))
b


c<-ggplot(data,aes(x = Pool_size_m2, y = FluxCO2_mgCm2d,color= SiteID)) +geom_dotplot(binaxis='y', stackdir='center',stackratio=.8, dotsize=.8,fill = "grey70")+
  geom_pointrange(data, mapping = aes(x = Pool_size_m2, y = FluxCO2_mgCm2d,color = SiteID),
                  stat = "summary",size=1,
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median)+ coord_trans(x="log2")+ 
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),
                       text = element_text(family = "Arial", size = 14, color = "black"),axis.title.x = element_blank(),axis.text.y = element_text(color = "black",hjust = 1),
                       axis.text.x = element_blank(),axis.title.y=element_text(), legend.position = "none") +
  scale_color_manual("Site", values = c("#D55E00","#0072B2","#009E73","#E69F00","darkred"))+ggtitle("a)")+
  ylab(expression(paste(""~CO[2]*" diffusion (mg C m"^"-2"," d"^"-1",")")))+xlab("")+ scale_x_continuous(breaks = c(0,100,250,500,1000,2000,4000,6000))
c

d<-ggplot(data,aes(x = Pool_size_m2, y = FluxCH4_mgCm2d,color= SiteID)) +geom_dotplot(binaxis='y', stackdir='center',stackratio=.8, dotsize=.8,fill = "grey70")+
  geom_pointrange(data, mapping = aes(x = Pool_size_m2, y = FluxCH4_mgCm2d,color = SiteID),
                  stat = "summary",size=1,
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median)+ coord_trans(x="log2")+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),
                       text = element_text(family = "Arial", size = 14, color = "black"),axis.title.x = element_blank(),axis.text.y = element_text(color = "black",hjust = 1),
                       axis.text.x = element_blank(),axis.title.y=element_text(), legend.position = "none") +
  scale_color_manual("Site", values = c("#D55E00","#0072B2","#009E73","#E69F00","darkred"))+ylim(0,160)+ggtitle("b)")+
  ylab(expression(paste(""~CH[4]*" diffusion (mg C m"^"-2"," d"^"-1",")")))+xlab("")+ scale_x_continuous(breaks = c(0,100,250,500,1000,2000,4000,6000))
d

data$d13CCO2_permil
e<-ggplot(data,aes(x = Pool_size_m2, y =d13CCO2_permil,color= SiteID)) +geom_dotplot(binaxis='y', stackdir='center',stackratio=.6, dotsize=.6,fill = "grey70")+
  geom_pointrange(data, mapping = aes(x = Pool_size_m2, y = d13CCO2_permil,color = SiteID),
                  stat = "summary",size=1,
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median)+ coord_trans(x="log2")+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),
                       text = element_text(family = "Arial", size = 14, color = "black"),axis.title.x = element_text(),axis.text.y = element_text(color = "black",hjust = 1),
                       axis.text.x = element_text(angle = 25,color = "black",hjust = 1),axis.title.y=element_text(),legend.position = "none") +ggtitle("e)")+
  scale_color_manual("Site", values = c("#D55E00","#0072B2","#009E73","#E69F00","darkred"))+
  ylab(expression(paste(delta^{13}, "C-CO"[2]," (\u2030)")))+xlab(expression(paste("Pool Size (m"^"2",")")))+ scale_x_continuous(breaks = c(0,100,250,500,1000,2000,4000,6000))
e

my.formula <- y ~ x
f<-ggplot(data,aes(x = Pool_size_m2, y =d13CCH4_permil,color= SiteID)) +geom_dotplot(binaxis='y', stackdir='center',stackratio=.6, dotsize=.6,fill = "grey70")+
  geom_pointrange(data, mapping = aes(x = Pool_size_m2, y = d13CCH4_permil,color = SiteID),
                  stat = "summary",size=1,
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median)+ coord_trans(x="log2")+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),
                       text = element_text(family = "Arial", size = 14, color = "black"),axis.title.x = element_text(),axis.text.y = element_text(color = "black",hjust = 1),
                       axis.text.x = element_text(angle = 25,color = "black",hjust = 1),axis.title.y=element_text(),legend.position = "none") +ggtitle("f)")+
  scale_color_manual("Site", values = c("#D55E00","#0072B2","#009E73","#E69F00","darkred"))+
  ylab(expression(paste(delta^{13}, "C-CH"[4]," (\u2030)")))+xlab(expression(paste("Pool Size (m"^"2",")")))+ scale_x_continuous(breaks = c(0,100,250,500,1000,2000,4000,6000))
f


#offloading figures
#svglite(("Figure3.svg"), height = 32, width = 28)
tiff(("Figure3.tiff"), height = 32, width = 28, units = 'cm', compression = "lzw", res = 600)
ggarrange(c,d,a,b,e,f, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
dev.off()