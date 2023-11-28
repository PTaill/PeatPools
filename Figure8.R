# Load packages
library(dplyr)
library(cowplot)
library(pastecs)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(lubridate)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(chron)
library(hms)
library(scales)
library(ggthemes)
library(tidyverse)

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 3")
data <- read.csv('Figure3_Figure5_Figure7.csv',header=TRUE, sep=",", skip=0, dec=".")

data$CO2_mgCL<-data$water_CO2_umolL1*12.0107/1000
data$X1CO2_mgCL<-1/data$CO2_mgCL
data$X1CO2_ppm<-1/data$pCO2_ppm
data$CO2Mix<-data$CO2_mgCL*data$d13CCO2_permil
data$CO2Mix_edit<-ifelse(data$CO2_mgCL>=20&data$CO2Mix>=(-100),NA,data$CO2Mix)
data$Water.body<-ifelse(data$Water.body=="W","PW",data$Water.body)
head(data)

my.formula <- y ~ x
a <- ggplot(data = data[(data$Water.body=="M" & data$Year==2019)|data$Water.body=="PW",] , aes(x = CO2_mgCL, y =CO2Mix,color=SiteID)) +geom_point(size=2)+
  geom_smooth(method = "lm", se=F, formula = my.formula, linetype = "dashed") +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label..,..rr.label.., ..p.value.label.., sep = "*`,`~")), label.x.npc = "right",label.y.npc = "top",
               parse = TRUE) +ylim(-12,-2.8)+xlim(0.3,3)+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),
                       legend.position = "bottom")+
  labs(y=expression(paste(delta^{13}, "C-"*~CO[2]*" (\u2030) *"~CO[2]*" (mg C L"^"-1",")")), x =expression(paste(""*~CO[2]*" (mg C L"^"-1",")")))+ggtitle("a) Surface Water (Open-water Pools)") +
  scale_colour_manual("Site",values=c("M11"="#E69F00", "M12"="#CC79A7",
                               "M13" = "#009E73", "M14"="#D55E00", "M15"="#0072B2",
                               "PW" = "#661100"))
a


my.formula <- y ~ x
b <- ggplot(data = data[data$Water.body=="PW",] , aes(x = CO2_mgCL, y =CO2Mix_edit,color=Water.body)) +geom_point(size=2)+
  geom_smooth(method = "lm", se=F, formula = my.formula, linetype = "dashed") +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label..,..rr.label.., ..p.value.label.., sep = "*`,`~")), label.x.npc = "right",label.y.npc = "top",
               parse = TRUE) +xlim(0,70)+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),
                       legend.position = "none",legend.title=element_blank())+
  labs(y=expression(paste(delta^{13}, "C-"*~CO[2]*" (\u2030) *"~CO[2]*" (mg C L"^"-1",")")), x =expression(paste(""*~CO[2]*" (mg C L"^"-1",")")))+ggtitle("b) Peat Porewater and Wells") +
  scale_colour_manual(values=c("PW" = "#661100"))
b

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 8")
#tiff(("Figure8.tiff"), height = 16, width = 28, units = 'cm', compression = "lzw", res = 600)
ggarrange(a,b, ncol=2, nrow=1, common.legend = TRUE, legend="right")
#dev.off()
