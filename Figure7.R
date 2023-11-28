# Load packages
library(dplyr)
library(lubridate)
library(pastecs)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(ggpmisc)
library(pastecs)
library(chron)
library(hms)
library(scales)
library(ggthemes)

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 3")
data <- read.csv('Figure3_Figure7.csv',header=TRUE, sep=",", skip=0, dec=".")


my.formula <- y ~ x
j <- ggplot(data = data[(data$Water.body=="M" & data$Year==2019)|data$Water.body=="PW",] , aes(y = d13CCO2_permil, x =d13CCH4_permil,color=SiteID)) +
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14, color = "black"),
                       legend.position = "right")+
  labs(x=expression(paste(delta^{13}, "C-"*~CH[4]*" (\u2030)")), y =expression(paste(delta^{13}, "C-"*~CO[2]*" (\u2030)")))+
  geom_abline(intercept = (1.09*1000-1000), slope = 1.09, linetype="dashed", size=.4)+
  geom_abline(intercept = (1.08*1000-1000), slope = 1.08, linetype="dashed", size=.4)+
  geom_abline(intercept = (1.07*1000-1000), slope = 1.07, linetype="dashed", size=.4)+
  geom_abline(intercept = (1.06*1000-1000), slope = 1.06, linetype="dashed", size=0.4)+
  geom_abline(intercept = (1.05*1000-1000), slope = 1.05, linetype="dashed", size=.4)+
  geom_abline(intercept = (1.04*1000-1000), slope = 1.04, linetype="dashed", size=.4)+
  geom_abline(intercept = (1.03*1000-1000), slope = 1.03, linetype="dashed", size=.4)+
  geom_point(aes(size = CH4_mgCL))+
  annotate("text", x=-85.9, y=-21, label= "α=1.06",angle='45')+
  annotate("text", x=-77, y=-21, label= "α=1.05",angle='45')+
  annotate("text", x=-68.5, y=-21, label= "α=1.04",angle='45')+
  annotate("text", x=-59.5, y=-21, label= "α=1.03",angle='45')+
  annotate("text", x=-90.5, y=-17, label= "α=1.07",angle='45')+
  annotate("text", x=-90.7, y=-8, label= "α=1.08",angle='45')+
  annotate("text", x=-78, y=-7.1, label= "Oxidation", size=6,angle='-2',color = 2)+
  annotate("text", x=-85, y=-5, label= "Hydrogenotrophic", size=6,color = 3)+
  annotate("text", x=-62, y=-17.5, label= "Acetoclastic", size=6,color = 4)+
  scale_colour_manual("Sampling Site",values=c("M11"="#E69F00", "M12"="#CC79A7",
                                               "M13" = "#009E73", "M14"="#D55E00", "M15"="#0072B2",
                                               "PW" = "#661100"))+
  labs(size=expression(paste(""*~CH[4]*" (mg C "~L^-1*")"))) +
    geom_segment(x = -82, y = -7.4885, xend = -60, yend = -8.211, color = 2, size=1,
               arrow = arrow())+
  geom_segment(x = -99.88, y = -10.83, xend =-80.51, yend =4.74, color = 3, size=1)+
  geom_segment(x = -80.51, y = 4.74, xend =-65.80, yend =15.28, color = 3, size=1)+
  geom_segment(x = -65.80, y = 15.28, xend =-54.80, yend =18.58, color = 3, size=1)+
  geom_segment(x = -54.80, y =18.58, xend =-51.45, yend =-16.07, color = 3, size=1)+
  geom_segment(x = -51.45, y =-16.07, xend =-53.72, yend =0.49, color = 3, size=1)+
  geom_segment(x = -53.72, y = 0.49, xend =-54.80, yend =-1.24, color = 3, size=1)+
  geom_segment(x = -54.80, y = -1.24, xend =-58.98, yend =-4.07, color = 3, size=1)+
  geom_segment(x = -58.98, y = -4.07, xend =-62.09, yend =-7.84, color = 3, size=1)+
    geom_segment(x = -62.09, y = -7.84, xend =-65.20, yend =-16.65, color = 3, size=1)+
  geom_segment(x = -65.20, y = -16.65, xend =-74.05, yend =-24.67, color = 3, size=1)+
geom_segment(x = -74.05, y =-24.67, xend =-80.39, yend =-28.29, color = 3, size=1)+
geom_segment(x =-80.39, y =-28.29, xend =-85.05, yend =-29.39, color = 3, size=1)+
geom_segment(x =-85.05, y =-29.39, xend =-91.75, yend =-28.76, color = 3, size=1)+
geom_segment(x =-85.05, y =-29.39, xend =-91.75, yend =-28.76, color = 3, size=1)+
geom_segment(x =-91.75, y =-28.76, xend =-100, yend =-27.66, color = 3, size=1)+
  geom_segment(x =-66.41, y =-28.56, xend =-65.20, yend =-16.65, color = 4, size=1)+
geom_segment(x =-65.20, y =-16.65, xend= -62.09, yend = -7.84, color = 4, size=1,linetype="dashed")+
geom_segment(x = -62.09, y = -7.84, xend =-59.17, yend =-4.19, color = 4, size=1,linetype="dashed")+
  geom_segment(x =-59.17, y = -4.19, xend =-58.98, yend =-4.07, color = 4, size=1,linetype="dashed")+
geom_segment(x = -58.98, y = -4.07, xend = -54.80, yend = -1.24, color = 4, size=1,linetype="dashed")+
  geom_segment(x =-51.99, y=-2.76, xend =-49.39, yend =-6.56, color = 4, size=1,linetype="dashed")+
  geom_segment(x =-49.39, y =-6.56, xend =-48.19, yend =-11.02, color = 1, size=1)+
  geom_segment(x =-48.19, y =-11.02, xend =-48.38, yend =-17.18, color = 4, size=1)+
  geom_segment(x =-48.38, y =-17.18, xend =-51.86, yend =-20.98, color = 4, size=1)+
  geom_segment(x =-51.86, y=-20.98, xend =-63.72, yend =-28.30, color = 4, size=1)+
  geom_segment(x =-63.72, y =-28.30, xend =-66.41, yend =-28.56, color = 4, size=1)
j

###Oxidation line from https://doi.org/10.5194/bg-5-1457-2008
###Hydrogenotrophic and Acetoclastic boxes from 10.1371/journal.pone.0078204

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 5")
tiff(("Figure7.tiff"), height = 20, width = 30, units = 'cm', compression = "lzw", res = 600)
j
dev.off()