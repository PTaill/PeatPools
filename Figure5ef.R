# Load packages
library(dplyr)
library(lubridate)
library(pastecs)
library(ggplot2)
library(ggbreak) 
library(patchwork)

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 5")
data<- read.csv('Figure5ef.csv',header=TRUE, sep=",", dec=".",check.names = F)
head(data)

data$TimeDate=paste(data$Sampling_date,data$Sampling_time)
data$TimeDateEast=as.POSIXct(data$TimeDate, format = "%d/%m/%Y %H:%M")
data$Pool.ID<-data$SiteID

minA1=as.POSIXct("06-08-19 00:00", format = "%d-%m-%y %H:%M")
maxA1=as.POSIXct("07-08-19 00:01", format = "%d-%m-%y %H:%M")
minAA1=as.POSIXct("06-08-19 11:00", format = "%d-%m-%y %H:%M")

Night=as.POSIXct("06-08-19 20:00", format = "%d-%m-%y %H:%M")
Day=as.POSIXct("06-08-19 04:48", format = "%d-%m-%y %H:%M")

## ensure that data is sorted by DateTime 
data2 <- data %>%
  arrange(TimeDateEast)
data2$Pool.ID
data2<-subset(data,data$SiteID=="M11"|data$SiteID=="M12"|data$SiteID=="M13"|data$SiteID=="M14"|data$SiteID=="M15")
data2$Pool.ID <- factor(data2$Pool.ID, levels = c('M15','M13','M12',"M14","M11"))

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/4. 24h time series")
#tiff(("20221221_Pools_TimeSeries.tiff"), height = 12, width = 17, units = 'cm', compression = "lzw", res = 600)

par(mfrow=c(2,2), mar=c(0,1.5,0,4), oma=c(4,4,2,1), xpd=F, cex=.5)
data$d13CCO2_permil
c<-ggplot(data2, aes(x=TimeDateEast, y=d13CCO2_permil,group = Pool.ID)) +
  geom_line(aes(color = Pool.ID))+geom_point(aes(color = Pool.ID))+
  scale_x_datetime(limits=as.POSIXct(c("2019-08-06 00:00:00 +08","2019-08-07 00:00:01 +08")),
                   breaks = scales::date_breaks("3 hour"),date_labels = "%H:%M")+
  ggtitle("e)")+
  theme_tufte()+theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14),
                      legend.position = "none",axis.text.x = element_text(color = "black",size = 10, angle = 45,hjust = 1))+
  ylim(-23,-5)+
  annotate("rect", xmin=as.POSIXct(c("2019-08-06 00:00:00 +08")), xmax=as.POSIXct(c("2019-08-06 04:48:00 +08")), ymin=-23, ymax=-5, alpha=0.2, fill="Grey20")+
  annotate("rect", xmin=as.POSIXct(c("2019-08-06 20:00:00 +08")), xmax=as.POSIXct(c("2019-08-07 00:00:01 +08")), ymin=-23, ymax=-5, alpha=0.2, fill="Grey20")+
  ylab(expression(paste(delta^{13}, "C-CO"[2]," (\u2030)")))+xlab("Time (HH:MM)")+
  scale_color_manual("Site", values = c("#D55E00","#0072B2","#009E73","#E69F00","darkred"))
c

d<-ggplot(data2, aes(x=TimeDateEast, y=d13CCH4_permil,group = Pool.ID)) +
  geom_line(aes(color = Pool.ID))+geom_point(aes(color = Pool.ID))+
  scale_x_datetime(limits=as.POSIXct(c("2019-08-06 00:00:00 +08","2019-08-07 00:00:01 +08")),breaks = scales::date_breaks("3 hour"),date_labels = "%H:%M")+
  ggtitle("f)")+
  theme_tufte()+theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14),
                      legend.position = "none",axis.text.x = element_text(color = "black",size = 10, angle = 45,hjust = 1))+
  ylim(-75,-50)+
  annotate("rect", xmin=as.POSIXct(c("2019-08-06 00:00:00 +08")), xmax=as.POSIXct(c("2019-08-06 04:48:00 +08")), ymin=-75, ymax=-50, alpha=0.2, fill="Grey20")+
  annotate("rect", xmin=as.POSIXct(c("2019-08-06 20:00:00 +08")), xmax=as.POSIXct(c("2019-08-07 00:00:01 +08")), ymin=-75, ymax=-50, alpha=0.2, fill="Grey20")+
  ylab(expression(paste(delta^{13}, "C-CH"[4]," (\u2030)")))+xlab("Time (HH:MM)")+
  scale_color_manual("Site", values = c("#D55E00","#0072B2","#009E73","#E69F00","darkred"))
d

#offloading figures
svglite(("20231120_Pools_TimeSeries.svg"), height = 20, width = 28)
#tiff(("20231120_Pools_TimeSeries.tiff"), height = 20, width = 28, units = 'cm', compression = "lzw", res = 600)
ggarrange(c,d, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off()
