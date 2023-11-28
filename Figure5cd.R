library(ggplot2)
library(munsell)
library(ggpubr)
library(ggthemes)
library(svglite)

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 5")
data <- read.csv('Figure5cd.csv',header=TRUE, sep=",", skip=0, dec=".")

data$Fulldate=as.POSIXct(data$Fulldate, format = "%Y-%m-%d %H:%M")
data$Year=format(as.Date(data$Fulldate, format="%Y-%m-%d %H:%M"),"%Y")
data$DoY=strftime(data$Fulldate, format = "%j")
data$HHMM=strftime(data$Fulldate, format = "%H:%M")
data$Hour=sapply(strsplit(data$HHMM,":"),
                 function(x) {
                   x <- as.numeric(x)
                   x[1]+x[2]/60
                 }
)


data$Hourdec=data$Hour/24
data$DoY3<-as.numeric(data$DoY)
data$Hour2<-as.numeric(data$Hourdec)
data$DoY2=data$DoY3+data$Hour2

a<-ggplot(data, aes(x=Hour, y=pool_CO2_var_mgCL, color = as.integer(DoY), group = DoY)) + 
  geom_rect(data=data,mapping=aes(xmin=19,xmax=23,ymin=-0.2, ymax=0.3),
            col="white",fill="gray65",alpha=0.008, inherit.aes = FALSE) +
  geom_rect(data=data,mapping=aes(xmin=00,xmax=06,ymin=-0.2, ymax=0.3),
            col="white",fill="gray65",alpha=0.008, inherit.aes = FALSE)+
  geom_line()+
  ggtitle("c)")+
  theme_tufte()+theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14),
                      legend.position = "none",axis.text.x = element_text(color = "black",size = 10, angle = 45,hjust = 1))+
  geom_hline(yintercept=0, colour="black")+  scale_colour_gradient2(name = "", 
                                                                    low = "#D55E00",  midpoint = 203.5,mid="#F0E442", high = "#009E73",
                                                                    breaks=c(176,203.5,231),labels=c("June", "July", "August"))+
  scale_x_continuous(name="Hour of the day (HH:MM)",breaks=c(1,4,7,10,13,16,19,22,25),
                     labels=c("01:00","04:00","07:00","10:00","13:00","16:00","19:00","22:00","01:00"))+
  scale_y_continuous(expression(paste(''*~CO[2]*" variation from the daily mean (mg C L"^"-1",")")),limits=c(-.2,0.3))

a

b<-ggplot(data, aes(x=Hour, y=pool_CH4_var_ugCL, color = as.integer(DoY), group = DoY))+
  geom_rect(data=data,mapping=aes(xmin=19,xmax=23,ymin=-40, ymax=40),
            col="white",fill="gray65",alpha=0.008, inherit.aes = FALSE) +
  geom_rect(data=data,mapping=aes(xmin=00,xmax=06,ymin=-40, ymax=40),
            col="white",fill="gray65",alpha=0.008, inherit.aes = FALSE) + 
  ggtitle("d)")+
  theme_tufte()+theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "Arial", size = 14),
                      legend.position = "none",axis.text.x = element_text(color = "black",size = 10, angle = 45,hjust = 1))+
  geom_line()+
  geom_hline(yintercept=0, colour="black")+  scale_colour_gradient2(name = "", 
                                                                    low = "#D55E00",  midpoint = 203.5,mid="#F0E442", high = "#009E73",
                                                                    breaks=c(176,203.5,231),labels=c("June", "July", "August"))+
  scale_x_continuous(name="Hour of the day (HH:MM)",breaks=c(1,4,7,10,13,16,19,22,25),
                     labels=c("01:00","04:00","07:00","10:00","13:00","16:00","19:00","22:00","01:00"))+
  scale_y_continuous(expression(paste(''*~CH[4]*" variation from the daily mean (", mu, "g C L"^"-1",")")),limits=c(-40,40))
b

#tiff(("Figure5cd.tiff"), height = 20, width =28, units = 'cm', compression = "lzw", res = 600)
ggarrange(a,b, ncol = 2, nrow = 1,common.legend = TRUE, legend="bottom",align = c("hv"))
#dev.off()
#
