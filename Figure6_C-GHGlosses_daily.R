library(pastecs)
library(ggplot2)
library(ggpubr)

setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 6")
data<- read.csv('Figure6_C-GHGlosses_daily.csv',header=TRUE, sep=",", dec=".")

data$Fulldate=as.POSIXct(data$Date, format = "%m/%d/%Y")

a<- ggplot(data, aes(fill=Flux_type , y=Flux_mgCm2d, x=Fulldate)) +
  geom_bar(position="stack", stat="identity")+
  theme(panel.background = element_rect(fill = "white", colour = "black"),legend.title=element_blank(),legend.position="right",
        text = element_text(size=16))+
  labs(y=expression(paste("C-GHG evasion (mg C m"^" 2","" ," d"^"-1",")")), x = " ")+ 
  scale_fill_manual(values=c( "#56B4E9","#E69F00","#D55E00"),breaks=c("CO2_evasion", "CH4_evasion","CH4_ebullition"),
                    labels=c(expression(paste(" CO"[2]," diffusion")),expression(paste("CH"[4]," diffusion")),expression(paste(" CH"[4]," ebullition"))))
a

tiff(("Figure6_C-GHGlosses_daily.tiff"), height = 20, width = 30, units = 'cm', compression = "lzw", res = 600)
a
dev.off()


