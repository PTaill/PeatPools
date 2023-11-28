setwd("C:/Users/eripet/Documents/UQAM/CO2 and CH4 in and from pools/Draft/Revision 1/Figures, Scripts, Datasets/Figure 4")
data<- read.csv('Figure4_CO2CH4timeseries.csv',header=TRUE, sep=",", dec=".")

min2020=as.POSIXct("00:00:00 24/06/2020", format = "%H:%M:%S %d/%m/%Y")
max2020=as.POSIXct("23:00:00 24/08/2020", format = "%H:%M:%S %d/%m/%Y")
min=as.POSIXct("00:00:00 24/06/2020", format = "%H:%M:%S %d/%m/%Y")
min2=as.POSIXct("00:00:00 23/06/2020", format = "%H:%M:%S %d/%m/%Y")
min3=as.POSIXct("00:00:00 22/06/2020", format = "%H:%M:%S %d/%m/%Y")
max=as.POSIXct("23:00:00 24/08/2020", format = "%H:%M:%S %d/%m/%Y")

data$TIMEEAST=as.POSIXct(data$Date_Time, format = "%m/%d/%Y %H:%M")

data$Rain_mm_Tot<-ifelse(data$Rain_mm_Tot==0,NA,data$Rain_mm_Tot)

tiff(("Figure4.tiff"), height = 12, width = 18, units = 'cm', compression = "lzw", res = 600)

par(mfrow=c(5,1), mar=c(0,4,0,4), oma=c(4,2,2,2), xpd=F, cex=.5)
plot(data$Rain_mm_Tot~data$TIMEEAST,pch=16,xlab="",type="h",ylab="",xaxt="n",las=1,xlim=c(min2020,max2020),
     col="#0072B2",ylim=c(18,0),yaxt="n")
axis(4,las=1,col.axis="#0072B2")
mtext(expression(paste("Rain (mm)")), side=4,line=3.3,cex=0.7,col="#0072B2")
par(new=T)
plot(data$WTD_m~data$TIMEEAST,pch=16,xlab="",type="l",ylab="",xaxt="n",las=1,
     xlim=c(min2020,max2020),ylim=c(-0.4,0.1),yaxt="n")
text(min2,0.03, labels="a)",cex=2)
axis(2,las=1)
mtext(expression(paste("WTD")), side=2, line=4.3,cex=0.7)
mtext(expression(paste("(m)")), side=2, line=2.7,cex=0.7)

plot(data$Rain_mm_Tot~data$TIMEEAST,pch=16,xlab="",type="h",ylab="",xaxt="n",las=1,xlim=c(min2020,max2020),
     col="#0072B2",ylim=c(20,0),yaxt="n")
axis(4,las=1,col.axis="#0072B2")
mtext(expression(paste("Rain (mm)")), side=4,line=3.3,cex=0.7,col="#0072B2")
par(new=T)
plot(data$CO2_mgCL~data$TIMEEAST,pch=16,xlab="",ylab="",cex=0.2,xaxt="n",las=1,xlim=c(min,max),
     type="l")
text(min2,0.75, labels="b)",cex=2)
mtext(expression(paste("CO"[2]," (mg C L"^"-1",")")), side=2, line=3.3,cex=0.7)

plot(data$Rain_mm_Tot~data$TIMEEAST,pch=16,xlab="",type="h",ylab="",xaxt="n",las=1,xlim=c(min2020,max2020),
     col="#0072B2",ylim=c(20,0),yaxt="n")
axis(4,las=1,col.axis="#0072B2")
mtext(expression(paste("Rain (mm)")), side=4,line=3.3,cex=0.7,col="#0072B2")
par(new=T)
plot(data$CH4_ugCL~data$TIMEEAST,pch=16,xlab="",ylab="",cex=0.2,xaxt="n",las=1,xlim=c(min,max),
     type="l",ylim=c(0,200))
text(min2,180, labels="c)",cex=2)
mtext(expression(paste("CH"[4]," (", mu, "g C L"^"-1",")")), side=2, line=3.3,cex=0.7)

plot(data$Rain_mm_Tot~data$TIMEEAST,pch=16,xlab="",type="h",ylab="",xaxt="n",las=1,xlim=c(min2020,max2020),
     col="#0072B2",ylim=c(18,0),yaxt="n")
axis(4,las=1,col.axis="#0072B2")
mtext(expression(paste("Rain (mm)")), side=4,line=3.3,cex=0.7,col="#0072B2")
par(new=T)
plot(data$Temp40cm_degC~data$TIMEEAST,pch=16,cex=0.4,xlab="",type="l",ylab="",xaxt="n",las=1,
     xlim=c(min2020,max2020),ylim=c(8,16))
text(min2,15, labels="d)",cex=2)
mtext(expression(paste("Soil Temp\n-40cm (degC)")), side=2,line=3.3,cex=0.7)

plot(data$Rain_mm_Tot~data$TIMEEAST,pch=16,xlab="",type="h",ylab="",xaxt="n",las=1,xlim=c(min2020,max2020),
     col="#0072B2",ylim=c(18,0),yaxt="n")
axis(4,las=1,col.axis="#0072B2")
mtext(expression(paste("Rain (mm)")), side=4,line=3.3,cex=0.7,col="#0072B2")
par(new=T)
plot(data$PoolT_degC~data$TIMEEAST,pch=16,xlab="",ylab="",xaxt="n",cex=0.2,las=1,
     xlim=c(min,max),type="l")
text(min2,24, labels="e)",cex=2)
saxis=seq(from=min, to=max, length=10)
axis.POSIXct(side=1, x=saxis, at=saxis, format = "%d/%m", labels=T)
mtext("Date (DD/MM/2020)", side=1, line=3,cex=0.7,las=1)
mtext(expression(paste("Pool Temp\n-40cm (degC)")), side=2, line=3.3,cex=0.7)

dev.off()
