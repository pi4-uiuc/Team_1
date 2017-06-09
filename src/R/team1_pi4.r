library(readr)
ref20 <- read.csv("~/Team_1/data/Ref_Factor_20.csv")
ref48 <- read.csv("~/Team_1/data/Ref_Factor_48.csv")
ref75 <- read.csv("~/Team_1/data/Ref_Factor_75.csv")
ref95 <- read.csv("~/Team_1/data/Ref_Factor_95.csv")
ref99 <- read.csv("~/Team_1/data/Ref_Factor_99.csv")
#View(ref20)
#colnames(ref20)
# Delelting all those rows/observations for which Reflective percentage is above 100
#ref20$Reflect...>100
Ref20_100<-which(ref20$Reflect... >=100)
Ref48_100<-which(ref48$Reflect... >=100)
Ref75_100<-which(ref75$Reflect... >=100)
Ref95_100<-which(ref95$Reflect... >=100)
Ref99_100<-which(ref99$Reflect... >=100)

## Replace all those Rediation for which %Reflection is above or equal 100 with NA
ref20$Rad...Ref..[Ref20_100]<- NA
ref48$Rad...Ref..[Ref48_100]<- NA
ref75$Rad...Ref..[Ref75_100]<- NA
ref95$Rad...Ref..[Ref95_100]<- NA
ref99$Rad...Ref..[Ref99_100]<- NA

## Next we want to keep only wavelength and Rad(ref) in our data set for 20%, 48%, 75%, 95%, and 99%

new_ref20<-ref20[c(1,3)]
new_ref48<-ref48[c(1,3)]   #  keep first column (wavelenth) of all bcs dim is not same for all files after cleaning data
new_ref75<-ref75[c(1,3)]
new_ref95<-ref95[c(1,3)]
new_ref99<-ref99[c(1,3)]
## check 
names(new_ref20)

## Now to clean the data a little more, We will replace all the negative values in the Radiation (ref) column with NA

new_ref20[new_ref20 <= 0]<-NA
new_ref48[new_ref48 <= 0]<-NA
new_ref75[new_ref75 <= 0]<-NA
new_ref95[new_ref95 <= 0]<-NA
new_ref99[new_ref99 <= 0]<-NA
## Check dimension of each now
dim(new_ref20)
dim(new_ref48)
dim(new_ref75)
dim(new_ref95)
dim(new_ref99)

## Perfect, all files have the same dimension now

## T0 make typing easy, lets rename the variables
x20<- new_ref20$Wvl
y20<-new_ref20$Rad...Ref..

x48<- new_ref48$Wvl
y48<-new_ref48$Rad...Ref..

x75<- new_ref75$Wvl
y75<-new_ref75$Rad...Ref..

x95<- new_ref95$Wvl
y95<-new_ref95$Rad...Ref..

x99<- new_ref95$Wvl
y99<-new_ref95$Rad...Ref..
## Firs plot all the files in the same window
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)

pdf(file = '~/Team_1/results/dataplots.pdf')
plot(x20, y20,style ='p',  pch = 19, cex = .6, col='red',  xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor vs Wavelength")
#par(new= TRUE)
lines(x48, y48,style ='p', pch = 20, cex = .7, col = 'green')
#par(new= TRUE)
lines(x75, y75, style ='p',pch = 18, cex = .8, col = 'cyan')
#par(new= TRUE)
lines(x95, y95, style ='p',pch = 17, cex = .6, col='blue')
#par(new= TRUE)
lines(x99, y99, style ='p',pch = 15, cex = .6, col = 'black')
legend("topright", legend=c("Ra20%", "Rad48%", "Rad75", "Rad95", "Rad99"),
       col=c("red", 'green', 'cyan', 'blue',  "black"), pch = c(19, 20, 18, 17, 15), cex=0.8, box.lty=2, box.lwd=2, box.col="green")
dev.off()
#dev.off()
## Fitting the data

### First fit on the 20% data

f20 <- splinefun(x20, y20, method = "fmm", ties = mean)

#par(mfrow=c(4,1))
pdf(file = '~/Team_1/results/fitt20_48.pdf')
plot(x48, f20(x48), type = 'l',col = 'red',  xlab ='wavelength', ylab='Radiation(W/m^2) ', main = "Spline fitted vs Ref_Factor48%")
#par(new= TRUE)
lines(x48, y48, type = 'p', pch=19,cex = 0.5,  col='blue')
legend("topright", legend=c("fitted20%", "ref48%"),
       col=c("red", "blue"), lty=1:2,   cex=0.8, box.lty=2, box.lwd=2, box.col="green")
dev.off()
#par(new=TRUE)
pdf(file = '~/Team_1/results/fitt20_75.pdf')
plot(x75, f20(x75), type = 'l',col = 'red',  xlab ='wavelength', ylab='Radiation(W/m^2) ', main = "Spline fitted vs Ref_Factor75%")
par(new= TRUE)
lines(x75, y75, type = 'p', pch=18,cex = 0.5,  col='blue')
legend("topright", legend=c("fitted20%", "ref48%"),
       col=c("red", "blue"), lty=1:2,   cex=0.8, box.lty=2, box.lwd=2, box.col="green")
dev.off()

pdf(file = '~/Team_1/results/fitt20_95.pdf')
plot(x95, f20(x95), type = 'l',col = 'red',  xlab ='wavelength', ylab='Radiation(W/m^2) ', main = "Spline fitted vs Ref_Factor95%")
par(new= TRUE)
lines(x95, y95, type = 'p', pch=17,cex = 0.5,  col='blue')
legend("topright", legend=c("fitted20%", "ref95%"),
       col=c("red", "blue"),  lty=1:2, cex=0.8, box.lty=2, box.lwd=2, box.col="green")

dev.off()

pdf(file = '~/Team_1/results/fitt20_99.pdf')
plot(x99, f20(x99), type = 'l',col = 'red',  xlab ='wavelength', ylab='Radiation(W/m^2) ', main = "Spline fitted vs Ref_Factor99%")
par(new= TRUE)
lines(x95, y95, type = 'p', pch=15,cex = 0.5,  col='blue')
legend("topright", legend=c("fitted20%", "ref99%"),
       col=c("red", "blue"),  lty=1:2, cex=0.8, box.lty=2, box.lwd=2, box.col="green")
dev.off()

### Now, fit the data one the base of three files 20%, 75%, and 99%
Rad_mean<-rowMeans(cbind(new_ref20$Rad...Ref.., new_ref75$Rad...Ref.., new_ref99$Rad...Ref..), na.rm=TRUE)
#Wvl_mean<-rowMeans(cbind(new_ref20$, new_ref75$Rad...Ref.., new_ref99$Rad...Ref..), na.rm=TRUE)

fmean <- splinefun(x20, Rad_mean, method = "fmm", ties = mean)

#par(mfrow=c(1,2))
pdf(file = '~/Team_1/results/fittmean_48.pdf')
plot(x48, fmean(x48), type = 'l',col = 'red',  xlab ='wavelength', ylab='Radiation(W/m^2) ', main = "Spline fitted on 20%, 75%, and 99%")
par(new= TRUE)
lines(x48, y48, type = 'p', pch = 20, cex = 0.5, col='blue')
legend("topright", legend=c("fittedmean%", "ref48%"),
       col=c("red", "blue"),  lty=1:2, cex=0.8, box.lty=2, box.lwd=2, box.col="green")
dev.off()
#par(new=TRUE)
pdf(file = '~/Team_1/results/fittmean_99.pdf')
plot(x95, fmean(x95), type = 'l', col = 'red', xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Spline fitted on 20%, 75%, and 99%")
par(new= TRUE)
lines(x95, y95, type = 'p', pch = 17, cex = 0.5, col='black')
legend("topright", legend=c("fittedmean%", "ref95%"),
       col=c("red", "black"),  lty=1:2, cex=0.8, box.lty=2, box.lwd=2, box.col="green")
dev.off()





