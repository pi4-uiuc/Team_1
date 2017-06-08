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

dim(ref20)
dim(ref48)
dim(ref75)
dim(ref95)
dim(ref99)


# Check the dimension of the new data after deleting values for which %Reflection > 100

# hmmm, the new data does not have the  same dimension for 20%, 48%,  75%, 95%, and 99%   Ask David about it? 
# Next merge all this data into one data table, and we only need one column for wave length since the
# wave lenth column has same values for the all percentage files (i.e, 20%,48%,  75%, 95%, and 99% )
# Also, I only want to extract Rad(ref) from all data files and want to plot them against wavelength. 
new_ref20<-ref20[c(1,3,7)]
new_ref48<-ref48[c(1,3,7)]   #  keep first column (wavelenth) of all bcs dim is not same for all files after cleaning data
new_ref75<-ref75[c(1,3,7)]
new_ref95<-ref95[c(1,3,7)]
new_ref99<-ref99[c(1,3,7)]
#
new_ref20[new_ref20 <= 0]<-NA
new_ref48[new_ref48 <= 0]<-NA
new_ref75[new_ref75 <= 0]<-NA
new_ref95[new_ref95 <= 0]<-NA
new_ref99[new_ref99 <= 0]<-NA
min(new_ref48)
dim(new_ref99)

colbind
ymean<-rowMeans(cbind(new_ref20$Rad...Ref.., new_ref75$Rad...Ref.., new_ref99$Rad...Ref..), na.rm=TRUE)
ymean
library(plotly)
mean(new_ref20$Rad...Ref..[1], new_ref75$Rad...Ref..[1], new_ref99$Rad...Ref..[1])
(new_ref20$Rad...Ref..[1]+ new_ref75$Rad...Ref..[1]+new_ref99$Rad...Ref..[1])/3

## Regression plots
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

##  Spline fit on the 20%, 75%, and 99%
fmean <- splinefun(x20, ymean, method = "fmm", ties = mean)

## Use the above function to predict the plots for 48% and 95%


plot(x48, fmean(x48), type = 'l')
par(new= TRUE)
plot(x48, y48, pch = 20, cex = .6, col='red')
plot(x95, fmean(x95), type = 'l')
par(new= TRUE)
plot(x95, y95, pch = 20, cex = .6, col='red')


