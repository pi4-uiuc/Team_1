library(readr)
ref20 <- read.csv("~/Team_1/data/Ref_Factor_20.csv")
ref48 <- read.csv("~/Team_1/data/Ref_Factor_48.csv")
ref75 <- read.csv("~/Team_1/data/Ref_Factor_75.csv")
ref95 <- read.csv("~/Team_1/data/Ref_Factor_95.csv")
ref99 <- read.csv("~/Team_1/data/Ref_Factor_99.csv")
#colnames(ref20)
# Delelting all those rows/observations for which Reflective percentage is above 100
row20_to_keep<- which(ref20$Reflect...<=100)
ref20<- ref20[row20_to_keep, ]
row48_to_keep<- which(ref48$Reflect...<=100)
ref48<- ref48[row48_to_keep, ]
row75_to_keep<- which(ref75$Reflect...<=100)
ref75<- ref75[row75_to_keep, ]
row95_to_keep<- which(ref95$Reflect...<=100)
ref95<- ref95[row95_to_keep, ]
row99_to_keep<- which(ref99$Reflect...<=100)
ref99<- ref99[row99_to_keep, ]

# check the max of the %Reflection column now
max(ref20$Reflect...)
max(ref48$Reflect...)
max(ref75$Reflect...)
max(ref95$Reflect...)
max(ref99$Reflect...)
View(ref20)

# Check the dimension of the new data after deleting values for which %Reflection > 100

# hmmm, the new data does not have the  same dimension for 20%, 48%,  75%, 95%, and 99%   Ask David about it? 
# Next merge all this data into one data table, and we only need one column for wave length since the
# wave lenth column has same values for the all percentage files (i.e, 20%,48%,  75%, 95%, and 99% )
# Also, I only want to extract Rad(ref) from all data files and want to plot them against wavelength. 
new_ref20<-ref20[c(1,3)]
new_ref48<-ref48[c(1,3)]   #  keep first column (wavelenth) of all bcs dim is not same for all files after cleaning data
new_ref75<-ref75[c(1,3)]
new_ref95<-ref95[c(1,3)]
new_ref99<-ref99[c(1,3)]
#
# Get rid of those rows where we have negative radiation values
row20n_to_keep<- which(new_ref20$Rad...Ref..>0)
new_ref20<- new_ref20[row20n_to_keep, ]
row48n_to_keep<- which(new_ref48$Rad...Ref..>0)
new_ref48<- new_ref48[row48n_to_keep, ]
row75n_to_keep<- which(new_ref75$Rad...Ref..>0)
new_ref75<- new_ref75[row75n_to_keep, ]
row95n_to_keep<- which(new_ref95$Rad...Ref..>0)
new_ref95<- new_ref95[row95n_to_keep, ]
row99n_to_keep<- which(new_ref99$Rad...Ref..>0)
new_ref99<- new_ref99[row99n_to_keep, ]


# Check the new data sets
new_ref20
new_ref48
names(new_ref20[2])
names(new_ref48)
# Now change the names of the column to easy name to work with like Ref20, Ref48, Ref75, Ref95, Ref99 
#names(new_ref20[2])<- paste("Ref20")
#names(new_ref20[1])<- "Ref48"
#names(new_ref20[1])<- "Ref75"
#names(new_ref20[1])<- "Ref95"
#names(new_ref20[1])<- "Ref99"
# Check names now
#names(new_ref20)
#names(new_ref48)
#names(new_ref75)
#names(new_ref95)
#names(new_ref99)
# Next, combine all the new data sets into one single dataset and 
new_ref20
dim(ref20)
dim(ref48)
dim(ref75)
dim(ref95)
dim(ref99)
## Plots here 
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)
#par(mfrow=c(2,3))
x20<- new_ref20$Wvl
y20<-new_ref20$Rad...Ref..
min(y20)
x48<- new_ref48$Wvl
y48<-new_ref48$Rad...Ref..
min(y48)
x75<- new_ref75$Wvl
y75<-new_ref75$Rad...Ref..
min(y75)
x95<- new_ref95$Wvl
y95<-new_ref95$Rad...Ref..
min(y95)
x99<- new_ref95$Wvl
y99<-new_ref95$Rad...Ref..
min(y99)

par(mfrow=c(2,3))
plot(x20, y20, log= "y", type= 'l', lty=2, col="red", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_20% vs Wavelength")
plot(x48, y48, log= "y", type= 'l', lty=2, col="blue", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_48% vs Wavelength")
plot(x75, y75, log= "y", type= 'l', lty=2, col="cyan", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_75% vs Wavelength")
plot(x95, y95, log= "y", type= 'l', lty=2, col="green", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_95% vs Wavelength")
plot(x99, y99, log= "y", type= 'l', lty=2, col="black", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_99% vs Wavelength")

par(mfrow = c(2,3))
 plot(new_ref20$Wvl, new_ref20$Rad...Ref.. , type= 'l',lty=2, col="red", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_20% vs Wavelength")
 plot(new_ref48$Wvl, new_ref48$Rad...Ref.. ,type= 'l',lty=2, col="blue", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_48% vs Wavelength")
 plot(new_ref75$Wvl, new_ref75$Rad...Ref.. ,type= 'l',lty=2, col="cyan", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_75% vs Wavelength")
 plot(new_ref95$Wvl, new_ref95$Rad...Ref.. ,type= 'l',lty=2, col="green", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_95% vs Wavelength")
 plot(new_ref99$Wvl, new_ref99$Rad...Ref..,type= 'l',lty=2, col="black", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_99% vs Wavelength")

 #################################
 
 #   Regression
 
 ###############################
# 20% data 
model20<- lm(y20 ~ 1 + x20) 
 summary(model20)
model20hat<- fitted(model20)
resid20<- y20-model20hat
# 48% data
model48<- lm(y48 ~ x48 +sin(x48)) 
model48hat<- fitted(model48)
resid48<- y48-model48hat

# 75% data

model75<- lm(y75 ~ 1 + x75) 
model75hat<- fitted(model75)
resid75<- y75-model75hat

# 95% data

model95<- lm(y95 ~ 1 + x95) 
model95hat<- fitted(model95)
resid95<- y95-model95hat

# 99% data

model99<- lm(y99 ~ 1 + x99) 
model99hat<- fitted(model99)
resid99<- y99-model99hat

## Regression plots

par(mfrow=c(2,3))
plot(x20, model20hat, type= 'l', lty=2, col="red", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_20% vs Wavelength")
plot(x48, model48hat,  type= 'l', lty=2, col="blue", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_48% vs Wavelength")
plot(x75, model75hat, type= 'l', lty=2, col="cyan", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_75% vs Wavelength")
plot(x95, model95hat, type= 'l', lty=2, col="green", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_95% vs Wavelength")
plot(x99, model99hat, type= 'l', lty=2, col="black", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_99% vs Wavelength")


### Fitting with Cubic Splines

## 20%

spline_model20<-smooth.spline(x20, y20)
spmod20_hat<-fitted(spline_model20)

## 48% 

spline_model48<-smooth.spline(x48, y48)
spmod48_hat<-fitted(spline_model48)

##  75%

spline_model75<-smooth.spline(x75, y75)
spmod75_hat<-fitted(spline_model75)

## 95%

spline_model95<-smooth.spline(x95, y95)
spmod95_hat<-fitted(spline_model95)

## 99%

spline_model99<-smooth.spline(x99, y99)
spmod99_hat<-fitted(spline_model99)

#######################################################
#
#         Plots for Spline Model
#
#######################################################

par(mfrow=c(2,3))
plot(x20, spmod20_hat, type= 'l', lty=10, col="red", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_20% vs Wavelength")
par(new=TRUE)
plot(x20, y20)
plot(x48, spmod48_hat,  type= 'l', lty=2, col="blue", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_48% vs Wavelength")
par(new=TRUE)
plot(x48, y48)
plot(x75, spmod75_hat, type= 'l', lty=2, col="cyan", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_75% vs Wavelength")
par(new=TRUE)
plot(x75, y75)
plot(x95, spmod95_hat, type= 'l', lty=2, col="green", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_95% vs Wavelength")
par(new=TRUE)
plot(x95, y95)
plot(x99, spmod99_hat, type= 'l', lty=2, col="black", xlab='wavelength', ylab='Radiation(W/m^2) ', main = "Ref_Factor_99% vs Wavelength")
par(new=TRUE)
plot(x99, y99)
















