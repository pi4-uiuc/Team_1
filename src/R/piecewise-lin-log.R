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

#Mean of y20, y75, and y99
Rad_mean<-rowMeans(cbind(new_ref20$Rad...Ref.., new_ref75$Rad...Ref.., new_ref99$Rad...Ref..), na.rm=TRUE)

#Log-scale 
Rad_mean <- log(Rad_mean)

#Mean data-frame
logmean_df <- data.frame('Wvl' = x20, 'Rad. (Ref.)' = Rad_mean)
View(logmean_df)

#Removing NaN from dataframe
newlogmean_df <- logmean_df[complete.cases(logmean_df),]
View(newlogmean_df)

#Reindexing 
rownames(newlogmean_df) <- 1:nrow(newlogmean_df)
View(newlogmean_df)

#Finding critcial points
critpoints <- list()
for( i in (1:1010)){
  if((newlogmean_df[i+1,2]-newlogmean_df[i,2])*(newlogmean_df[i+2,2]-newlogmean_df[i+1,2])< 0){
    critpoints <- append(critpoints, list(c(newlogmean_df[i+1,1],newlogmean_df[i+1,2])))
  }
  
}
print(critpoints)

#Putting critical points in a dataframe
critpoints_df <- data.frame(matrix(unlist(critpoints), nrow=308, byrow=T))
View(critpoints_df)

##Piecewise-linear predictor function 
piecewiseLinear <- function(x, df){
  for(i in 1:153){
    if (df[i,1] < x & df[i+1,1] > x){
      radiation = ((df[i+1,2]-df[i,2])/(df[i+1,1]-df[i,1]))*(x - df[i,1]) + df[i,2]
      return(radiation)
    } 
    else{
      next
    } 
  }
}


## Some predictions of log(Rad. (Ref.))

piecewiseLinear(1110, critpoints_df)
piecewiseLinear(2110, critpoints_df)
piecewiseLinear(2210, critpoints_df)
piecewiseLinear(2410, critpoints_df)

#cleaning df48 and df95
df48 <- data.frame('Wvl' = x48, 'Rad. (Ref.)' = y48)
newdf48 <- df48[complete.cases(df48),]

df95 <- data.frame('Wvl' = x95, 'Rad. (Ref.)' = y95)
newdf95 <- df95[complete.cases(df95),]

##Predicting via log mean 
#y48

plot(x20, log(y20), type = 'l', col='orange', xlab = 'Wavelength', ylab = 'log(radiation)', 
     main = "logTarget 20 vs. logTarget 48")
#par(new=TRUE)
lines(newdf48[,1], log(newdf48[,2]), type = 'l', col='blue', xlab = 'Wavelength', ylab = 'log(radiation)')

plot(newlogmean_df[,1], newlogmean_df[,2], type = 'l', col='orange', xlab = 'Wavelength', ylab = 'log(radiation)', 
     main = "PL-Model vs. logTarget 48")
#par(new=TRUE)
lines(newdf48[,1], log(newdf48[,2]), type = 'l', col='blue', xlab = 'Wavelength', ylab = 'log(radiation)')

#y95
plot(x20, log(y20), type = 'l', col='orange', xlab = 'Wavelength', ylab = 'log(radiation)',
     main = "logTarget 20 vs. logTarget 95")
#par(new=TRUE)
lines(newdf95[,1], log(newdf95[,2]), type = 'l', col='blue', xlab = 'Wavelength', ylab = 'log(radiation)')

plot(newlogmean_df[,1], newlogmean_df[,2], type = 'l', col='orange', xlab = 'Wavelength', ylab = 'log(radiation)',
     main = "PL-Model vs. logTarget 95")
#par(new=TRUE)
lines(newdf95[,1], log(newdf95[,2]), type = 'l', col='blue', xlab = 'Wavelength', ylab = 'log(radiation)')
