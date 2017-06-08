#Loading Data
library(readr)
ref20 <- read.csv("~/Team_1/data/Ref_Factor_20.csv")

#Cleaning Data
# Delelting all those rows/observations for which Reflective percentage is above 100
row20_to_keep<- which(ref20$Reflect...<=100)
ref20<- ref20[row20_to_keep, ]

new_ref20<-ref20[c(1,3)]

# Get rid of those rows where we have negative radiation values
row20n_to_keep<- which(new_ref20$Rad...Ref..>0)
new_ref20<- new_ref20[row20n_to_keep, ]

#Log-scale 
new_ref20[,2] <- log(new_ref20[,2])


#Collecting Critical Points 
rownames(new_ref20) <- 1:nrow(new_ref20)
View(new_ref20)

critpoints <- list()
for( i in (500:997)){
  if((new_ref20[i+1,2]-new_ref20[i,2])*(new_ref20[i+2,2]-new_ref20[i+1,2])< 0){
    critpoints <- append(critpoints, list(c(new_ref20[i+1,1],new_ref20[i+1,2])))
  }
  
}
print(critpoints)

#Putting critical points in a dataframe
newdf <- data.frame(matrix(unlist(critpoints), nrow=154, byrow=T))
View(newdf)

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

piecewiseLinear(1110, newdf)
piecewiseLinear(2110, newdf)
piecewiseLinear(2210, newdf)
piecewiseLinear(2410, newdf)

##Piecewise-linear log plot
x = newdf[,1]
y = newdf[,2]

plot(x,y, type = 'l')

