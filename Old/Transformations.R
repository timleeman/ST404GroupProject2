load("~/Documents/University/Year3/ST404/Assignment2/USACrime.Rda")

USACrime$region[USACrime$region == "Pacific"] <- "West"

USACrime$ownHousMed[USACrime$ownHousMed==500001] <- NA
USACrime$ownHousQrange[USACrime$ownHousQrange==0] <- NA
USACrime$rentMed[USACrime$rentMed==1001] <- NA
USACrime$rentQrange[USACrime$rentQrange==0] <- NA
USACrime.2 <- na.omit(USACrime)

vars <- c(4,5,6,7,8,9,10,11,13,14,16,17,19,21,22,23,24)

posVars <- c()
negVars <- c()
for (i in vars) {
  if(skewness(USACrime.2[,i]) > 0.7){
    posVars <- c(posVars, i)
  }
  if(skewness(USACrime.2[,i]) < -0.7){
    negVars <- c(negVars, i)
  }
}
print(posVars)
print(negVars)

USACrimeTrans <- USACrime.2
for (i in posVars){
  a <- abs(skewness(USACrime.2[,i]))
  b <- abs(skewness(log(USACrime.2[,i])))
  c <- abs(skewness(sqrt(USACrime.2[,i])))
  z <- NA
  y <- min(a,b,c)
  if (y == a){
    z <- "normal"
  }
  else if(y == b){
    z <- "log"
    USACrimeTrans[,i] <- log(USACrimeTrans[,i])
  }
  else{
    z <- "sqrt"
    USACrimeTrans[,i] <- sqrt(USACrimeTrans[,i])
  }
  print(paste(colnames(USACrime.2[i]),z,min(a,b,c)))
}

print(colnames(USACrime.2[13]))
print(skewness(USACrime.2[,13]^9))

