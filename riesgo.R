library("zoo")
library("imputeTS")
library("plot3D")

MIN_BATTERY <- 30
MAX_BATTERY <- 97

MIN  <- 94.4444444444444 
MED  <- 2891.66666666667 
MEAN <- 3328.72743251454
MAX  <- 8802.77777777778

pdf()

RAD  <- read.csv("radiatio.csv") 

# plot(c(MIN,MED,MEAN),FIT[1,])
# for (i in 2:TEST) points(c(MIN,MED,MEAN),FIT[i,])

DATES    <- seq(as.Date("1985/3/1"), as.Date("2019/11/30"), "day")
RAD$DATE <- base::as.Date(paste(RAD$AÑO,RAD$MES,RAD$DIA,sep="/"))
a <- zoo(rep(0,length(DATES)),order.by=base::as.Date(DATES))
b <- zoo(RAD$RGLODIA,order.by=RAD$DATE)
c <- merge(a,b)
z <- na_random(c$b)
plot(ggplot_na_imputations(c$b,z))

# REG  <- read.csv("regresion.csv")
REG2 <- read.csv("regresion2.csv",header=F)

TEST <- length(REG2[[1]])
END  <- length(DATES)

COEF <- matrix(ncol=2,nrow=TEST)
ERR  <- matrix(ncol=1,nrow=TEST)
FIT  <- matrix(ncol=3,nrow=TEST)
RISK <- matrix(ncol=3,nrow=TEST)

for (i in 1:TEST)
{
  rad  <- c(REG2[i,1],REG2[i,3],REG2[i,5])
  dsoc <- c(REG2[i,2],REG2[i,4],REG2[i,6])
  
  model    <- lm(dsoc~rad)
  COEF[i,] <- coef(model)
  ERR[i]   <- mean(abs(residuals(model))/abs(fitted.values(model)))
  FIT[i,]  <- fitted.values(model)

  j        <- 2
  AUX      <- matrix(ncol=3,nrow=END)
  AUX[1,]  <- c(as.numeric(z[1]),coef(model)[1]+coef(model)[2]*as.numeric(z[1]),70)
  coef_a   <- coef(model)[1]
  coef_b   <- coef(model)[2]
  while(AUX[j-1,3] >= MIN_BATTERY & j <= END)
  {
    rad     <- as.numeric(z[j])
    delta   <- coef_a + coef_b*rad
    AUX[j,] <- c(rad,delta,min(AUX[j-1,3]+delta,MAX_BATTERY))
    j       <- j + 1 
  }
  RISK[i,]  <- c(REG2$V9[i],REG2$V10[i],j)

  plot(zoo(AUX,order.by=DATES),main=paste("PV =",REG2$V9[i],"ESS =",REG2$V10[i]),xlab="",ylab=c("radiación","DeltaSOC","SOC"))
}

write.csv(RISK,"risk.csv")
scatter3D(RISK[,1],RISK[,2],RISK[,3])

dev.off()


