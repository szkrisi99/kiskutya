# 1. feladat
# általános statisztikai elemzés
summary(zn)
# Library
library(moments)
# ferdeség vizsgálata
skewness(zn)
# lapultság vizsgálata
kurtosis(zn)
# peremek függetlensége
cor(zn)

# Eloszlás vizsgálata - Sűrűségfüggvény diagrammal
library(ggpubr)
ggdensity(zn[,1], main="Sűrűségfüggvény (1)")
ggdensity(zn[,2], main="Sűrűségfüggvény (2)")

# Eloszlás vizsgálata - Kvantilis diagrammal
library(car)
qqPlot(zn)

# 3. Feladat
library(LSMRealOptions)
set.seed(ss+37) # Reprodukálás
n <- 1 # Egy szimuláció
t <- 500/365 # 500 nap
mu <- ax # Várható érték
sigma <- (ax+az)/(ax+ay+az) # Szórás
S0 <- 100 # A részvény kezdő értéke legyen 100
dt <- 1/365 # Naponta egyszer vizsgálódunk
# Függvény hívása
gbm <- GBM_simulate(n, t, mu, sigma, S0, dt);
plot(gbm, type='l')
#statisztikai elemzés
summary(gbm)
skewness(gbm)
kurtosis(gbm)

# 4. feladat

poisson_folyamat <- function () {
  set.seed(ss+17)
  x <- y <- x.new <- y.new <-x.new.p <- y.new.p <- vector ()
  for (i in 1:500) {
    x <- rpois (1, lambda=2)
    y <- rpois (1, lambda=2) 
    x.new <- c(x.new, x)
    y.new <- c(y.new, y)
    x.new.p <- cumsum(x.new)
    y.new.p <- cumsum(y.new)
    plot (x.new.p, y.new.p, type="b", main=paste("Poisson folyamat\nIdo", i,sep=""),
    xlab="x ertekek", ylab="y ertekek", col=c (rep ("gray", i-1), "red"), pch=c (rep (20,1-1),1))
  }
  poisson_g <- matrix(c(x.new.p,y.new.p), ncol= 2)
  return (poisson_g)
  }
# Meghívás
poisson_generalt <- poisson_folyamat()

# 5. feladat

data <- read.csv("C:/Users/au085553/Downloads/GOOG.csv") #csv fájl beolvasása
logreturn = c()
zaro <- data$Close 
for (i in 1:length(zaro)-1){
  logreturn[i] = abs(log(zaro[i+1]/zaro[i]))
}
chisq.test(logreturn)

#Statisztikák
hist(logreturn, main="záró árak változása")
plot(logreturn) 

# Egy évre becsülés
mu<-mean(logreturn) # logreturn átlaga
sig<-sd(logreturn) # logreturn szórása 
price<-rep(NA,365) # egy év
price[1]<- zaro[length(zaro)] # utolsó ismert érték
#Árak szimulálása
for(i in 2:365){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}
random_data<-cbind(price,1:(365))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)
random_data%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Google (GOOG) árfolyam szimuláció 1 évre")+theme_bw()

