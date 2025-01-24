EPI_data <- read.csv("epi2024results06022024.csv")
attach(EPI_data)

# Exercise 1
# EPI
EPI.new

NAs <- is.na(EPI.new)
EPI.new.noNAs <- EPI.new[!NAs] 

summary(EPI.new) 
fivenum(EPI.new,na.rm=TRUE) 

stem(EPI.new) # stem and leaf plot
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)

boxplot(EPI.new, APO.new)

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new) 

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw="SJ"))
rug(EPI.new) 

x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

# Exercise 2
# EPI.new
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

# BDH
NAs <- is.na(BDH.new)
BDH.new.noNAs <- BDH.new[!NAs] 

summary(BDH.new)
fivenum(BDH.new,na.rm=TRUE) 

hist(BDH.new)
lines(density(BDH.new,na.rm=TRUE,bw=1.))
rug(BDH.new) 

plot(ecdf(BDH.new), do.points=FALSE, verticals=TRUE) 
qqnorm(BDH.new); qqline(BDH.new) 
qqplot(rnorm(250), BDH.new, xlab = "Q-Q plot for norm dsn") 
qqline(BDH.new)
qqplot(rt(250, df = 5), BDH.new, xlab = "Q-Q plot for t dsn") 
qqline(BDH.new)

# APO
NAs <- is.na(APO.new)
BDH.new.noNAs <- APO.new[!NAs] 

summary(APO.new)
fivenum(APO.new,na.rm=TRUE) 

hist(APO.new)
lines(density(APO.new,na.rm=TRUE,bw=1.))
rug(APO.new) 

plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE) 
qqnorm(APO.new); qqline(APO.new) 
qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn") 
qqline(APO.new)
qqplot(rt(250, df = 5), APO.new, xlab = "Q-Q plot for t dsn") 
qqline(APO.new)
