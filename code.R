install.packages("rafalib")
library(rafalib)
install.packages("swirl")
library(swirl)

##-----------------------
x<- c(2.23,3.45,1.87,2.11,7.33,18.34,19.23)
mean(x)


sum<-0
for (i in 1:25) {
  sum=sum+i^2
}
sum

cars
class(cars)

nrow(cars)

names(cars)[2]

cars[,2]

mean(cars[,2])

which(cars[,2]==85)


##
x<-1:10
y<-rnorm(10)
plot(x,y)
fit<-lm(y~x)
fit


##
dat<-read.csv("femaleMiceWeights.csv")
head(dat)
names(dat[2])

dat[12,2]

weight<-dat$Bodyweight
weight[11]

length(weight)

seq(3:7)
seq(3,7)

weight<-dat$Bodyweight
mean(weight[13:24])

set.seed(1)
i<-sample(13:24,1)
i
dat$Bodyweight[i]


##
install.packages("dplyr")
library(dplyr)


dat<-read.csv("femaleMiceWeights.csv")
library(dplyr)
controls<-filter(dat,Diet=="chow")
controls2<-filter(dat,Diet=="hf")
controls

ff<-select(controls2,Bodyweight)
ff

unlist(ff)

pp<-filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist
pp

#
summarize(starwars,mean(height,na.rm=TRUE))


##----------------
dat<-read.csv("msleep_ggplot2.csv")
dat

nrow(dat)
dat<-filter(dat,order=="Primates")
nrow(dat)

dat<-filter(dat,order=="Primates")%>%select(sleep_total)
dat

dat<-read.csv("msleep_ggplot2.csv")
dat<-filter(dat,order=="Primates")%>%select(sleep_total)%>%unlist
dat

mean(dat)

#
summarize(starwars,mean(height,na.rm=TRUE))

summarize(dat,mean(sleep_total,na.rm=TRUE))

dat<-filter(dat,order=="Primates")
summarize(dat,mean(sleep_total,na.rm=TRUE))




##-----------------------------
Introduction to Exploratory Data Analysis Histgram

install.packages("UsingR")
library(UsingR)
x=father.son$fheight
x
View(x)
length(x)

round(sample(x,20),1)
sample(x,20)

hist(x,breaks = seq(floor(min(x)),ceiling(max(x))),
    main="Height histgram",xlab="Height in inches")
hist(x,20)
hist(x,breaks=20)
seq(floor(min(x)),ceiling(max(x)))

sum(x>=65 &x<=75)
sum(x>65 & x<75)/length(x)          ##
## proportion ??

xs<-seq(floor(min(x)),ceiling(max(x)),0.1)
xs

plot(xs,ecdf(x)(xs),type="l",
     xlab="Height in inches",ylab="F(x)")          ##



plot(xs,ecdf(x)(xs),
     xlab="Height in inches",ylab="F(x)")

plot(x,ecdf(x)(x))

plot(ecdf(x))

plot(x,type="l")
## random_variables-----------------


# QQ-plot
mean(x)

sd(x)
#sd means standard deviation

a<-mean(x>70)
a
b<-1-pnorm(70,mean(x),sd(x))
b

#if a and b continuously agree(10,12,23,80,90...), normal distribution

#sqrt(9)=3
#sd=sqrt(variance)

mean(x<70)
pnorm(70,mean(x),sd(x))

mean(x<59)
pnorm(59,mean(x),sd(x))

mean(x<79)
pnorm(79,mean(x),sd(x))

ps<-seq(0.01,0.99,0.01)
ps
#seq 
qs<-quantile(x,ps)
qs
quantile(x)

normalqs<-qnorm(ps,mean(x),sd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height")
abline(0,1,col="blue")  ##identity line

## abline function in R
##http://www.sthda.com/english/wiki/abline-r-function-an-easy-way-to-add-straight-lines-to-a-plot-using-r-software
plot(cars)
abline(v=15,col="blue")
abline(h=c(40,80),col=c("blue","red"),lty=c(1,2),lwd=c(1,3))
##third example
set.seed(1234);
dat<-rnorm(200)
hist(dat,col="lightblue")
abline(v=mean(dat),col="red",lwd=3,lty=2)
#Add regression line
abline(1,2)#???
par(mgp=c(2,1,0),mar=c(3,3,1,1))
#fit regression line
require(stats)
reg<-lm(dist ~ speed, data = cars)
coeff=coefficients(reg)
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# plot
plot(cars, main=eq)
abline(reg, col="blue") #????????????? not understand

##
qqnorm(x)
qqline(x)


##-------------------------------
QQ-plot Exercises

load("skew.RData")
dim(dat)
qqnorm(dat)

par(mfrow=c(3,3))

for(i in 1:9)
  qqnorm(dat)

for (variable in vector) {
  
}


## the qqnorm? 
##http://www.sthda.com/english/wiki/qq-plots-quantile-quantile-plots-r-base-graphs
dat<-ToothGrowth
dat
qqnorm(dat$len,pch=1,frame=FALSE)
qqline(dat$len,col="steelblue",lwd=2)


##---------------------------
for (i in 1:9) {
  qqnorm(dat[,i])
  qqline(dat[,i])
}

hist(dat[,1])
hist(dat[,2])
hist(dat[,3])
hist(dat[,4])
hist(dat[,5])
hist(dat[,6])
hist(dat[,7])
hist(dat[,8])
hist(dat[,9])

par(mfrow=c(1,1))


##----------------------------------
# Boxplot
data(exec.pay)
hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)

mean(exec.pay)
median(exec.pay)

##------------------------------------
boxplot(exec.pay,ylab="10,000s of dollars",ylim=c(0,400))

head(InsectSprays)
View(InsectSprays)

boxplot(InsectSprays$count ~ InsectSprays$spray)

boxplot(count ~ spray, data = InsectSprays)

## boxplot
View(airquality)
boxplot(Temp~Month,
        data=airquality,
        main="Different boxplots for each month",
        xlab="Month Number",
        ylab="Degree Fahrenheit",
        col="orange",
        border="brown"
)

##------------------------------------------------
library(UsingR)
dat<-nym.2002
dat

library(dplyr)
data(nym.2002, package="UsingR")

head(dat)

boxplot(time~gender,data=dat)
qqnorm(dat$time)
hist(dat$time,30)

#
#pp<-filter(dat,Diet=="hf")
male<-filter(dat,gender=="Male") %>% select(time) %>% unlist
# have to library(dplyr) first
hist(male)
abline(v=mean(male))

female<-filter(dat,gender=="Female")%>%select(time)%>%unlist
hist(female)
abline(v=mean(female))

#
mypar(1,3)
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))


### end of week one ###



#
par(mfrow=c(3,3))
male<-filter(dat,gender %in% c("Male"))
hist(male$time,20)#?
female<-filter(dat,gender %in% c("Female"))
hist(female$time,20)

##
head(InsectSprays)
View(InsectSprays)
boxplot(count~spray,data=InsectSprays)

par(mfrow=c(3,3))
a<-filter(InsectSprays,spray=="A") %>% select(count) %>% unlist
hist(a)
b<-filter(InsectSprays,spray=="B") %>% select(count) %>% unlist
hist(b)
c<-filter(InsectSprays,spray=="C") %>% select(count) %>% unlist
hist(c)
d<-filter(InsectSprays,spray=="D") %>% select(count) %>% unlist
hist(d)
e<-filter(InsectSprays,spray=="E") %>% select(count) %>% unlist
hist(e)


#####################
par(mfrow=c(3,3))
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
abline(v=mean(females),col="blue",lty=c(1,2),lwd=c(1,3))
hist(males,xlim=c(range( nym.2002$time)))
abline(v=mean(males),col="blue",lty=c(1,2),lwd=c(1,3))

mean(females)-mean(males)


##
library(lattice)
num2<-as.data.frame(nym.2002)
histogram(~time|gender,data=num2)

##---
dat<-ToothGrowth
head(dat)
qqnorm(dat$len)
qqline(dat$len,lwd=2)




##  Week 2
##-----------------------------------------------------------
library(dplyr)
dat<-read.csv("femaleMiceWeights.csv")
dat

control<-filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment<-filter(dat,Diet=="hf")%>%select(Bodyweight)%>%unlist

mean(control)
mean(treatment)

population<-read.csv("femaleControlsPopulation.csv")
population
population<-unlist(population)
population

sample(population,12)
mean(sample(population,12))


##---------------------
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

x<-read.csv("femaleControlsPopulation.csv")%>%unlist
x
mean(x)

set.seed(1)
dat<-sample(x,5)
dat
abs(dat)

abs(mean(x)-mean(dat))

set.seed(1)
X <- sample(x,5)
abs( mean(X) - mean(x) )

set.seed(5)
p<-sample(x,5)
p
abs(mean(p)-mean(x))

set.seed(1)

n <- 10000
null <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  null[i] <- mean(treatment) - mean(control)
}
max(null)
null
hist(null,30)


library(dplyr)
dat<-read.csv("femaleMiceWeights.csv")
control<-filter(dat,Diet=="chow")%>%select(Bodyweight)%>%unlist
treatment<-filter(dat,Diet=="hf")%>%select(Bodyweight)%>%unlist
mean(control)
mean(treatment)
obsdiff<-mean(treatment)-mean(control)
obsdiff

mean(null >= obsdiff)


# Cumulative Distribution Function
smallest <- floor( min(x) )
largest <- ceiling( max(x) )
values <- seq(smallest, largest,len=300)
heightecdf <- ecdf(x)
ecdf(x)
x
values
heightecdf
View(heightecdf)
plot(values, heightecdf(values), type="l",
     xlab="a (Height in inches)",ylab="Pr(x <= a)")


##--------------------------
#Probability Distribution
n <- 100
library(rafalib)
nullplot(-5,5,1,30, xlab="Observed differences (grams)", ylab="Frequency")

totals <- vector("numeric",11)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  nulldiff <- mean(treatment) - mean(control)
  j <- pmax(pmin(round(nulldiff)+6,11),1)
  totals[j] <- totals[j]+1
  text(j-6,totals[j],pch=15,round(nulldiff,1))
  ##if(i < 15) Sys.sleep(1) ##You can add this line to see values appear slowly
}

hist(null, freq=TRUE)
abline(v=obsdiff, col="red", lwd=2)

sum(null>obsdiff)/n
mean(null>obsdiff)
mean(abs(null)>obsdiff)  ## P value 


##-----------------------
library(dplyr)
population<-read.csv("femaleControlsPopulation.csv") %>% unlist

set.seed(1)
n<-1000
null<-vector("numeric",n)
for (i in 1:n) {
  sample<-sample(population,5)
  null[i]<-mean(sample)
}

mean(abs(null)-mean(population)>1)/n

sum(abs(null)-mean(population)>1)/n  ##


mean(abs(null-mean(population)))

mean(abs(null-mean(population))>1)  ##


sum(null-sample>1)/n

sum(abs(null-mean(population))>1)/n  ##

mean(population)-mean(sample)



##---------------
library(dplyr)
population<-read.csv("femaleControlsPopulation.csv") %>% unlist

set.seed(1)
n<-10000
null<-vector("numeric",n)
for (i in 1:n) {
  sample<-sample(population,5)
  null[i]<-mean(sample)
}

sum(abs(null-mean(population))>1)/n



##-------------------

install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
count(gapminder)

i952<-filter(gapminder,year=="1952")
count(i952)
i952h<-select(i952,lifeExp) %>% unlist
plot(i952h)
abline(v=6)

mean(i952h<=40)

##----
library(gapminder)
data(gapminder)
dat1952 = gapminder[ gapminder$year == 1952, ]
x = dat1952$lifeExp
mean(x <= 40)
x


prop=fuction(q) {
  mean(x)
  qs=seq(form=min(x),to=max(x),length=20)
  props=sapply(qs,prop)
  plot(qs,props)
  props=sapply(qs,function(q) mean(x<=q))
  plot(ecdf(x))
  }




##
dat1952 = gapminder[ gapminder$year == 1952, ]
x = dat1952$lifeExp
mean(x <= 40)

prop=function(q) {
  mean(x<=q)
}

qs=seq(min(x),max(x),length=200)

props=sapply(qs,prop)

plot(qs,props)

props=sapply(qs,function(q) mean(x<=q))

plot(ecdf(x))



null<-vector("numeric",) 


##----------------------------
library(dplyr)
x<-read.csv("femaleControlsPopulation.csv") %>% unlist

# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

plot(averages5)
boxplot(averages5)
plot(ecdf(averages5))
hist(averages5)


# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
plot(averages50)
boxplot(averages50)
plot(ecdf(averages50))
hist(averages50)

# proportion
sum(averages50>23 & averages50<25)/length(averages50)

#
mean( averages50 < 25 & averages50 > 23)



##    trying this S curve plot    ##
smallest <- floor( min(x) )
largest <- ceiling( max(x) )
values <- seq(smallest, largest,len=300)
heightecdf <- ecdf(x)
plot(values, heightecdf(values), type="l",
     xlab="a (Height in inches)",ylab="Pr(x <= a)")


## propotion ##
pnorm(25,mean(averages50),sd(averages50))-pnorm(23,mean(averages50),sd(averages50))

#
pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43) 


##--------------------
dat<-read.csv("mice_pheno.csv")
dat
dat<-na.omit(dat)
dat

library(dplyr)
#treatment<-filter(dat,Diet=="hf")%>%select(Bodyweight)%>%unlist
populationx<-filter(dat,Diet=="chow"& Sex=="F") %>% select(Bodyweight) %>% unlist
populationx
mean(populationx)

library(rafalib)
popsd(populationx)

#
set.seed(2)
samplex<-sample(populationx,25)
mean(samplex)


populationy<- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(populationy)
popsd(populationy)

set.seed(2)
sampley<-sample(populationy,25)
mean(sampley)


a<-abs(mean(populationx)-mean(populationy))
b<-abs(mean(samplex)-mean(sampley))
a<-mean(populationx)-mean(populationy)
b<-mean(samplex)-mean(sampley)

abs(a-b)

a<-mean(populationy)-mean(populationx)
b<-mean(sampley)-mean(samplex)

abs(a-b)

# be careful on mistyping



