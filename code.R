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


##
dat<-read.csv("femaleMiceWeights.csv")

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

xs<-seq(floor(min(x)),ceiling(max(x)),0.1)
xs

plot(xs,ecdf(x)(xs),type="l",
     xlab="Height in inches",ylab="F(x)")

plot(xs,ecdf(x)(xs),
     xlab="Height in inches",ylab="F(x)")

plot(x,ecdf(x)(x))

plot(ecdf(x))

plot(x,type="l")


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
dat<-read.csv("femaleMiceWeight.csv")
dat

control<-filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment<-filter(dat,Diet=="hf")%>%select(Bodyweight)%>%unlist

mean(control)
mean(treatment)

