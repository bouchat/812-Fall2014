#Sarah Bouchat
#PS812 Section 6
#10 October 2014
#Plotting and distributions in R

##################################
# 0: Distributions
##################################

##### Exponential Distribution #####
x<-seq(0,5,.001)
y<-dexp(x,1)
plot(x,y,type="l", col="orange",ylim=c(0,1),ylab="f(x)")

##### Gamma Distribution #####
x<-seq(0,1,.001)
y<-dgamma(x,2,5)
z<-dgamma(x,1,5)
a<-dgamma(x,2,8)
b<-dgamma(x,2,13)
plot(x,y,type="l", col="red",ylim=c(0,5),ylab="f(x)")
lines(x,z, col="blue")
lines(x,a, col="orange")
lines(x,b, col="green")

##### Beta Distribution #####
x<-seq(0,1,.001)
y<-dbeta(x,30,3)
plot(x,y,type="l", col="blue",ylim=c(0,10),ylab="f(x)")

##################################
# 1: Sampling
##################################

#From your last problem set, question #2
values<-c(-2,0,1,4)
Sample1=function(n) sample(values, n, replace=T, prob=c(.4,.1,.3,.2))
Draws<-Sample1(10000)
plot(ecdf(Draws), col.01line="black", main="Problem 2 Empirical CDF", xlab="y", ylab="F(y)")
axis(side=1, at=seq(-4,6, by=1))
axis(side=2, at=seq(0,1,by=.1), labels=TRUE)

##################################
# 2: Transforming
##################################

#Imagine that we have some transformation of our random variable x such that y=x/3. 
#This means that x=3y

#First let's draw some x's from our distribution
xdraws<-rnorm(1000,0,1)

#Now transform this into our y using what we found above
ydraws<-(xdraws)/3

#Plot that sucker (where by sucker I mean the density)! Use the hints written on your problem set
hist(ydraws, freq=FALSE, col="gray", breaks=30, main="Density of Y")

#Now let's add the curve for the pdf
curve((3*(1/(sqrt(2*pi)))*(exp(-((3*x)^2)/2))), add=TRUE, lwd=1.5)


##################################
# 3: Plotting
##################################


##### Plotting using curve, functions #####
z<-seq(-1,1,.001)
fz<-function(z){
  
  ifelse(z<0,0,ifelse(z>1,1,z^2))
}

pdfz<-function(z){
  
  ifelse(z<0,0,ifelse(z>1,0,ifelse(z!=1,2*z,NA)))
}

curve(fz, from=-1,2, xlab="z", ylab="F(z)", main="CDF")
curve(pdfz,from=-.5,to=1.5,xlab="z",ylab="f(z)", main="pdf")

help(curve)


##### Changing parameters and plotting the pdf #####
x<-seq(-5,5,.001)
w<-dnorm(x,0,.25)
y<-dnorm(x,0,.5)
z<-dnorm(x,0,1)
plot(x,w,type="l", col="red",ylim=c(0,2),ylab="f(x)", lwd=1.5)
lines(x,y,col="blue", lwd=1.5)
lines(x,z,col="green", lwd=1.5)

