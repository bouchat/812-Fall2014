#Sarah Bouchat
#PS812 Week 10
#7 November 2014
#WLLN and ecdfs

#############################
# LAW OF LARGE NUMBERS
#############################
# Use this example to convince yourself of the law of large numbers. This for loop creates draws from a standard uniform. We'll plot it to see if, given a large number of draws, the sample mean converges to the mean mu=.5 as we expect
n<-1000
mu<-.5
means<-rep(0,1000)
for(i in 1:n){
	draws<-runif(i)
	means[i]<-mean(draws)
}
plot(means, type="l", main="Law of Large Numbers", xlab="Sample Size", ylab="Sample Means", col="blue")
abline(h=mu, col="red")


#############################
# PLOTTING EMPIRICAL CDFS
#############################


######### In General #########

# ecdf() gives you the empirical cdf of a set of random draws from a distribution. We can wrap plot() around it in order to visualize it
plot(ecdf(x))

# Remembering that for large enough draws, things start to look very "normal", we might want to overlay the cdf of a normal distribution on our empirical cdf. We can do that after the above plot() command this way:
curve(pnorm(x,2,5), add=TRUE, col="green")

# Notice the add=TRUE argument: this is how R knows to overlay the curve on the already existing plot!
# In this case, we're using pnorm for the distribution function of a normal distribution, and telling R that it should have the same "length" as the empirical cdf we plotted, a mean of 2, and a standard deviation of 5
# Also note that you can change the color argument to make the two lines distinguishable


######### More Examples #########

# A reminder from your previous problem set: we can do plot empirical cdfs with explicit values
values<-c(-2,0,1,4)
Sample1=function(n) sample(values, n, replace=T, prob=c(.4,.1,.3,.2))

# Now you know more concretely why I took a sample of 10000!
Draws<-Sample1(10000)
plot(ecdf(Draws), col.01line="black", main="Problem 2 Empirical CDF", xlab="y", ylab="F(y)")
axis(side=1, at=seq(-4,6, by=1))
axis(side=2, at=seq(0,1,by=.1), labels=TRUE)

# A more general example: we can plot the empirical cdf with just random numbers
n<-100
sample<-rep(0,n)
for(i in 1:n){
	draws<-runif(9)
	sample[i]<-mean(draws)
}
plot(ecdf(sample), col="blue")

# Another way to think about it: we're creating for loops or using apply() to create a vector of means (we're specifying some restrictions then drawing sample means according to those restrictions). Then we're plotting that set of samples

# There many ways to draw samples and plot the ecdf. The one we'll do is with a for loop.

# For loop version (like above).
# Suppose we're asked to draw 10,000 samples from a discrete uniform distribution that goes from 0-10


sd<-sqrt(((10*2.5)/100))

z.sample<-NULL
for(i in 1:10000){
	my.sample<-mean(rbinom(10, 10, .5))
	z.sample<-c(z.sample, my.sample)
}
plot(ecdf(z.sample), main="Empirical CDF", col="blue")
curve(pnorm(x,mean=5, sd=sd), add=TRUE, col="red")

