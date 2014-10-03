#Sarah Bouchat
#PS812 Section 5
#3 October 2014
#Plotting pmfs, pdfs, cdfs in R

#There are many functions in R starting with d-, p- , q-, r- and the name of the distribution. These allow you to compute the density, distribution function, quantile function, and random deviates (respectively). Our primary concern will be with d- and p- functions.

#Aside: we can create our own "e" value this way
e<-exp(1)
1/(6*e)

###############################################
#1: OBTAINING PROBABILITIES USING DISTRIBUTIONS
###############################################


#Suppose we ant to use a Poisson distribution and find the probability that an outcome is *greater* than or equal to 2. Initially you might panic because we only know how to do CDFs with less than or equal to. But, greater than or equal to is the same thing as the total probability of the outcome minus the less than/equal to value. Like so:
1-ppois(2,1)

#We can also use d- to find densities at particular points and add them to get the probability of being in a range. Here we're getting densities of a binomial distribution where the n is 15, and the first argument (before the first comma) in the dbinom() function is telling us the point at which we want the density. That is, dbinom(5,15,.5) is the density where Y=y=5. The third argument (.5) is the binomial probability. This is a value we would be given and is just an example here.
dbinom(5,15,.5)+dbinom(6,15,.5)+dbinom(7,15,.5)+dbinom(8,15,.5)+dbinom(9,15,.5)+dbinom(10,15,.5)

#If I wanted to do this same thing with pbinom it would look like:
1-pbinom(4,15,.5)

#Careful! There are yet other ways to get P(Y>y). You can use one of the final optional arguments in pbinom (or p-(insert distribution here)). The default setting is lower.tail=TRUE. This means that pbinom is computing P(Y<=y). If you set lower.tail=FALSE, pbinom will compute P(Y>y).

#These functions exist for many distributions, including uniform (unif), geometric (geom), hypergeometric (hyper), etc.


###############################################
#2: PLOTTING
###############################################

#On your problem set for this week, you have the option of plotting your CDF and PDF in R or by hand. There are a few different ways to do this in R

################
# (1) By Hand
################

#First I want to create a sequence of values that my function can take on, effectively its range and how many "slices" I want there to be in that range
y<-seq(-1,1,length=1000) 

#Then I can specify my variables as they're expressed in the CDF function. Imagine each of these as one line inside that function. You just assign the information in that line to a vector. In this case my vectors are named m, n, and p. 

#Vector m, for example, tells R simply return 0 in the sequence I just created *if* y is less than or equal to 0. This "if" statement is in brackets. Note here that I put 0*y. You can also just put 0, but plot() often will not print the line for those values in that case
m<-0*y[y<=0]

#Next, I create the vector n, which has the value 2*y^2 when y is either strictly greater than 0 or strictly less than 1. The "&" lets R know that the for the first *and* the second conditions, it will perform the same operation (y^2)
n<-(2*y^2)[0<y & y<1]

#Vector p rounds things out. It just produces a value of .75 for any y greater than or equal to 1
p<-.75[1<=y]

#Now I want to tell R that each of these 3 objects/vectors are part of the same function f(y), so I "concatenate" them using c() and put all that information inside the object named "fy"
fy<-c(m,n,p)

#Now I can plot my CDF! Using the plot function, I specify that I want my x values to take on the values in the y sequence I created and the y values to come from the fy "function." The third argument "type=" tells R that I want this to be a line graph. Finally, be careful of the support of your function--that is, what possible values it can take on. The argument "xlim=" tells R to limit your x axis to a set of values. Here I've told R to keep the plot between -1 and 1 on the x axis
plot(y,fy,type="l",xlim=c(-1,1))

################
# (2) Functions
################

#The second way to plot is with functions. Above our "function" is actually just a vector comprised of different vectors itself. Here we will write our own function and "run" it over a series of values

#Start the same way, specifying a sequence. Notice here that I didn't use "length". I used .001 to say that I want the intervals of that sequence to have a width of .001
y<-seq(-1,1,.001)

#Now I will write a function for the CDF. Here the CDF is only a function of y. Rather than writing it piecemeal as a series of vectors, I use "ifelse" to assign values quickly. Here R is being told, when y<0, set the value of the function to 0, and if that's not true, make another determination: is y>1? In that case make the function take on 1. Otherwise, if it isn't either of those first two things, make it take on the value y^2.
fy<-function(y){
  
  ifelse(y<0,0,ifelse(y>1,1,y^2))
}


#We can do the same thing for the PDF

#Here notice that the third nested "ifelse" uses "!=". This means does not equal. So the final argument says if y does not equal 1, then you should make the function take on the value 2*y. If it equals 1, fill in NA because our function isn't defined at that point.
pdfy<-function(y){
  
  ifelse(y<0,0,ifelse(y>1,0,ifelse(y!=1,2*y,NA)))
}

#Rather than using plot, we can also use curve(). Here "from=" and "to=" serve the same function as our "xlim=" in plot(). I've also labeled my axes here, which is good plotting practice.
curve(pdfy,from=-.5,to=1.5,xlab="y",ylab="f(y)")

#If you need help with curve or want to know more, use its help documentation
help(curve)

#Notice that you don't even need the fancy function to plot. But! Notice that here I'm using from/to and xlim together. What do each do?
curve(2*x,from=0, to=1, xlim=c(-.5,1.5),xlab="y",ylab="f(y)")
curve(0*x,from=-0.5,to=0)






#Many thanks to Emily Sellars for past years' section materials
