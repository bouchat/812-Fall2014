### Data Analysis Help ###

# Welcome back to R! I'm going to demonstrate today using the R console.
# You can also use RStudio.
# Lesson 1: Comment on your code using the '#' symbol.

#####################################
# STEP 1: SETTING UP YOUR SCRIPT FILE
#####################################

#Begin the script file with a label for yourself and others:

#File Name: PS 812 Data Analysis Project
#Data: [Insert dataset name here]
#Author: Sarah Bouchat
#Date: 9 Dec 2014

#Next, set your working directory. This way you can save throughout without naming new file paths.
#A working directory lets R know where to look for files and data you reference, and where to deposit output you save
setwd("/Users/sbouchat/Dropbox/WISC/Classes/2014-Fall/TAing/812Section")
#If you are using Winstat or the SSCC computers, use your U drive this way
setwd("U:/Section")
#In order to do this, you must have a folder named "Section" (or whatever you'd like to call it) in your U drive already. To get to the U drive, click on the Windows button on the menu bar, and then your name. It should be in the list of drives on the left

#####################################
# STEP 2: LOADING PACKAGES
#####################################

#After you set the working directory and before you load your data, load the packages you need

#Load packages by calling them from the library
library(foreign)
library (MASS)
library(xtable)

#If you've never used a package before, you may have to install it first:
install.packages("reshape")
library(reshape)

#R often automatically loads dependent packages for you, but if not it will let you know

#Don't know what package you need? The internet is your friend. Try StackExchange or CRAN

# For this project, you will almost certainly need to do the following:
install.packages("foreign")
library(foreign)
# This package lets you load in data that is in SPSS, STATA, or csv format. You can also load in table formatted data

#####################################
# STEP 3: DATA
#####################################

#There are many ways to enter data into R

#First, you can enter it manually
#Use '<-' to assign data to a variable/object

#Scalar examples
scalar<-2
X<-3

#Vector or matrix examples
set<-c(1,2,3,4,5,6)
matrix.1<-matrix(set,nrow=3, ncol=2, byrow=F)
matrix.2<-matrix(1,2,3)
matrix.3<-diag(3)

#Need help? You can always ask R
help(matrix)
?matrix
??matrix
#These help pages will often at least tell you the arguments that a function takes. If you need more help...to the google!

#Note that the 'data' can be numeric or string
streetnames<-c("Langdon", "Park", "University", "Observatory")

#Check that R remembers what we've entered and see what each object looks like
scalar
X
matrix.1
matrix.2
matrix.3

#Be careful! R is case sensitive
counting.is.fun<-c(1,2,3)
counting.is.fun
Counting.is.fun

#You can always remove variables with 'rm()'
rm(counting.is.fun)
counting.is.fun

#Check what variables remain in the workspace with 'ls()' (that is list objects)
ls()

#Burn it down: remove everything from the workspace! 
rm(list=ls())

#You can save your work history this way
savehistory(file="filename.Rhistory")

#You can also save objects that you've created (for example, images) directly
pdf(file="myprettypicture.pdf")
math<-function(x){x+1}
values<-c(0:10)
math(values)
plot(curve(math, xlim=c(0,12), ylab="Function Values"))
dev.off()

#The other main way to enter data is to pull in a dataset from outside R
#These datasets can be .csv, .dta, etc. formats
#Note that I'm pulling in data here that you don't have access to. Try this at home with your own data file

##### Here be an important thing! #####

# To use these different data file formats, you need to have the 'foreign' package installed and to tell R what kind of file it's reading. Here I've said "read.dta" because it's a dta file. If it were csv, I would say "read.csv" instead
apsr<-read.dta("apsr.dta")

#After you load it, get to know your data
summary(apsr)
names(apsr)
head(apsr)

#Need to change something but want to do it with a tiny gui? [That is, a pop-up spreadsheet]
fix(apsr)

#You need to either constantly reference the dataset or attach it to reference variables
mean(apsr$local_total)
attach(apsr)
mean(local_total)

#####################################
# STEP 4: BASICS OF COMPUTATION IN R 
# (REVIEW ON YOUR OWN)
#####################################

#You can use R as a fancy calculator if you want
1+1
9^2
sqrt(2)

#You can also store calculations for later use:
Addition<-2+2
Multiplication<-3*5000

#We'll also be using many of R's probability functions in this class
#Don't worry about these right now, just keep them in mind for later

#Use R to find the cdf
pnorm(2,mean=0, sd=1)
pnorm(3,4,5)
x<-pnorm(3,4,5)

#Look up quantiles
qnorm(.5, 4,5)
qnorm(x,4,5)
help(qnorm)

#Find the density
#How likely it is that a fair coin turns up heads exactly 4 times in 10 trials?
dbinom(4,size=10,prob=0.5)

####################################################################
# STEP 5: More advanced things (like MLE) # Warning: Here be dragons
####################################################################

#### FUNCTIONS ####

#Defining your own functions

myfavefunction<-function(x){
	x^2
	}
myfavefunction(2)
myfavefunction(10)


secondfavefunction<-function(y){
	log(y)
	}
	
secondfavefunction(5)
log(5)

#You can also use functions for sequences of values
myseq<-c(1:10)
myfavefunction(myseq)
myfavefunction(c(1:10))
myfavefunction(seq(1,2,3))


#### OLS REGRESSION [Use me for bivariate regression] ####
#Synethic data example
# This part creates data
y<-rnorm(1000,mean=5, sd=.5)
rnorm<-rnorm(1000, mean=10, sd=1)
x<-rep(0,1000)
for(i in 1:1000){
	x[i]<-mean(sample(rnorm, 10, replace=TRUE, prob=NULL))
}
m<-rep(0,1000)
for(i in 1:1000){
	m[i]<-median(sample(rnorm, 10, replace=TRUE, prob=NULL))
}

# This part does OLS. lm() is for "linear model" where the variable in the y spot is my dependent variable, and things after ~ are my independent variables. In this case, both x and m are independent variables. I could add more with + between them. 
simpleOLS<-lm(y~x+m)

# I can get regression output using summary()
summary(simpleOLS)

# If you've loaded in a table package such as xtable [by running library(xtable)] you can get this summary to print out in a Latex-friendly format

mytable<-xtable(summary(simpleOLS))
mytable

#####################################
# DESIDERATA: OTHER USEFUL FUNCTIONS
#####################################

#Find out the class of a vector or object
x<-matrix(1,2,3)
class(x)

#Make a dataframe!
stuff<-c("a", "b", "c")
things<-c(1,2,3)
stuffandthings<-data.frame(stuff, things)

#Bind columns using 'cbind'
a<-c(1,2,3)
b<-c(4,5,6)
g<-cbind(a,b)

#All of the apply functions (apply, sapply, lapply, tapply, etc.)
#Find out more: ??apply
#Use tapply to generate summary statistics
#Example: 
tapply(age, local_total, mean)
#Gives me the mean age of each representative by the number of times they mentioned a "local" issue
	
#If you want to see these just enumerated, use 'table'
table(age, local_total)


#***Many thanks to Emily Sellars for previous years' section notes!***
