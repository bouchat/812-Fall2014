# PS812 Data Analysis Project Guide
# 12 December 2014
# TA: Sarah Bouchat

#######################################
# 1: LOADING DATA
#######################################
library(MASS)
library(foreign)

setwd("/Users/sbouchat/Downloads")

qog<-read.dta("qog_std_cs_20dec13.dta")
# Note that if you read in a .csv file or a different data type (for example, a .asc file), you may need to add an additional argument header=TRUE

#attributes(qog) You can do this with smaller datasets, but maybe don't with qog
summary(qog$wvs_trust)
#Ditto for the above command. With smaller datasets you can just do summary(nameofdataset) but for something this large, you should probably only do summaries of single variables at a time

head(qog) #this just gives you the first few rows. Again, useful for smaller datasets to make sure you've loaded everything correctly

attach(qog)


#######################################
# 2: MANIPULATING DATA
#######################################

# Refresher on creating new variables/dealing with NA's
#######################################

#Practice creating a "new" variable
qog$wdi.log.gdp<-log(wdi_gdp)

#Dealing with missing data: DANGER
qog.nona<-na.omit(qog)

#You'll learn more advanced methods for dealing with missingness (imputation) in MLE


# Dropping and recoding data
#######################################

#Let's use our good friends the snails as an example
data(snails)

#Let's say that the 96th row contained one NA--that lab tech fell asleep and knocked over the snails and they escaped, perhaps
snails.nomiss<-snails[-96,]
#This just drops the single "missing" observation in row 96 [note that this drops THE ENTIRE ROW in which there was a missing observation]
attach(snails.nomiss)

#Note that the documentation for this data thinks this experiment is unusual in its relatively low fatality rate. That might suggest that we want to bin some of the low mortality observations. Let's pretend we don't think there's that much difference between 7 and 8 deaths or 14 and 16 deaths, etc., and just recode into "high death" and "low death" cases

#Here's just one way to do that

snails.nomiss$deathHL<-NA
#We've created a new empty variable rather than rewriting the old Deaths variable

snails.nomiss$deathHL[snails.nomiss$Deaths>=9]<-1
snails.nomiss$deathHL[snails.nomiss$Deaths<9]<-0

#You could also specify other ranges if you were doing a multinomial setup in this way (assuming this is your DV; you can also do this with independent variables, obviously):

snails.nomiss$deathrange<-NA

snails.nomiss$deathrange[snails.nomiss$Deaths==0] <-0
snails.nomiss$deathrange[snails.nomiss$Deaths==1 | snails.nomiss$Deaths==2 | snails.nomiss$Deaths==3 | snails.nomiss$Deaths==4 | snails.nomiss$Deaths==5 | snails.nomiss$Deaths==6 | snails.nomiss$Deaths==7] <-1
snails.nomiss$deathrange[snails.nomiss$Deaths==9 | snails.nomiss$Deaths==10 | snails.nomiss$Deaths==11 | snails.nomiss$Deaths==12 | snails.nomiss$Deaths==14 | snails.nomiss$Deaths==16] <-2


# Changing Data Type
######################################

# Look at the ht_region variable
head(qog$ht_region)
# Yuck, it's a string/character variable. But we can fix that

qog$myregions<-as.numeric(qog$ht_region)
head(qog$myregions)
# Note that you shouldn't leave something like regions as numeric. That implies that there's a meaningful (ordinal) relationship between regions (so that, for example, going from #7, Southeast Asia to #8 South Asia is some sort of meaningful measure)
# Instead, you'll want these to be a factor variable
qog$myregions<-as.factor(qog$myregions)
# You can check the type of variable by using
class(qog$myregions)

# Ditto with our snail deaths
snails$deathfactor<-as.factor(snails$deathrange)

# Subsetting Data
######################################

# Now that we've made regions that are numeric/factors/easily manipulable, let's suppose that I'm only really interested in the effect of democracy on GDP levels in Southeast Asia. Because I don't want to posit a general effect for the whole world, I want to subset my data to only include the Southeast Asian country observations

# We can subset based on the myregions variable values this way
newqog <- qog[ which(qog$myregions==7), ]
# Note that I'm creating a new dataframe here that's just the subset I want. This is because I might want to go back later and do other things with the whole QOG dataset
newqog$cname

# You can also subset by more than one variable with either 'and' (&) or 'or' (|) operators
newqog2 <- qog[ which(qog$myregions==7 & qog$wdi_gdpc>500),]
newqog2$cname
# This is creating a limited dataset that only includes democracies in Southeast Asia. I can also do this with other mathematical operators:

newqog3 <- qog[ which(qog$myregions==7 | qog$dpi_erlc=='1. Right'),]
newqog3$cname
# Make sure you know what values your variables take on. Sometimes things that are unintelligible or missing for a specific reason will be coded something other than "NA" (e.g., 999 or 888)

#######################################
# 3: ANOVA AND REGRESSION
#######################################

# First, ANOVA. You can enter your own data or use some built-in data:
data(mtcars)
attach(mtcars)
t.test(mpg~am, var.equal=T)
linear.model<-lm(mpg~cyl, data=mtcars)
anova(linear.model)
summary(mtcars)

# There's more than one way to do this
library(MASS)
data(iris)
summary(iris)
attach(iris)
oneway.test(Petal.Length~Species, var.equal=T)

linear.fit<-lm(Petal.Length~Species, data=iris)
anova(linear.fit)

anova<-aov(Petal.Length~Species, data=iris)
anova

# My own. Assume we're randomly assigning people to drink coffee (or not) each day for 10 days. Then we measure their diastolic blood pressure
nocup<-c(76,78,90,85,87,82,81,80,79,81)
onecup<-c(81,82,95,83,85,90,95,82,77,87)
twocups<-c(75,81,80,86,79,80,82,77,73,85)

total<-c(nocup, onecup, twocups)
n<-rep(10,3)
groups<-rep(1:3, n)

# Find the means, variances
means<-tapply(total,groups,mean)
variances<-tapply(total,groups,var)

# Make this into a dataframe
data<-data.frame(y=total, group=factor(groups))
fit<-lm(y~groups, data=data)
anova(fit)

help(oneway.test)
attach(data)
oneway.test(y~groups, var.equal=T)

# Bivariate regression

# Suppose, per my ongoing example, that I want to test either the impact of democracy on GDP or GDP on democracy in Southeast Asia, say. I could do just this bivariate regression this way in R
mymodel<-lm(newqog$wdi_gdpc~newqog$fh_ipolity2)
summary(mymodel)

# Or I could do this for the whole set of countries

mymodel2<-lm(qog$wdi_gdpc~qog$fh_ipolity2)
summary(mymodel2)

# Note that here I've put GDP per capita on the "lefthandside" because in OLS there's an underlying assumption that, like a normal distribution, our variable can continuously vary. That is, it's not bounded (as in variables that only take on values of 0 or 1, etc.)


#######################################
# 4: GRAPHICS
#######################################

library(stats) 
library(MASS)
#need this to load truehist()

#### BASICS ####

#Looking at your data: plot the kernel density of variables to see their distribution
attach(trees)
plot(density(trees$Height), main="Trees by Height")

#We can also do simple histograms
mtcars
hist(mtcars$mpg)

#Manually adjust the characteristics of your graph
hist(mtcars$mpg, breaks=c(10,20,30,40), main="Name of my graph", col="blue", border="red", ylab="Frequency", xlab="Miles per Gallon")



#We can also do basic scatterplots to look at our data, and fit a regression line
attach(mtcars)
plot(hp, mpg) 
abline(lm(mpg~hp))
title("Regression of Miles per Gallon on Horsepower")

#Change what kinds of points you use
plot(wt, mpg)
plot(wt, mpg, pch=16)
#There are many options for this argument, but a few useful ones are: 1: default open circle, 0: open square, 2: open triangle, 16: filled circle, 15: filled square, 17: filled triangle, *: *

#You can also manually set the colors, and in some cases fill and border colors. pch=c(21:25) have border and fill color options. With these numbers, use additional arguments col= for the border and bg= for the fill colors.



#There are other ways of distinguishing data. Let's try a line graph

#We'll graph number of hours slept over a single week. Grad school is rough.
me<-c(1.5,3,6,4,1.5,11,9)
roommate<-c(8,2.5,9,5,7,4.5,3)

#Create a plot with the appropriate y axis (when do you get to sleep more than 12 hours?). We'll leave off axis and axis labels so we can do them ourselves
plot(me, type="o", col="blue", ylim=c(0,12), axes=FALSE, ann=FALSE)
#Remember to tell R what kind of punctuation we want for each day. "o" is "overplotted" because we are doing both points and lines, whereas "p" is for points and "l" is for lines. 

#Use "lines" to add a line to a preexisting plot. Make sure we can distinguish my sleep from my roommate's using line type and point type
lines(roommate, type="o", pch=22, lty=2, col="red")

#Just like points, there are many "line type" (lty) options: 0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash. You can also call them using character strings: "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", where "blank" uses ‘invisible lines’ (i.e., does not draw them)

#Create a title outside the main plot command, and change the font for fun
title(main="Sleep: A Study", font.main=4)

#Let's label our axes too, shall we?
axis(1, at=1:7, lab=c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"))
axis(2, at=0:12)

#Alternatively, we could have done axis labels/super labels with:
title(xlab="Days")
title(ylab="Hours Slept")

#Hm, how should we let the reader know whose line is whose? We'll come back to that and more advanced graphics in MLE...


#One of R's strengths relative to other software is graphics. Walk through these examples on your own to see the types of arguments they take and the types of graphics you can produce
example(plot)                       
example(barplot)                     
example(boxplot)
example(dotchart)
example(coplot)
example(hist)
example(fourfoldplot)
example(stars)
example(image)
example(contour)
example(filled.contour)
example(persp)



#######################################
# 5: GETTING RESULTS INTO LATEX
#######################################

#Try these packages
library(xtable)  
library(texreg)
library(stargazer)

#Let's start by saving some models to objects
attach(ToothGrowth)
length.dose.reg<-lm(len~dose)
length.supp.reg<-lm(len~supp)
length.int.reg<-lm(len~dose+supp+dose*supp)


# 1: Texreg
########################

#Now use texreg to pull each of those saved objects into a LaTeX-friendly table
texreg(list(length.dose.reg, length.supp.reg, length.int.reg),
       caption="OLS Estimates",
	   custom.coef.names=c("Intercept","Dose", "Supplement", "Dose*Sup"),
       dcolumn=TRUE,
       custom.model.names=c("Dose Only","Supplement Only", "W/ Interact"))
	   
#Notice that I can reference LaTeX packages from within texreg as well
	   texreg(list(length.dose.reg, length.supp.reg, length.int.reg),
	          caption="OLS Estimates, this time with booktabs!",
	   	   custom.coef.names=c("Intercept","Dose", "Supplement", "Dose*Sup"),
	          dcolumn=TRUE,
	          custom.model.names=c("Dose Only","Supplement Only", "W/ Interact"), booktabs=TRUE)

#Also notice that I can choose to omit variables as I like
	   texreg(list(length.dose.reg, length.supp.reg, length.int.reg),
	          caption="OLS Estimates, but not the intercept!",
	   	   custom.coef.names=c("Intercept","Dose", "Supplement", "Dose*Sup"),
	          dcolumn=TRUE,
	          custom.model.names=c("Dose","Supplement", "Interact"),
			  omit.coef=c("Intercept"))

# 2: Stargazer
########################

#Even simpler, but with different arguments

stargazer(length.dose.reg, length.supp.reg, length.int.reg, align=TRUE, column.labels=c("Dose Only", "Supplement Only", "w/Interact"), covariate.labels=c("Dose","Supplement","Dose*Supplement", "Intercept"), dep.var.labels=c("Tooth Length"))

# Want more? See these other packages: #http://stackoverflow.com/questions/5465314/tools-for-making-latex-tables-in-r