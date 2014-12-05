##############################
# Chi-squared
##############################

# Let's sort this data by rows and create 2 vectors for the types of war termination
victory<-c(29,5)
settle<-c(6,6)

# Now we can actually reconstruct our table in R using rbind (that is, combine these two vectors in rows)
mytable<-rbind(victory, settle)

# R has a built-in chi-square test that we can use. We set correct=FALSE because we don't (at this point) want to do a continuity correction. This gives us the p-value of our test
chisq.test(mytable, correct=FALSE)

# Alternatively, we can construct our test statistic "by hand" in R

# This gives us our total n
total<-sum(victory,settle)

# Here we're getting the sums of observations in row 1, row 2, column 1, and column 2 respectively
r1<-sum(mytable[1,])
r2<-sum(mytable[2,])
c1<-sum(mytable[,1])
c2<-sum(mytable[,2])

expect.1.1<-(r1*c1)/total
expect.1.2<-(r1*c2)/total
expect.2.1<-(r2*c1)/total
expect.2.2<-(r2*c2)/total

stat.1<-(victory[1]-expect.1.1)^2/expect.1.1
stat.2<-(victory[2]-expect.1.2)^2/expect.1.2
stat.3<-(settle[1]-expect.2.1)^2/expect.2.1
stat.4<-(settle[2]-expect.2.2)^2/expect.2.2

chisqr.stat<-stat.1+stat.2+stat.3+stat.4
chisqr.stat
pchisq(chisqr.stat,df=1,lower.tail=F)

# At what level of significance?
qchisq(.1, df=1, lower.tail=FALSE)
qchisq(.05, df=1, lower.tail=FALSE)
qchisq(.01, df=1, lower.tail=FALSE)

##############################################
# Confidence Intervals and Hypothesis Testing
##############################################

# Enter our data
purple<-c(40.0, 39.5,38.5,39.5,41.4,39.2,39.1,41.2,40.5,39.7)
orange<-c(37.0, 37.5, 36.9, 37.7, 37.5, 38.1, 36.7, 37.7, 37.1, 38.0)

# Now find the sample means and standard deviations that we need to calculate the test statistic
mean.purple<-mean(purple)
mean.orange<-mean(orange)
svar.purple<-var(purple)
svar.orange<-var(orange)

# Pooled sample standard deviation calculation. Remember we're trying to construct, for each one: s^2_p = sum((n-1)*s^2)/sum(n-1)
n<-length(purple)
m<-length(orange)
s.sqr.p<-(svar.purple*(n-1)+svar.orange*(m-1))/((n-1)+(m-1))
s.p<-sqrt(s.sqr.p)

# Now we can "build" our test statistic

test.stat.top<-(mean.purple-mean.orange)-2
test.stat.bottom<-s.p*sqrt((1/n)+(1/m))
test.stat.bottom
test.stat<-test.stat.top/test.stat.bottom
test.stat
1-pt(test.stat,df=18)

#Making the test statistic
help(Tdist)

# Finding the p-value: alternative is specifying the alternative hypothesis, which here is "greater than"
t.test(purple,orange, alternative="g", mu=2, var.equal=T, conf.level=0.95)
