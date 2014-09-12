#Sarah Bouchat
#PS812 Section 2
#12 September 2014

#Doing factorials with R

fact2<-factorial(2)
fact2.byhand<-2*(2-1)
fact10<-factorial(10)
fact10.byhand<-10*9*8*7*6*5*4*3*2*1

#Verify that factorial() gives you the same answer
identical(fact10, fact10.byhand)

#Binomial coefficient: number of combinations of k items that can be selected from a set n

#How many ways can we select paired teams from a class of 10 people? 
#n is 10
#k is 2
#Want to compute: 10 choose 2

#By hand, n choose k is:
#n!/(k!)(n-k!)

#Exercise: Do this with factorial()
Tenchoosetwo<-(factorial(10))/(factorial(2)*factorial(8))
Tenchoosetwo

#But R can do this calculation for us more easily with choose()***
choose<-choose(10,2)

#Verify that this is the same as by hand:
identical(Tenchoosetwo, choose)

#Careful! Order matters
choose(2,10)


#*** Note that another option is nChoosek, which performs the same function. To use this option, however, you will have to load the R.basic package using library(R.basic)