# Please load in the dataset included in the midterm1 directory. It will be
# required to perform the following tasks. The dataset includes data for countries in 2012.

# calculate the mean and the maximum of pop (population) in the dataset. Store these as the
# variables <mean.pop> and <max.pop> respectively.
mean.pop<-mean(SO2012Ctry$pop)
max.pop<-max(SO2012Ctry$pop)
# mean.pop <- your code here
# max.pop <- your code here

# For each country in the dataset, calculate the number of female athletes (Female) divided
# by the total number of athletes (Female + Male). Store this as the variable
# <female.prop>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset.
female<-SO2012Ctry$Female
total<-female+SO2012Ctry$Male
female.prop<-female/total
female.prop
# female.prop <- your code here

# Create the following two subsets of the dataset and store them as variables with the
# indicated names:
# 1) Countries with 0 silver medals: <subset.nosilver>
# 2) Countries with more than or exactly 3 silver medals: <subset.threesilver>
subset.nosilver<-SO2012Ctry[SO2012Ctry$Silver<1,]
subset.nosilver
subset.threesilver<-SO2012Ctry[SO2012Ctry$Silver==3,]
subset.threesilver
# subset.nosilver <- your code here
# subset.threesilver <- your code here

# For each of your subsets, create a vector giving the population size. Store
# these as variables <subset.nosilver.pop> and <subset.threesilver.pop>.

subset.nosilver.pop <- subset.nosilver$pop

subset.threesilver.pop <- subset.threesilver$pop


# Implement the function meanpopByGDPPP. Your function should take the following
# arguments:
#
# <GDPPP.cutoff>: a numeric constant giving a cutoff to subset by
# <GDPPP>: a numeric vector of GDP per person
# <pop>: a numeric vector of populations
#   (this should be the same length as <GDPPP>)
#
# Your function should return the mean of the populations of countries
# whose values in <GDPPP> are strictly less that <GDPPP.cutoff>.

meanpopByGDPPP <- function(GDPPP.cutoff, GDPPP, pop){
  if(GDPPP<GDPPP.cutoff)
  return(sum(as.numeric(SO2012Ctry$pop))/length(SO2012Ctry))
}

# Please create a plot of the proportion of female athletes (y-axis) 
# against the total number of athletes (x-axis). Your plot should include the following 
# features:
# 1) a title "Proportion of female athletes vs Total # athletes"
# 2) axis labels: "Proportion of female athletes" and "Total # athletes"
# 3) plotting character set to 19
# 4) a green horizontal line at female proportion of 0.50.

plot(total,female.prop,pch=19,main="Proportion of Female Athletes vs Total Number of Athletes",xlab="Total Number of Athletes",ylab="Proportion of Female Athletes")
abline(h=0.5,col="green")
