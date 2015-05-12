# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

#initial.doctors<-sample(c(0,1),n.doctors, replace=TRUE)


sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  has_adopted<-matrix(,nrow=n.doctors,ncol=n.days)
  
  for(i in 1:n.days){
    two.doctors<-sample(1:n.doctors,2, replace=FALSE)
    if((initial.doctors[two.doctors[1]]!=initial.doctors[two.doctors[2]]) & runif(n=1)<p){
      initial.doctors[two.doctors[1]]<-1
      initial.doctors[two.doctors[2]]<-1  
    }
   has_adopted[,i]<-initial.doctors
  }
  return(has_adopted)
} 






  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output






# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
initial.doctors<-sample(c(0,1),20,prob=c(0.9,0.1),replace=TRUE)

# Run your function for at least 5 different values of <p> and plot

p2<-sim.doctors(initial.doctors, 20 ,30,0.2)
p4<-sim.doctors(initial.doctors, 20 ,30,0.4)
p5<-sim.doctors(initial.doctors, 20 ,30,0.5)
p6<-sim.doctors(initial.doctors, 20 ,30,0.6)
p8<-sim.doctors(initial.doctors, 20 ,30,0.8)

# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
plot(x=1:30,y=apply(p2,2,sum),col="purple",type="l",ylim=c(4,17),ylab="Number of Converted Doctors",xlab="Days",main="Number of Doctors that COnvert by Day")
lines(x=1:30,y=apply(p4,2,sum),type="l",col="red")
lines(x=1:30,y=apply(p5,2,sum),type="l",col="green")
lines(x=1:30,y=apply(p6,2,sum),type="l",col="yellow")
lines(x=1:30,y=apply(p8,2,sum),type="l",col="blue")
legend("topleft",c("0.2","0.4","0.5","0.6","0.8"),title="Conversion Prob",fill=c("purple","red","green","yellow","blue"))

