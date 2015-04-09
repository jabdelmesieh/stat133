#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  total.spaces<-r*c
  m<-matrix(sample(c(0,1,2),total.spaces,replace=TRUE,prob=c((1-p), p/2, p/2)),r,c)
  return(m)
}

bml.init(5,6,0.8)

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

test.matrix<-matrix(c(1,0,0,2,1,1,0,0,0,1,2,0,0,2,1,1,1,2,0,0,0,2,0,0,0),5,5)

bml.step.red<-function(m){
  mstar<-m
  blocked<-mstar[,c(2:ncol(m),1)]!=0
  red.blocked<-(blocked)*(m==1)
  red.unblocked<-(m==1)*(!blocked)
  final.red<-(m!=1)*m+(red.blocked)+(re
  red.unblocked[,c(ncol(m),2:ncol(m)-1)])
  return(final.red)
}
bml.step.blue<-function(m){
  mstar<-m
  blocked<-mstar[c(nrow(m),1:nrow(m)-1),]!=0
  blue.blocked<-(blocked)*(m==2)*m
  blue.unblocked<-2*(m==2)*(!blocked)
  final.blue<-(m!=2)*m+(blue.blocked)+(blue.unblocked[c(2:nrow(m),1),])
  return(final.blue)
}
bml.step<-function(m){
  red.shift<-bml.step.red(m)
  blue.shift<-bml.step.blue(red.shift)
  grid.new<-all(blue.shift==m)
  return(list(blue.shift,!grid.new))
  image(blue.shift)
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m<-bml.init(r,c,p)
  i=1
  while(i<=1000){
    step.output<-bml.step(m)
    m<-step.output[[1]]
    grid.new<-step.output[[2]]
    if(grid.new==FALSE){
      summary<-list("Gridlocked Matrix"=m,"Total Timesteps of Gridlock"=i,"Traffic Density"=p)
      return(summary)
    stop("Traffic has Gridlock")}
  else{
    i<-i+1
  }
}
return(list("Free Flowing Matrix"=m, "Total Timesteps"=i-1, "Traffic Density"=p))
}

