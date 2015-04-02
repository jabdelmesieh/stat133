#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

bml.init <- function(r, c, p){
  total.spaces<-r*c
  m<-matrix(sample(c(0,1,2),total.spaces,replace=TRUE,prob=c((1-p), p/2, p/2)),r,c)
  return(m)
}

bml.step.red<-function(m){
  mstar<-m
  blocked<-mstar[,c(2:ncol(m),1)]!=0
  red.blocked<-(blocked)*(m==1)
  red.unblocked<-(m==1)*(!blocked)
  final.red<-(m!=1)*m+(red.blocked)+(red.unblocked[,c(ncol(m),2:ncol(m)-1)])
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

###10x10 Grid
result.matrix.10<-matrix(ncol=7,nrow=1000)
p<-c(0.1,0.3,0.5,0.6,0.7,0.8,0.9)
for (i in 1:7){
  vector<-replicate(1000,bml.sim(10,10,p[i])[[2]])
  result.matrix.10[,i]<-vector
}
result.vector.10<-c()
for (i in 1:7){
  total.timestep.gridlock.10<-sum(result.matrix.10[result.matrix.10[,i]!=1000,i])
  total.gridlock.10<-length(result.matrix[result.matrix.10[,i]!=1000,i])
  average.gridlock.10<-total.timestep.gridlock.10/total.gridlock.10
  result.vector.10<-c(result.vector.10,average.gridlock.10)
}
vector.percentages.10<-vector()
for(i in 1:7){
  number.gridlock.10<-sum(result.matrix.10[,i]<1000)
  total<-1000
  percentage.10<-number.gridlock.10/total
  vector.percentages.10<-c(vector.percentages.10,percentage.10)
}

###20x20 Grid
result.matrix.20<-matrix(ncol=7,nrow=1000)
p<-c(0.1,0.3,0.5,0.6,0.7,0.8,0.9)
for (i in 1:7){
  vector<-replicate(1000,bml.sim(20,20,p[i])[[2]])
  result.matrix.20[,i]<-vector
}
result.vector.20<-c()
for (i in 1:7){
  total.timestep.gridlock.20<-sum(result.matrix.20[result.matrix.20[,i]!=1000,i])
  total.gridlock.20<-length(result.matrix[result.matrix.20[,i]!=1000,i])
  average.gridlock.20<-total.timestep.gridlock.20/total.gridlock.20
  result.vector.20<-c(result.vector.20,average.gridlock.20)
}
vector.percentages.20<-vector()
for(i in 1:7){
  number.gridlock.20<-sum(result.matrix.20[,i]<1000)
  total<-1000
  percentage.20<-number.gridlock.20/total
  vector.percentages.20<-c(vector.percentages.20,percentage.20)
}
###50x50 Grid
result.matrix.50<-matrix(ncol=7,nrow=1000)
p<-c(0.1,0.3,0.5,0.6,0.7,0.8,0.9)
for (i in 1:7){
  vector<-replicate(1000,bml.sim(50,50,p[i])[[2]])
  result.matrix.50[,i]<-vector
}
result.vector.50<-c()
for (i in 1:7){
  total.timestep.gridlock.50<-sum(result.matrix.50[result.matrix.50[,i]!=1000,i])
  total.gridlock.50<-length(result.matrix.50[result.matrix.50[,i]!=1000,i])
  average.gridlock.50<-total.timestep.gridlock.50/total.gridlock.50
  result.vector.50<-c(result.vector.50,average.gridlock.50)
}
vector.percentages.50<-vector()
for(i in 1:7){
  number.gridlock.50<-sum(result.matrix.50[,i]<1000)
  total<-1000
  percentage.50<-number.gridlock.50/total
  vector.percentages.50<-c(vector.percentages.50,percentage.50)
}
###50x50 Grid for specific values
result.matrix.504<-matrix(ncol=5,nrow=1000)
p<-c(0.38,0.4,0.42,0.44,0.46)
for (i in 1:5){
  vector<-replicate(1000,bml.sim(50,50,p[i])[[2]])
  result.matrix.504[,i]<-vector
}
result.vector.504<-c()
for (i in 1:5){
  total.timestep.gridlock.504<-sum(result.matrix.504[result.matrix.504[,i]!=1000,i])
  total.gridlock.504<-length(result.matrix.504[result.matrix.504[,i]!=1000,i])
  average.gridlock.504<-total.timestep.gridlock.504/total.gridlock.504
  result.vector.504<-c(result.vector.504,average.gridlock.504)
}
vector.percentages.504<-vector()
for(i in 1:5){
  number.gridlock.504<-sum(result.matrix.504[,i]<1000)
  total<-1000
  percentage.504<-number.gridlock.504/total
  vector.percentages.504<-c(vector.percentages.504,percentage.504)
}
###100x100 Grid
result.matrix.100<-matrix(ncol=7,nrow=1000)
p<-c(0.1,0.3,0.5,0.6,0.7,0.8,0.9)
for (i in 1:7){
  vector<-replicate(1000,bml.sim(100,100,p[i])[[2]])
  result.matrix.100[,i]<-vector
}
result.vector.100<-c()
for (i in 1:7){
  total.timestep.gridlock.100<-sum(result.matrix.100[result.matrix.100[,i]!=1000,i])
  total.gridlock.100<-length(result.matrix.100[result.matrix.100[,i]!=1000,i])
  average.gridlock.100<-total.timestep.gridlock.100/total.gridlock.100
  result.vector.100<-c(result.vector.100,average.gridlock.100)
}
vector.percentages.100<-vector()
for(i in 1:7){
  number.gridlock.100<-sum(result.matrix.100[,i]<1000)
  total<-1000
  percentage.100<-number.gridlock.100/total
  vector.percentages.100<-c(vector.percentages.100,percentage.100)
}
###10x20 Grid
result.matrix.101<-matrix(ncol=7,nrow=1000)
p<-c(0.1,0.3,0.5,0.6,0.7,0.8,0.9)
for (i in 1:7){
  vector<-replicate(1000,bml.sim(10,20,p[i])[[2]])
  result.matrix.101[,i]<-vector
}
result.vector.101<-c()
for (i in 1:7){
  total.timestep.gridlock.101<-sum(result.matrix.101[result.matrix.101[,i]!=1000,i])
  total.gridlock.101<-length(result.matrix[result.matrix.101[,i]!=1000,i])
  average.gridlock.101<-total.timestep.gridlock.101/total.gridlock.101
  result.vector.101<-c(result.vector.101,average.gridlock.101)
}
vector.percentages.101<-vector()
for(i in 1:7){
  number.gridlock.101<-sum(result.matrix.101[,i]<1000)
  total<-1000
  percentage.101<-number.gridlock.101/total
  vector.percentages.101<-c(vector.percentages.101,percentage.101)
}


#Matrices
average.step.matrix<-matrix(c(result.vector.10,result.vector.20,result.vector.50),3,7,byrow=TRUE) 
average.step.matrix
percentage.step.matrix<-matrix(c(vector.percentages.10,vector.percentages.20,vector.percentages.50),3,7,byrow=TRUE)
percentage.step.matrix
matrix.100x10<-matrix(c(vector.percentages.10,vector.percentages.100),2,7,byrow=TRUE)
matrix.100x10

#Plots
#barplot(percentage.step.matrix[1:3,],beside=TRUE,las=3,col=c("red","yellow", "blue"),xlab="Density",ylab="Percentage of Gridlocks",main="Percentage of Gridlocks per Density", )
#legend("topleft",c("10x10","20x20","50x50"),fill=c("red","yellow", "blue"))
#axis(side = 1, at = c(2.5,6.5,10.6,14.4,18.5,22.5,26.7), tick = TRUE,labels=c(0.1,0.3,0.5,0.6,0.7,0.8,0.9))

#barplot(average.step.matrix[1:3,],beside=TRUE,las=3,col=c("red","yellow", "blue"),xlab="Density",ylab="Average Number of Steps Until Gridlock",main="Average Number of Steps Until Gridlock per Density", )
#legend("topleft",c("10x10","20x20","50x50"),fill=c("red","yellow", "blue"))
#axis(side = 1, at = c(2.5,6.5,10.6,14.4,18.5,22.5,26.7), tick = TRUE,labels=c(0.1,0.3,0.5,0.6,0.7,0.8,0.9))

#boxplot(average.step.matrix[1:3,],beside=TRUE,las=3,col=c("red","orange","yellow","green", "blue","purple","pink"),ylab="Average Number of Steps Until Gridlock",main="Average Number of Steps Until Gridlock per Density", )
#legend("topleft",title="Density",c("0.1","0.3","0.5","0.6","0.7","0.8","0.9"),fill=c("red","orange","yellow","green", "blue","purple","pink"))

#barplot(vector.percentages.504,main="Percentage of Gridlocks for a 50x50 grid",xlab="Desnsity",ylab="Percentage of Gridlocks",col=c("red","green","blue","yellow","purple"))
#axis(side=1, at=c(0.7,1.9,3.1,4.3,5.55),labels=c(0.38,0.4,0.42,0.44,0.46),tick=TRUE)
#axis(side=2,at=c(0.2,1))

#barplot(matrix.100x10,beside=TRUE,las=3,col=c("darkblue","darkgreen"),xlab="Density",ylab="Percentage of Gridlocks",main="Percentage of Gridlocks per Density", )
#legend("topleft",c("10x10","100x100"),fill=c("darkblue","darkgreen"))
#axis(side = 1, at = c(2,4.8,7.9,10.9,13.8,17,20), tick = TRUE,labels=c(0.1,0.3,0.5,0.6,0.7,0.8,0.9))

