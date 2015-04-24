xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}


### 
genBootY = function(x, y, rep = TRUE){
  unlist(tapply(y,x,sample,replace=TRUE))
}

  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  j<-sample(1:length(err))
  newValues<-fit + err[j]
  return(newValues)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if(degree==1){
    reg1<-lm(y~x)
    coeff<-coef(reg1)
  }
  if(degree==2){
    reg2<-lm(y~x+I(x^2))
    coeff<-coef(reg2)
  }
  return(coeff)
}


oneBoot = function(data, fit = NULL, deg = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  
  ### Use fitModel to fit a model to this bootstrap Y 
  if(is.null(fit)){
    z<-genBootY(data$x,data$y)
  }
  else{
    z<-genBootR(data$x,fit$y, rep=TRUE)
  } 
  fitModel(data$x,z,degree=deg)
}

repBoot = function(data, B = 1000){
  a<-t(replicate(B,oneBoot(data,fit=NULL,deg=1)))
  b<-t(replicate(B,oneBoot(data,fit=NULL,deg=2)))
  c<-t(replicate(B,oneBoot(data,deg=1)))
  d<-t(replicate(B,oneBoot(data,deg=2)))
  coeff<-list(a,b,c,d)
  return(coeff)
} 
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
 
bootPlot = function(x, y, coeff, trueCoeff){
  plot(x,y)
  if(ncol(coeff)==2){
    a<-coeff
    mapply(abline, a=a[,1], b=a[,2], col=rgb(1,.2,.8, alpha=0.2))
    curve(0+1*x+1*(x^2), add=TRUE, col="blue")
  }
  if(ncol(coeff)==3){
    b<-coeff
    mapply(function(a,b,c){curve(a+b*x+c*(x^2),add=TRUE, col=rgb(0.2,1,0.8,alpha=0.2))},
           b[,1],b[,2],b[,3])
    curve(0+1*x+1*(x^2), add=TRUE, col="purple")
  }
}
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  ### Make a scatter plot of data

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out


### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}

