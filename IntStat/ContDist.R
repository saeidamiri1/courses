plotnorm<-function(mean, sd, lb, ub){
if(is.na(lb)) {
  lb<-mean-5*sd
  
  x <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x,mean,sd)
  plot(x, hx, type="n", xlab=" Values", ylab="", main="Normal Distribution", axes=FALSE)
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P(X<",ub,")=",signif(area, digits=3))
  mtext(result,3)
  axis(1, pos=0)
} else if(is.na(ub)){
    ub<-mean+5*sd 
  x <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x,mean,sd)
  plot(x, hx, type="n", xlab=" Values", ylab="", main="Normal Distribution", axes=FALSE)
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P(",lb,"< X) =",signif(area, digits=3))
  mtext(result,3)
  axis(1, pos=0)
  }else{
    x <- seq(-4,4,length=100)*sd + mean
    hx <- dnorm(x,mean,sd)
    plot(x, hx, type="n", xlab=" Values", ylab="", main="Normal Distribution", axes=FALSE)
    i <- x >= lb & x <= ub
    lines(x, hx)
    polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
    area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
    result <- paste("P(",lb,"< X<", ub,") =",signif(area, digits=3))
    mtext(result,3)
    axis(1, pos=0)
  }

}


plotunif<-function(minimum, maximum, lb, ub){
  if(is.na(lb)) {
    lb<-minimum
  sd<-(maximum-minimum)^2/12
  mean<-(maximum+minimum)/2
  x <- seq(minimum-1.5*sd,maximum+sd*1.5,length=1000)
  hx <- dunif(x,minimum,maximum)
  plot(x, hx, type="n", xlab="Values", ylab="", main="Uniform Distribution", axes=FALSE)
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  area <- punif(ub, minimum,maximum) - punif(lb,minimum,maximum)
  result <- paste("P(X<",ub,") =",signif(area, digits=3))
  mtext(result,3)
  axis(1, pos=0)
}else if(is.na(ub)) {
  ub<-maximum
  sd<-(maximum-minimum)^2/12
  mean<-(maximum+minimum)/2
  x <- seq(minimum-1.5*sd,maximum+sd*1.5,length=1000)
  hx <- dunif(x,minimum,maximum)
  plot(x, hx, type="n", xlab="Values", ylab="", main="Uniform Distribution", axes=FALSE)
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  area <- punif(ub, minimum,maximum) - punif(lb,minimum,maximum)
  result <- paste("P(",lb,"< X) =",signif(area, digits=3))
  mtext(result,3)
  axis(1, pos=0)
} else {
  sd<-(maximum-minimum)^2/12
  mean<-(maximum+minimum)/2
  x <- seq(minimum-1.5*sd,maximum+sd*1.5,length=1000)
  hx <- dunif(x,minimum,maximum)
  plot(x, hx, type="n", xlab="Values", ylab="", main="Uniform Distribution", axes=FALSE)
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  area <- punif(ub, minimum,maximum) - punif(lb,minimum,maximum)
  result <- paste("P(",lb,"< X <",ub,") =",signif(area, digits=3))
  mtext(result,3)
  axis(1, pos=0)
}
}
