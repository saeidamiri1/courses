plotnorm<-function(mean, sd, lb, ub){
  x <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x,mean,sd)
  plot(x, hx, type="n", xlab="IQ Values", ylab="", main="Normal Distribution", axes=FALSE)
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P(",lb,"< IQ <",ub,") =",signif(area, digits=3))
  mtext(result,3)
  axis(1, pos=0)
}


plotunif<-function(minimum, maximum, lb, ub){
  sd<-(maximum-minimum)^2/12
  mean<-(maximum+minimum)/2
  x <- seq(-4,4,length=1000)*sd + mean
  hx <- dunif(x,minimum,maximum)
  plot(x, hx, type="n", xlab="Values", ylab="", main="Uniform Distribution", axes=FALSE)
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  area <- punif(ub, minimum,maximum) - punif(lb,minimum,maximum)
  result <- paste("P(",lb,"< IQ <",ub,") =",signif(area, digits=3))
  mtext(result,3)
  axis(1, pos=0)
}