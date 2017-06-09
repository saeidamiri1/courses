
varP<-function(x)  mean((x-mean(x))^2)

percentile<-function(x,p){
  n<-length(x)
  xs<-sort(x)
  r<-n*p
  ab<-n*p-floor(n*p)
  if(ab==0) {return((xs[r]+xs[r+1])/2)}
  else {xs[r+1]}
}


FreqTable<-function(obs, nclass ,delta=.1){
  nclass0<-(nclass+1)
  range0<-seq(min(obs)-delta, max(obs)+delta,length=nclass0)
  l0<- -length(range0)
  low0<-range0[l0]
  high0<-range0[-1]
  
  factor0 <- (cut(obs, breaks=range0))
  xout <- data.frame(table(factor0),ClassMid=(high0+low0)/2)
  
  xout2 <- data.frame(Class=xout$factor0,ClassMid=xout$ClassMid,FreQ=xout$Freq,cumFreq = cumsum(xout$Freq), Relative = prop.table(xout$Freq),cumRel=cumsum(prop.table(xout$Freq)))
  return(xout2)
}



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
  result <- paste("P(X<",ub,")=",signif(area, digits=4))
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
  result <- paste("P(",lb,"< X) =",signif(area, digits=4))
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
    result <- paste("P(",lb,"< X<", ub,") =",signif(area, digits=4))
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

plotalpha <- function(CENTER){
 from = -5; to = 5; n = 1000
 alpha<-1-CENTER
  alt.alpha <-alpha/2
  crit.upper <- qnorm(p = alt.alpha, lower.tail = FALSE) 
  crit.lower <- qnorm(p = alt.alpha, lower.tail = TRUE) 
  cord.x1 <- c(from, seq(from = from, to = crit.lower, 
                         length.out = 100), crit.lower) 
  cord.y1 <- c(0, dnorm(x = seq(from = from, to = crit.lower, 
                                 length.out = 100),), 0) 
  cord.x2 <- c(crit.upper, seq(from = crit.upper, to = to, 
                               length.out = 100), to) 
  cord.y2 <- c(0, dnorm(x = seq(from = crit.upper, to = to, 
                                 length.out = 100),), 0) 
  curve(dnorm(x), from = from, to = to, 
        n = n, col = "black", lty = 1, lwd = 2, 
        ylab = "Density", xlab = "Values") 

    polygon(x = cord.x1, y = cord.y1, col = 'red') 

    polygon(x = cord.x2, y = cord.y2, col = "red") 
    result <- paste("CENTER=",CENTER, ",d=",round(qnorm(p = alt.alpha, lower.tail = FALSE),digits = 3))
    mtext(result,3)

} 

  
  
  p.valuemean<-function(X=NA,mean=NA,mu0=0,sd=NA,n=NA, alternative = c("two.sided", "less", "greater")){
 
  if(!is.na(mean)==TRUE){
    z0<-(mean-mu0)/(sd/n^.5)
    if(alternative=="less"){
      p.value<-pnorm(z0)
    } else if(alternative=="greater"){
      p.value<-1-pnorm(z0)
    }else{
      p.value<-2*min(pnorm(z0),pnorm(z0))
    }
  }
  
  
   if((!is.na(sd)==TRUE)&(!is.na(mean)==FALSE)){
    z0<-(mean(X)-mu0)/(sd/length(X)^.5)
    if(alternative=="less"){
      p.value<-pnorm(z0)
    } else if(alternative=="greater"){
      p.value<-1-pnorm(z0)
    }else{
      p.value<-2*min(pnorm(z0),pnorm(z0))
    }
  }
    if((is.na(sd)==TRUE)&(!is.na(mean)==FALSE)){
      p.value<-t.test(X,mu=mu0,alternative=alternative)$p.value
  }
  p.value
}

    
    
    
    TestmeanZ<-function(mean=NA,mu0=NA,sigma=NA,n=NA,alpha=0.05, alternative = c("two.sided", "less", "greater")){
  a0<-paste("The alpha you chose is", alpha)
  if(!is.na(mean)==TRUE){
    z0<-(mean-mu0)/(sigma/n^.5)
    if(alternative=="less"){
      p.value<-pnorm(z0)
      a1<-paste("\n The Rejection Area is Z< -Z",alpha,sep="")
      a2<-paste(" \n Z=",round(z0,4))
      a3<-paste("\nZ",alpha,sep="","=",round(-qnorm(alpha),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    } else if(alternative=="greater"){
      p.value<-1-pnorm(z0)
      a1<-paste("\n The Rejection Area is Z> Z",alpha,sep="")
      a2<-paste(" \n Z=",round(z0,4)) 
      a3<-paste("\nZ",alpha,sep="","=",round(-qnorm(alpha),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }else{
      p.value<-2*min(pnorm(z0),1-pnorm(z0))
      a1<-paste("\n The Rejection Area is |Z|> Z",alpha/2,sep="")
      a2<-paste(" \n Z=",round(z0,4)) 
      a3<-paste("\n Z",alpha/2,sep="","=",round(-qnorm(alpha/2),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }
  }
}


TestmeanT<-function(mean=NA,mu0=NA,sd=NA,n=NA,alpha=0.05, alternative = c("two.sided", "less", "greater")){
  a0<-paste("The alpha you chose is", alpha)
  if(!is.na(mean)==TRUE){
    z0<-(mean-mu0)/(sd/n^.5)
    if(alternative=="less"){
      p.value<-pt(z0,n-1)
      a1<-paste("\n The Rejection Area is T< -t",alpha,sep="")
      a2<-paste(" \n T=",round(z0,4))
      a3<-paste("\nt",alpha,sep="","=",round(-qt(alpha, n-1),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    } else if(alternative=="greater"){
      p.value<-1-pt(z0, n-1)
      a1<-paste("\n The Rejection Area is T> t",alpha,sep="")
      a2<-paste(" \n T=",round(z0,4)) 
      a3<-paste("\n t",alpha,sep="","=",round(-qt(alpha, n-1),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }else{
      p.value<-2*min(pt(z0,n-1),1-pt(z0,n-1))
      a1<-paste("\n The Rejection Area is |T|> t",alpha/2,sep="")
      a2<-paste(" \n T=",round(z0,4)) 
      a3<-paste("\n t",alpha/2,sep="","=",round(-qt(alpha/2,n-1),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }
  }
}



Testprop<-function(phat=NA,p0=NA,n=NA,alpha=0.05, alternative = c("two.sided", "less", "greater")){
  a0<-paste("The alpha you chose is", alpha)
  if(!is.na(phat)==TRUE){
    z0<-(phat-p0)/(p0*(1-p0)/n)^.5
    if(alternative=="less"){
      p.value<-pnorm(z0)
      a1<-paste("\n The Rejection Area is Z< -Z",alpha,sep="")
      a2<-paste(" \n Z=",round(z0,4))
      a3<-paste("\nZ",alpha,sep="","=",round(-qnorm(alpha),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    } else if(alternative=="greater"){
      p.value<-1-pnorm(z0)
      a1<-paste("\n The Rejection Area is Z> Z",alpha,sep="")
      a2<-paste(" \n Z=",round(z0,4)) 
      a3<-paste("\nZ",alpha,sep="","=",round(-qnorm(alpha),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }else{
      p.value<-2*min(pnorm(z0),1-pnorm(z0))
      a1<-paste("\n The Rejection Area is |Z|> Z",alpha/2,sep="")
      a2<-paste(" \n Z=",round(z0,4)) 
      a3<-paste("\n Z",alpha/2,sep="","=",round(-qnorm(alpha/2),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }
  }
}



Test2meanZ<-function(mean1=NA,mean2=NA,sigma1=NA,sigma2=NA,n1=NA,n2,alpha=0.05, alternative = c("two.sided", "less", "greater")){
  a0<-paste("The alpha you chose is", alpha)
  if((!is.na(mean1)&!is.na(mean2))==TRUE){
    z0<-(mean1-mean2)/(sigma1^2/n1+sigma2^2/n2)^.5
    if(alternative=="less"){
      p.value<-pnorm(z0)
      a1<-paste("\n The Rejection Area is Z< -Z",alpha,sep="")
      a2<-paste(" \n Z=",round(z0,4))
      a3<-paste("\nZ",alpha,sep="","=",round(-qnorm(alpha),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    } else if(alternative=="greater"){
      p.value<-1-pnorm(z0)
      a1<-paste("\n The Rejection Area is Z> Z",alpha,sep="")
      a2<-paste(" \n Z=",round(z0,4)) 
      a3<-paste("\nZ",alpha,sep="","=",round(-qnorm(alpha),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }else{
      p.value<-2*min(pnorm(z0),1-pnorm(z0))
      a1<-paste("\n The Rejection Area is |Z|> Z",alpha/2,sep="")
      a2<-paste(" \n Z=",round(z0,4)) 
      a3<-paste("\n Z",alpha/2,sep="","=",round(-qnorm(alpha/2),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }
  }
}




Test2meanT<-function(mean1=NA,mean2=NA,sd1=NA,sd2=NA,n1=NA,n2=NA,alpha=0.05, alternative = c("two.sided", "less", "greater")){
  a0<-paste("The alpha you chose is", alpha)
  n<-min(n1,n2)
  if((!is.na(mean1)&!is.na(mean2))==TRUE){
    z0<-(mean1-mean2)/(sd1^2/n1+sd2^2/n2)^.5
    if(alternative=="less"){
      p.value<-pt(z0,n-1)
      a1<-paste("\n The Rejection Area is T< -t",alpha,sep="")
      a2<-paste(" \n T=",round(z0,4))
      a3<-paste("\nt",alpha,sep="","=",round(-qt(alpha, n-1),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    } else if(alternative=="greater"){
      p.value<-1-pt(z0, n-1)
      a1<-paste("\n The Rejection Area is T> t",alpha,sep="")
      a2<-paste(" \n T=",round(z0,4)) 
      a3<-paste("\n t",alpha,sep="","=",round(-qt(alpha, n-1),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }else{
      p.value<-2*min(pt(z0,n-1),1-pt(z0,n-1))
      a1<-paste("\n The Rejection Area is |T|> t",alpha/2,sep="")
      a2<-paste(" \n T=",round(z0,4)) 
      a3<-paste("\n t",alpha/2,sep="","=",round(-qt(alpha/2,n-1),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }
  }
}







Test2meanZ<-function(mean1=NA,mean2=NA,sigma1=NA,sigma2=NA,Dis=0,n1=NA,n2,alpha=0.05, alternative = c("two.sided", "less", "greater")){
  a0<-paste("The alpha you chose is", alpha)
  if((!is.na(mean1)&!is.na(mean2))==TRUE){
    z0<-(mean1-mean2-Dis)/(sigma1^2/n1+sigma2^2/n2)^.5
    if(alternative=="less"){
      p.value<-pnorm(z0)
      a1<-paste("\n The Rejection Area is Z< -Z",alpha,sep="")
      a2<-paste(" \n Z=",round(z0,4))
      a3<-paste("\nZ",alpha,sep="","=",round(-qnorm(alpha),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    } else if(alternative=="greater"){
      p.value<-1-pnorm(z0)
      a1<-paste("\n The Rejection Area is Z> Z",alpha,sep="")
      a2<-paste(" \n Z=",round(z0,4)) 
      a3<-paste("\nZ",alpha,sep="","=",round(-qnorm(alpha),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }else{
      p.value<-2*min(pnorm(z0),1-pnorm(z0))
      a1<-paste("\n The Rejection Area is |Z|> Z",alpha/2,sep="")
      a2<-paste(" \n Z=",round(z0,4)) 
      a3<-paste("\n Z",alpha/2,sep="","=",round(-qnorm(alpha/2),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }
  }
}




Test2meanT<-function(mean1=NA,mean2=NA,sd1=NA,sd2=NA,Dis=0,n1=NA,n2=NA,alpha=0.05, alternative = c("two.sided", "less", "greater")){
  a0<-paste("The alpha you chose is", alpha)
  n<-min(n1,n2)
  if((!is.na(mean1)&!is.na(mean2))==TRUE){
    z0<-(mean1-mean2-Dis)/(sd1^2/n1+sd2^2/n2)^.5
    if(alternative=="less"){
      p.value<-pt(z0,n-1)
      a1<-paste("\n The Rejection Area is T< -t",alpha,sep="")
      a2<-paste(" \n T=",round(z0,4))
      a3<-paste("\nt",alpha,sep="","=",round(-qt(alpha, n-1),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    } else if(alternative=="greater"){
      p.value<-1-pt(z0, n-1)
      a1<-paste("\n The Rejection Area is T> t",alpha,sep="")
      a2<-paste(" \n T=",round(z0,4)) 
      a3<-paste("\n t",alpha,sep="","=",round(-qt(alpha, n-1),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }else{
      p.value<-2*min(pt(z0,n-1),1-pt(z0,n-1))
      a1<-paste("\n The Rejection Area is |T|> t",alpha/2,sep="")
      a2<-paste(" \n T=",round(z0,4)) 
      a3<-paste("\n t",alpha/2,sep="","=",round(-qt(alpha/2,n-1),4))
      a4<-paste("\n pvalue is",round(p.value,5)) 
      cat(a0,a1,a2,a3,a4)
    }
  }
}

