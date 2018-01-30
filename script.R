require(boot)
#function to get N bootstrap samples
getNBootStrapSamples<-function(data, no.Of.Resamples, sample.size){
  samples<-replicate(n = no.Of.Resamples,sample(x=data,size = sample.size,replace = TRUE));
  return(samples);# samples will be in column binded form
}
getBootStrapDistribution<-function(data,no.Of.Resamples, sample.size){ 
  #get Samples
  samples<-getNBootStrapSamples(data,no.Of.Resamples,sample.size); 
  #apply log(mean(x)) column wise, essentially getting bootstrap distribution of theta.hat
  bootstrapDistribution<-apply(X = samples,MARGIN = 2,FUN = function(x) log(mean(x))) 
  return(bootstrapDistribution)
}
calculateBiasAndSE<-function(bootStrapDistribution, original.data){
  #bias* = E(theta*.hat) - theta.hat
  #where theta*.hat is bootstrap Distribution of theta.hat
  bias<-mean(bootStrapDistribution)-log(mean(original.data))
  se <- sd(bootStrapDistribution)
  return(list(bias=bias, standardError=se))
}
solveProject6<-function(){
  require(boot)
  cpu<-scan(file = "cpu.txt");
  set.seed(2008);
  cpu.size<-length(cpu)
  #boot strap distribution of Theta Hat
  bsd<-getBootStrapDistribution(data = cpu,no.Of.Resamples = 1000,sample.size = cpu.size)
  #part(a)
  biasAndSE<-calculateBiasAndSE(bootStrapDistribution = bsd,original.data = cpu)
  
  #part(b)
  #alpha = 0.05
  #calculate lower and upper percentiles from bootstrap distribution
  lower<-0.025*(length(bsd)+1)
  upper<-0.975*(length(bsd)+1)
  percentiles1<-sort(bsd)[c(lower,upper)]
  #part(c)
  #2.5th and 97.5th percentiles of theta.hat - theta = percentile of theta*.hat-theta.hat
  logmean<-log(mean(cpu))
  percentiles2<-percentiles1-logmean
  #CI1 = Normal BS CI
  CI.normal<-c(logmean-biasAndSE$bias-qnorm(0.975)*biasAndSE$standardError,logmean-biasAndSE$bias-qnorm(0.025)*biasAndSE$standardError)
  #CI2 : percentiles method
  CI.percent<-percentiles1
  #CI3: basic
  CI.basic<-c(2*logmean-percentiles1[2], 2*logmean-percentiles1[1])
  return(list(bias=biasAndSE$bias, StandardError=biasAndSE$standardError,Percentiles.Of.Theta.hat=percentiles1,           Percentiles.Of.Theta.hat.minus.Theta=percentiles2,Basic.CI=CI.basic,NormalApproximation.CI=CI.normal, Percentile.CI=CI.percent))
}

#test
log.mean <- function(x,indices) {
  result <- log(mean(x[indices]))
  return(result)
}
npar.boot<-boot(data = cpu,statistic = log.mean,R = 1000,sim = "ordinary", stype = "i")
