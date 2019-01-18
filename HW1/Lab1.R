rollLoadedDie <-function(numberOfRolls)
  return(sample(1:6, numberOfRolls, replace=TRUE, prob=c(1,1,1,1,1,5)/10 ))
tMean = (1*.1)+(2*.1)+(3*.1)+(4*.1)+(5*.1)+(6*.5)  
cat("The theory mean is ", tMean)
tVar = (0.1)*(1-tMean)^2+(0.1)*(2-tMean)^2+(0.1)*(3-tMean)^2+(0.1)*(4-tMean)^2+(0.1)*(5-tMean)^2+(0.5)*(6-tMean)^2  
cat("The theory variance is", tVar)
x <- rollLoadedDie(10000)
print(x)
cat("The mean is ",mean(x))
cat("The variance is ",var(x))
hist(breaks=c(0:6),x)
#-------------------------------------
trialSizes <- c(5,10,15,20,25,30,40,50,100,200,300,400,500,1000,2000,3000,4000,5000,10000,20000,30000,100000)
means <- vector(mode="double",length=length(trialSizes))
variances <- vector(mode="double",length=length(trialSizes))
for(i in 1:length(trialSizes))
  {
  rolls <- vector(length=trialSizes[i], mode="double")
  rolls <- rollLoadedDie(trialSizes[i])
  means[i] <- mean(rolls)
  variances[i] <- var(rolls)
  }
plot(log10(trialSizes),means)
lines(log10(trialSizes),rep(tMean,length(trialSizes)))
plot(log10(trialSizes),variances)
lines(log10(trialSizes),rep(tVar,length(trialSizes)))


