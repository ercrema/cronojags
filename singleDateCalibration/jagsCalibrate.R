

jagsSingleCalibrate<-function(date,error,init=NA,raw=FALSE,plot=TRUE)
{
    require(rjags)
    require(Bchron)
    
    data(intcal13)
    calBP=intcal13[,1]
    C14BP=intcal13[,2]
    C14err=intcal13[,3]
    dataList=list(nDate=1, X=date, sigma=error, calBP=rev(calBP), C14BP=rev(C14BP),C14err=rev(C14err))

    if (is.na(init)){init=calBP[which(abs(C14BP-date)==min(abs(C14BP-date)))[1]]}

    ##Specify JAGS Model
    modelString="
      model{
      for (i in 1:nDate) {
      theta[i] ~ dunif(0,49980)
      mu[i] <- interp.lin(theta[i], calBP[], C14BP[])
      sigmaCurve[i] <- interp.lin(theta[i], calBP[], C14err[])
      tau[i] <- 1/(pow(sigma[i],2)+pow(sigmaCurve[i],2))
      X[i] ~ dnorm(mu[i],tau[i])
      twenty.year[i] <- 20*round(theta[i]/20)
      ten.year[i] <- 10*round(theta[i]/10)
      one.year[i] <- round(theta[i])
 	}
      
      }"

    writeLines(modelString,"./model.txt")
    initsList=list(theta=init)
    jagsModel=jags.model(file="./model.txt",data=dataList,inits=initsList,n.chains=5)
    update(jagsModel,n.iter=3000)
    codaSamples=coda.samples(jagsModel,variable.names=c("one.year"),n.iter=50000)
    if(plot==TRUE)
    {plot(density(codaSamples[[1]],bw=1),xlim=c(max(codaSamples[[1]]),min(codaSamples[[1]])),xlab="cal BP",ylab="density",main="")}
    system("rm ./model.txt")
    if(raw==TRUE){return(codaSamples)}
}
