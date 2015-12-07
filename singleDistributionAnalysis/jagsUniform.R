jagsUniform<-function(dates,errors,init=NA,raw=FALSE)
{
    require(rjags)
    require(Bchron)
    
    data(intcal13)
    calBP=intcal13[,1]
    C14BP=intcal13[,2]
    C14err=intcal13[,3]
    dataList=list(nDate=length(dates), X=dates, sigma=errors, calBP=rev(calBP), C14BP=rev(C14BP),C14err=rev(C14err),theta.min=0,theta.max=10000)

    theta.init=numeric()
    for (x in 1:length(dates))
        {
            theta.init[x]=calBP[which(abs(C14BP-dates[x])==min(abs(C14BP-dates[x])))[1]]
        }

    
    modelString="
      model{
      
      for (i in 1:nDate) {
      theta[i] ~ dunif(beta,alpha) #likliehood function#
      mu[i] <- interp.lin(theta[i], calBP[], C14BP[])
      sigmaCurve[i] <- interp.lin(theta[i], calBP[], C14err[])
      tau[i] <- 1/(pow(sigma[i],2)+pow(sigmaCurve[i],2))
      X[i] ~ dnorm(mu[i],tau[i])
      twenty.year[i] <- 20*round(theta[i]/20)
      ten.year[i] <- 10*round(theta[i]/10)
      one.year[i] <- round(theta[i])
 	}

      for (i in 1:2) {
        alphaBeta[i] ~ dunif(theta.min, theta.max) #priors#
       }
       param[1:2] <- sort(alphaBeta)
       beta <- param[1]
       alpha <- param[2]

      }"
    writeLines(modelString,"./model.txt")
    initsList=list(theta=theta.init,alphaBeta=c(2000,5000))
    jagsModel=jags.model(file="./model.txt",data=dataList,inits=initsList,n.chains=5)
    update(jagsModel,n.iter=3000)
    post=coda.samples(jagsModel,variable.names=c("alpha","beta"),n.iter=50000)
    plot(density(post[[1]],bw=1),xlim=c(max(c(post[[1]],post[[2]])),min(c(post[[1]],post[[2]]))),xlab="cal BP",ylab="density",main="")
    system("rm ./model.txt")
    return(post)
}
