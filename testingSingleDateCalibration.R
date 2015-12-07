

library(Bchron)
source("./jagsCalibrate.R")
source("./oxcalSingleDate.R")

testDate=5430
testDateError=40


test=BchronCalibrate(ages=testDate,ageSds=testDateError,calCurves="intcal13")
plot(test$date1$ageGrid,test$date1$densities,col=1,type="l",xlab="BP",main="Comparison",xlim=c(max(test$date1$ageGrid),min(test$date1$ageGrid)))
test2=jagsSingleCalibrate(testDate,testDateError,plot=FALSE,raw=TRUE)
lines(density(test2[[1]],bw=1),col=2)
test3=oxcalSingleDate(date=testDate,error=testDateError,plot=FALSE)
lines(test3[[1]]$x,test3[[1]]$y,col=3)
legend("topright",legend=c(),col=c(1,2,3))
