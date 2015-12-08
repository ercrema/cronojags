

ids=1:8
dates=c(3430,3220,3400,3510,3290,3330,3190,3300)
errors=c(30,30,20,20,30,40,30,20)
oxcalRes=oxcalUniformPhase(id=ids,dates=dates,errors=errors,init=NA,raw=FALSE,oxcalName="uniformPhase",OxCalDirectory="~/OxCal/",OxCalExecute="/Applications/OxCal/bin/OxCalMac",mcnsim=1000,BP=TRUE)
jagsRes=jagsUniform(dates=dates,errors=errors,init=NA,raw=FALSE)
lines(oxcalRes$posterior$startposterior$x,oxcalRes$posterior$startposterior$y,col=2)
lines(oxcalRes$posterior$endposterior$x,oxcalRes$posterior$endposterior$y,col=2)
### Slight Mismatch??###
