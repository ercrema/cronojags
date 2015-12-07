oxcalReadUniform<-function(fn="~/OxCal/PotteryPhasesSannaiMaruyama/Enokibayash_full.js",ndates,BP=TRUE)
{
    ## Uniform Recovers alpha and beta ##
    text <- readLines(fn,encoding="UTF-8")
    indexStart = 3
    indexEnd = ndates + 5


    ## General Model Properties ##
    general1 <- paste("ocd[0].posterior.overallAgreement=",sep="")
    general2 <- paste("ocd[0].posterior.modelAgreement=",sep="")

    index=pmatch(general1,text)
    overallAgreement=text[index]
    overallAgreement=gsub(".*=","",overallAgreement)
    overallAgreement=as.numeric(gsub(";","",overallAgreement))

    index=pmatch(general2,text)
    modelAgreement=text[index]
    modelAgreement=gsub(".*=","",modelAgreement)
    modelAgreement=as.numeric(gsub(";","",modelAgreement))


    ## Start Date Posterior ##
    
    startIndex1 <- paste("ocd[",indexStart,"].posterior.start=",sep="")
    startIndex2 <- paste("ocd[",indexStart,"].posterior.prob=",sep="")
    startIndex3 <- paste("ocd[",indexStart,"].posterior.resolution=",sep="")
    startIndex4 <- paste("ocd[",indexStart,"].posterior.probNorm=",sep="")
    startIndex5 <- paste("ocd[",indexStart,"].posterior.convergence=",sep="")

    index=pmatch(startIndex1,text)
    start=text[index]
    start=gsub(".*=","",start)
    start.start=as.numeric(gsub(";","",start))
    
    index=pmatch(startIndex2,text)
    probs=text[index]
    probs=gsub(".*=","",probs)
    probs=gsub(";","",probs)
    probs=gsub("\\[","",probs)
    probs=gsub("\\]","",probs)
    probs.start=as.numeric(strsplit(probs,split=", ")[[1]])
    
    index=pmatch(startIndex3,text)
    resolution=text[index]
    resolution=gsub(".*=","",resolution)
    resolution.start=as.numeric(gsub(";","",resolution))

    index=pmatch(startIndex4,text)
    norm=text[index]
    norm=gsub(".*=","",norm)
    norm.start=as.numeric(gsub(";","",norm))

    index=pmatch(startIndex5,text)
    convergence=text[index]
    convergence=gsub(".*=","",convergence)
    convergence.start=as.numeric(gsub(";","",convergence))

    x.start=seq(from=start.start,by=+resolution.start,length.out=length(probs.start))
    if (BP==TRUE) {x.start=abs(x.start)+1950}
    y.start=probs.start*norm.start

    ## End Date Posterior ##

    endIndex1 <- paste("ocd[",indexEnd,"].posterior.start=",sep="")
    endIndex2 <- paste("ocd[",indexEnd,"].posterior.prob=",sep="")
    endIndex3 <- paste("ocd[",indexEnd,"].posterior.resolution=",sep="")
    endIndex4 <- paste("ocd[",indexEnd,"].posterior.probNorm=",sep="")
    endIndex5 <- paste("ocd[",indexEnd,"].posterior.convergence=",sep="")
 
    

    index=pmatch(endIndex1,text)
    start=text[index]
    start=gsub(".*=","",start)
    start.end=as.numeric(gsub(";","",start))
    
    index=pmatch(endIndex2,text)
    probs=text[index]
    probs=gsub(".*=","",probs)
    probs=gsub(";","",probs)
    probs=gsub("\\[","",probs)
    probs=gsub("\\]","",probs)
    probs.end=as.numeric(strsplit(probs,split=", ")[[1]])
    
    index=pmatch(endIndex3,text)
    resolution=text[index]
    resolution=gsub(".*=","",resolution)
    resolution.end=as.numeric(gsub(";","",resolution))

    index=pmatch(endIndex4,text)
    norm=text[index]
    norm=gsub(".*=","",norm)
    norm.end=as.numeric(gsub(";","",norm))

    index=pmatch(endIndex5,text)
    convergence=text[index]
    convergence=gsub(".*=","",convergence)
    convergence.end=as.numeric(gsub(";","",convergence))

    x.end=seq(from=start.end,by=+resolution.end,length.out=length(probs.end))
    if (BP==TRUE) {x.end=abs(x.end)+1950}
    y.end=probs.end*norm.end



    
    return(list(startposterior=data.frame(x=x.start,y=y.start),
                endposterior=data.frame(x=x.end,y=y.end),
                startconvergence=convergence.start,
                endconvergence=convergence.end,
                modelAgreement=modelAgreement,
                overallAgreement=overallAgreement))
}





