oxcalUniformPhase<-function(id,dates,errors,init=NA,raw=FALSE,oxcalName="uniformPhase",OxCalDirectory="~/OxCal/",OxCalExecute="/Applications/OxCal/bin/OxCalMac",mcnsim=1000,mcinterval=100,BP=TRUE)
{
    fn=paste(OxCalDirectory,oxcalName,".oxcal",sep="")
    export <- file(fn) #create export file

    ## Create OxCal model
    cat("Plot(){\n",file=fn,append=FALSE) #Start Sequence#
    cat("Sequence(){\n",file=fn,append=TRUE) #Start Sequence#
    cat('Boundary("Start");\n',file=fn,append=TRUE)
    cat('Phase()\n',file=fn,append=TRUE)
    cat('{\n',file=fn,append=TRUE)
    for (x in 1:length(dates))
        {
            cat(paste('R_Date(','\"',id[x],'\",',dates[x],',',errors[x],');\n',sep=""),file=fn,append=TRUE)
        }
    cat('};\n',file=fn,append=TRUE)
    cat('Boundary("End");\n',file=fn,append=TRUE)
    cat('};\n',file=fn,append=TRUE)
    cat('Difference("diff","End","Start");\n',file=fn,append=TRUE)
    cat( paste("MCMC_Sample(\"",oxcalName,"\",",mcinterval,",",mcnsim,"){\n",sep=""),file=fn,append=TRUE)
    cat('Date("=Start",Start);\n',file=fn,append=TRUE)
    cat('Date("=End",End);\n',file=fn,append=TRUE)                
    cat('};\n',file=fn,append=TRUE)
    cat('};\n',file=fn,append=TRUE)

    ## Execute Oxcal Function
    excecuter=paste(OxCalExecute,paste(OxCalDirectory,oxcalName,".oxcal",sep=""))
    print("Running OxCal...")
    system(excecuter)

    ## Retrieve Posterior and MCMC Sample
    mcmcSample<-read.csv(paste(OxCalDirectory,oxcalName,".csv",sep=""))
    posterior=oxcalReadUniform(fn=paste(OxCalDirectory,oxcalName,".js",sep=""),ndates=length(dates))

    ## Retrieve Agreement Index
    agsum=oxcalAgreementCheck(fn=paste(OxCalDirectory,oxcalName,".js",sep=""))
    raw=data.frame(id=id,c14=dates,errors=errors,agreement=agsum$individualAgreements)
    return(list(posterior=posterior,raw=raw))
}
