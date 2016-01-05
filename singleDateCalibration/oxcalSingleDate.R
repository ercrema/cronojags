oxcalSingleDate<-function(id="tmp01",date,error,oxcalName="singleDate",OxCalDirectory="~/OxCal/",OxCalExecute="/Applications/OxCal/bin/OxCalMac",BP=TRUE,plot=TRUE,curve=c("IntCal13","ShCal13","Marine13"),DeltaR=0,DeltaRsd=0)
    {
        fn=paste(OxCalDirectory,oxcalName,".oxcal",sep="")
        export <- file(fn) #create export file
        cat("Options(){};\n",file=fn,append=FALSE) #Start Sequence#
        cat("Plot(){\n",file=fn,append=TRUE) #Start Sequence#
        if (curve=="Marine13")
            {
                cat('Curve("Marine13.14c");',file=fn,append=TRUE)
                cat(paste('Delta_R(',DeltaR,',',DeltaRsd,');\n',sep=""),file=fn,append=TRUE)
            }
        if (curve=="ShCal13")
            {
                cat('Curve("ShCal13");',file=fn,append=TRUE)
            }

        if (curve=="IntCal13")
            {
                cat('Curve("IntCal13");',file=fn,append=TRUE)
            }
        cat(paste('R_Date(','\"',id,'\",',date,',',error,');\n',sep=""),file=fn,append=TRUE)
        cat('};\n',file=fn,append=TRUE)
        excecuter=paste(OxCalExecute,paste(OxCalDirectory,oxcalName,".oxcal",sep=""))
        print("Running OxCal...")
        system(excecuter)
        text <- readLines(paste(OxCalDirectory,oxcalName,".js",sep=""),encoding="UTF-8")
        start <- "ocd[3].likelihood.start"
        resolution <- "ocd[3].likelihood.resolution"
        prob <- "ocd[3].likelihood.prob"
        probNorm <- "ocd[3].likelihood.probNorm"

        if (curve=="Marine13")
            {
        start <- "ocd[4].likelihood.start"
        resolution <- "ocd[4].likelihood.resolution"
        prob <- "ocd[4].likelihood.prob"
        probNorm <- "ocd[4].likelihood.probNorm"
            }

        start.index=pmatch(start,text)
        start=text[start.index]
        start=gsub(".*=","",start)
        start=as.numeric(gsub(";","",start))

        probNorm.index=pmatch(probNorm,text)
        probNorm=text[probNorm.index]
        probNorm=gsub(".*=","",probNorm)
        probNorm=as.numeric(gsub(";","",probNorm))


        resolution.index=pmatch(resolution,text)
        resolution=text[resolution.index]
        resolution=gsub(".*=","",resolution)
        resolution=as.numeric(gsub(";","",resolution))

        prob.index=pmatch(prob,text)
        prob=text[resolution.index+1]
        prob=gsub(".*=","",prob)
        prob=gsub(";","",prob)
        prob=gsub("\\[","",prob)
        prob=gsub("\\]","",prob)
        prob=as.numeric(strsplit(prob,split=", ")[[1]])

        x=seq(from=start,by=+resolution,length.out=length(prob))
        if (BP==TRUE)
            {x=-x+1950
             if (plot==TRUE)
                 {
                     plot(x,y=prob*probNorm, xlim=c(max(x),min(x)),
                          type="l",xlab="BP",ylab="density",main="",lwd=0)
                     polygon(x=c(x,rev(x)),y=c(prob*probNorm,rep(0,length=length(x))),border=NA,col="darkgrey")
                 }
             }

        if (BP==FALSE)
            {
                if (all(x>=0))
                    {
                        plot(x,y=prob*probNorm, xlim=c(max(x),min(x)),
                             type="l",xlab="CE",ylab="density",main="",lwd=0)
                        polygon(x=c(x,rev(x)),y=c(prob*probNorm,rep(0,length=length(x))),border=NA,col="darkgrey")

                    }
                if (all(x<=0))
                    {
                        plot(x,y=prob*probNorm, xlim=c(min(x),max(x)),
                             type="l",xlab="BCE",ylab="density",main="",axes=FALSE,lwd=0)
                        polygon(x=c(x,rev(x)),y=c(prob*probNorm,rep(0,length=length(x))),border=NA,col="darkgrey")
                        axis(side=2)
                        axis(side=1,at=pretty(x),labels=abs(pretty(x)))
                        box()
                    }
                if (any(x<=0)&any(x>=0))
                    {
                        plot(x,y=prob*probNorm, xlim=c(min(x),max(x)),
                             type="l",xlab="BCE/CE",ylab="density",main="",axes=FALSE,lwd=0)
                        polygon(x=c(x,rev(x)),y=c(prob*probNorm,rep(0,length=length(x))),border=NA,col="darkgrey")
                        axis(side=2)
                        labels=abs(pretty(x))
                        labels[which(labels==0)]="1BCE/1CE"
                        axis(side=1,at=pretty(x),labels=labels)
                        box()
                    }
            }


        
        
          return(list(data.frame(x=x,y=prob*probNorm)))
    }
