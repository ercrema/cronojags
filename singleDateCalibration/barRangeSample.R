


barRangeSample<-function(mcsamples,option=c("normal","uniform","trapezium"),xrange=7000:3200)
{
    require(trapezoid)
    require(scales)
    resmatrix=matrix(NA,nrow=nrow(mcsamples),ncol=length(xrange))
    if (option=="uniform")
        {
            starts=1950-mcsamples$Start
            ends=1950-mcsamples$End
            for (x in 1:nrow(mcsamples))
                {
                    resmatrix[x,]=dunif(xrange,min=ends[x],starts[x])
                }
            resvector=apply(resmatrix,2,sum)
        }
    
    if (option=="normal")
        {
            midpoints=1950-mcsamples$midpoint
            sigmas=mcsamples$sigma
            for (x in 1:nrow(mcsamples))
                {
                    resmatrix[x,]=dnorm(xrange,mean=midpoints[x],sd=sigmas[x])
                }
            resvector=apply(resmatrix,2,sum)
        }

    if (option=="trapezium")
        {
            startstart=1950-mcsamples$StartStart
            endstart=1950-mcsamples$EndStart
            startend=1950-mcsamples$StartEnd
            endend=1950-mcsamples$EndEnd

            for (x in 1:nrow(mcsamples))
                {
                    resmatrix[x,]=dtrapezoid(as.numeric(xrange),min=endend[x],mode1=startend[x],mode2=endstart[x],max=startstart[x])
                }
            resvector=apply(resmatrix,2,sum)
        }
return(list(xrange=xrange,raw=resvector,rescaled=rescale(resvector)))   
}


barRangeplot<-function(lowY,hiY,x,y,red=0,green=0,blue=0)
{
            halfresolution=abs(x[2]-x[1])/2
            for (i in 1:length(x))
                {
                    rect(xleft=x[i]-halfresolution,xright=x[i]+halfresolution,ybottom=lowY,ytop=hiY,col=rgb(red,green,blue,alpha=y[i]),border=NA)
                }

}
