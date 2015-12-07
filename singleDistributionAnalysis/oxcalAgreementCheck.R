oxcalAgreementCheck<-function(fn)
{
        text <- readLines(fn,encoding="UTF-8")
        index=grep(pattern="posterior.agreement=",text)
        Agreements=numeric(length=length(index))
        for (i in 1:length(index))
            {
                tmp=text[index[i]]
                tmp=gsub(".*=","",tmp)
                Agreements[i]=as.numeric(gsub(";","",tmp))
            }
        index=pmatch("ocd[0].posterior.overallAgreement=",text)
        overallAgreement=text[index]
        overallAgreement=gsub(".*=","",overallAgreement)
        overallAgreement=as.numeric(gsub(";","",overallAgreement))
        if (any(c(Agreements,overallAgreement)<60))
            {
                warning("Poor agreement!")
            }
        return(list(individualAgreements=Agreements,overallAgreement=overallAgreement))
}
