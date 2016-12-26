#By AnnieTran
#Building Delay Block Release Transition Matrix

remove(list=ls())
setwd('/Users/isarasuntichotinun/Desktop/ANNIE/ISE 560/Project/Data')
require(fitdistrplus)
require(MASS)


cleandata=read.csv('Clean Combined May.csv')

#subset the data to show defects that actually get fixed
defectfixed = cleandata$Defect.Number[cleandata$Changed.From=='Verify'&cleandata$Changed.To=='Closed']
#only store defects that were fixed
cleandata=cleandata[cleandata$Defect.Number %in% defectfixed,]

countdata=data.frame(unique(cleandata$Defect.Number))
colnames(countdata)='count'
j=1
for (i in 1:length(countdata$count)){
  while (countdata$count[i]==cleandata$Defect.Number[j] 
         & is.na(cleandata$Defect.Number[j])==FALSE){
    j=j+1
  }
  countdata$count[i] = j-1
}

countdata$countbegin=unique(cleandata$Defect.Number)

for (i in 1:length(countdata$count)){  
  if (i ==1){
    countdata$countbegin[i] = 1
  }else{
    countdata$countbegin[i] = countdata$count[i-1]+1
  }
}

timetofix = data.frame(unique(cleandata$Defect.Number))
colnames(timetofix) = 'defectnum'
timetofix$days = unique(cleandata$Defect.Number)
for (i in 1:length(countdata$count)){
  start=countdata$countbegin[i]
  end = countdata$count[i]
  timetofix$days[i]=as.integer(as.Date(cleandata$Timestamp[end])-as.Date(cleandata$Timestamp[start]))
}

#barplot(timetofix$days,main="Counts")
freqtable = table(as.numeric(timetofix$days))

plot(freqtable)

#fitting distribution another way using fitdistr
fit1 <- fitdistr(timetofix$days, "gamma") #fit gamma distribution
ks.test(timetofix$days, "pgamma", fit1$estimate[1], fit1$estimate[2])#for gamma

fit1 <- fitdistr(timetofix$days, "exponential") #fit exponential distribution
ks.test(timetofix$days, "pexp", fit1$estimate) #goodness of fit test for exponential
#null hypothesis is it came from an exponential distribution

#try lognormal for May
#bad fit
fit<-fitdistr(timetofix$days,"log-normal")
lines(dlnorm(0:max(timetofix$days),fit$estimate[1],fit$estimate[2]), lwd=3,col='red')
ks.test(timetofix$days,"plnorm",fit$estimate[1],fit$estimate[2])


hist(timetofix$days, freq = FALSE, breaks = 100, xlim = c(0, quantile(timetofix$days, 0.99)))
curve(dexp(x, rate = fit1$estimate), col = "red", add = TRUE)
curve(dgamma(x, fit1$estimate[1], fit1$estimate[2]), 0,4000, add=TRUE, 
      col="gray")
curve(dlnorm(x, fit$estimate[1],fit$estimate[2]), col = "red", add = TRUE)

daystofix_feb = 1/fit1$estimate 
febrate = fit1$estimate

#May doesn't fit exponential distribution, just gonna use normal frequency counts
#Q1 = fraction of defects getting fixed before SDV phase
#Q2 = fraction of defects getting fixed after SDV phase but before
#block release ends
#D = fraction of defects getting fixed after the block release
#(block release was delayed)
for (i in 1:nrow(cleandata)){
  
}

#probability that defect will be fixed before SDV phase
#don't know start date for sure, so we'll just use data to determine probability
pexp(63,rate=febrate) #63 working days, 93 all days from 9/1 to 12/2


defectfixedobs = cleandata[cleandata$Changed.From=='Verify'&
                             cleandata$Changed.To=='Closed',]
fixedbeforeSDV = defectfixedobs[as.Date(defectfixedobs$Timestamp) 
                                <=as.Date('2015-12-02'), ]
fixedafterSDV = defectfixedobs[as.Date(defectfixedobs$Timestamp) 
                                > as.Date('2015-12-02') &
                                 as.Date(defectfixedobs$Timestamp)
                               <=as.Date('2016-03-08') , ]

Q1feb = nrow(fixedbeforeSDV)/nrow(defectfixedobs)
Q2feb = nrow(fixedafterSDV)/nrow(defectfixedobs)
dFeb = 0
