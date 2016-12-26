setwd('/Users/isarasuntichotinun/Desktop/ANNIE/ISE 560/Project/Data')

require(MASS)
statechange=read.csv('May/NCSU Data Set 1 - Block 2016 May - State Changes.csv')

statechange$X<-NULL
statechange$X.1<-NULL
statechange$X.2<-NULL

statechange=read.csv('Combined Defect Data.csv')


countdata=data.frame(unique(statechange$Defect.Number))
colnames(countdata)='count'
j=1
for (i in 1:length(countdata$count)){
  while (countdata$count[i]==statechange$Defect.Number[j] 
         & is.na(statechange$Defect.Number[j])==FALSE){
    j=j+1
  }
  countdata$count[i] = j-1
}

countdata$countbegin=unique(statechange$Defect.Number)

for (i in 1:length(countdata$count)){  
  if (i ==1){
    countdata$countbegin[i] = 1
  }else{
    countdata$countbegin[i] = countdata$count[i-1]+1
  }
}

timetofix = data.frame(unique(statechange$Defect.Number))
colnames(timetofix) = 'defectnum'
timetofix$days = unique(statechange$Defect.Number)
for (i in 1:length(countdata$count)){
  start=countdata$countbegin[i]
  end = countdata$count[i]
  timetofix$days[i]=as.integer(as.Date(statechange$Timestamp[end])-as.Date(statechange$Timestamp[start]))
}

#barplot(timetofix$days,main="Counts")
freqtable = table(as.numeric(timetofix$days))

plot(freqtable) #see if it looks like some kind of distribution

# # it actually looks exponential. Yaaaaayyy
#turn table to dataframe so I can fit distribution
freqdataF = as.data.frame(freqtable)
colnames(freqdataF)=c('days','counts')
attach(freqdataF)
expofit <- lm(log(counts)~ log(as.numeric(days)))
expofit <- lm(log(counts)~ as.numeric(days))
expofit$coefficients
summary(expofit) #R^2 = 0.66
detach(freqdataF)
# timevalues <- seq(min(as.numeric(days)),min(as.numeric(days)), 0.1)
# Counts.exponential2 <- exp(predict(expofit,list(Time=timevalues)))
# plot(as.numeric(days), counts,pch=16,ylim=c(0,20))
# lines(as.numeric(days), Counts.exponential2,lwd=2, col = "blue", xlab = "Time (s)", ylab = "Counts")

for (i in 1:nrow(timetofix)){
  if (timetofix$days[i]==0){
    timetofix$days[i]=1 + timetofix$days[i]
  }
}

#fitting distribution another way using fitdistr
require(fitdistrplus)

fit1 <- fitdistr(as.numeric(timetofix$days), "exponential") #fit exponential distribution
ks.test(as.numeric(timetofix$days), "pexp", fit1$estimate) #goodness of fit test


hist(timetofix$days, freq = FALSE, breaks = 100, xlim = c(0, quantile(timetofix$days, 0.99)))
curve(dexp(x, rate = fit1$estimate), col = "blue", add = TRUE)
1/fit1$estimate #number of days to fix a defect

# fit2 <- fitdistr(timetofix$days, "gamma") #fit gamma distribution
# ks.test(timetofix$days, "pgamma", fit2$estimate[1], fit2$estimate[2])#for gamma
# 
# fit<-fitdistr(timetofix$days,"log-normal")
# lines(dlnorm(0:max(timetofix$days),fit$estimate[1],fit$estimate[2]), lwd=3,col='red')
# ks.test(timetofix$days,"plnorm",fit$estimate[1],fit$estimate[2])



# #Let's try regression
# defectinfo=read.csv('Feb/NCSU Data Set 1 - Block 2016 February - Defect Info.csv')
# detach(freqdataF)
# defectinfo$Limitation.Type <- NULL
# colnames(defectinfo)=c('defectnum','DateOpened','Severity','PhaseFound','DeferCloned','Limitation')
# regressiondf=merge(timetofix,defectinfo,by='defectnum')
# regressiondf$DateOpened<-NULL
# regressiondf$DeferCloned<-NULL
# attach(regressiondf)
# #didn't need a design matrix
# designmatrix=model.matrix(~as.factor(Severity)+PhaseFound+Limitation,regressiondf)
# 
# regmodel<-lm(days~as.factor(Severity)+PhaseFound+Limitation)
# summary(regmodel) #none of these variables are statistically significant, and R^2 is too low
# detach(regressiondf)
# 

#Forming states, decisions, and costs
#States: Open, Working, Fixed, Verify, Rejected, Limitation, Closed
#Decision: Temporary Limitation, Permanent limitation, fix now





