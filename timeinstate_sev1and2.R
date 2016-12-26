#transition matrix for sev 1 and 2

remove(list=ls())
setwd('/Users/isarasuntichotinun/Desktop/ANNIE/ISE 560/Project/Data')
require(fitdistrplus)

data=read.csv('Combined Defect Data.csv')
data=data[data$Severity %in% c(1,2),]
data$X<-NULL

#Create rates matrix---------------------------
dimNames=levels(data$Changed.To)
dimNames=dimNames[dimNames!='Limitation']
#Extract time spent in each state and hopefully it's exponential
#store rates in vector (defect in this state per day)
rates=matrix( rep( 0, len=36), nrow = 6,ncol=6)
colnames(rates)=dimNames
rownames(rates)=dimNames



days=data.frame(defectnum=numeric(0),daysinstate=numeric(0))
k=1
stateto = 'Working'
statefrom = 'Rejected'
for (i in 1:nrow(data)){
  if (i!=292 & data$Changed.To[i]==stateto & data$Changed.To[i+1]==statefrom){
    days[k,]=c(data$Defect.Number[i],as.integer(as.Date(data$Timestamp[i+1])-as.Date(data$Timestamp[i])))
    k = k+1
  }
}

freqtab = table(as.numeric(days$daysinstate))
plot(freqtab)
fit <- fitdistr(days$daysinstate, "exponential")
ks.test(days$daysinstate, "pexp", fit$estimate) #goodness of fit test

#This is the qij matrix (transition rate matrix)
rates[stateto,statefrom]<-fit$estimate


#if not enough data, estimate with table
openStatesFeb = data[which(data$Changed.From=='Working'),]
opentabFeb = table(openStatesFeb$Changed.To)
opentabFeb
opentabFeb/nrow(openStatesFeb)

rates[stateto,statefrom]<-6/261


rates = read.csv('sev1&2RateMatrix.csv')
rates=as.matrix(rates)
rates$X <- NULL
rownames(rates)<-dimNames

#transition matrix
rates['Closed','Closed']<-1
for (i in 1:nrow(rates)){
  rates[i,i]= -sum(rates[i,])
}

pij_sev1and2=matrix( rep( 0, len=36), nrow = 6,ncol=6)
for (i in 2:nrow(rates)){
  rowsum = -rates[i,i]
  for (j in 1:nrow(rates)){
    pij_sev1and2[i,j] = rates[i,j]/rowsum
  }
}

#set diagonal to 0
for (i in 2:nrow(pij_sev1and2)){
  pij_sev1and2[i,i]= 0
}
colnames(pij_sev1and2)=dimNames
rownames(pij_sev1and2)=dimNames

rates[1,1] = 0
fMat =  rates[2:6,2:6]
fMat = -solve(fMat)

daysuntilclosed = matrix(rep(0,len=5),nrow=5,ncol=1)
rownames(daysuntilclosed)<-colnames(fMat)
colnames(daysuntilclosed)='Closed'

rates[1,1] = 0
fMat =  rates[2:6,2:6]
fMat = -solve(fMat)

daysuntilclosed = matrix(rep(0,len=5),nrow=5,ncol=1)
rownames(daysuntilclosed)<-colnames(fMat)
colnames(daysuntilclosed)='Closed'
for(i in 1:nrow(fMat)){
  daysuntilclosed[i,1]=sum(fMat[i,])
}

write.csv(rates,file="sev1&2RateMatrix.csv")
write.csv(pij_sev1and2,file="sev1&2TransMatrix.csv")
write.csv(fMat,file="sev1&2absorbtionMat.csv")

