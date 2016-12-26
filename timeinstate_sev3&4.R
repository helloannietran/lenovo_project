#transition matrix for sev 1 and 2

remove(list=ls())
setwd('/Users/isarasuntichotinun/Desktop/ANNIE/ISE 560/Project/Data')
require(fitdistrplus)

data=read.csv('Combined Defect Data.csv')
data=data[data$Severity %in% c(3,4),]
data$X<-NULL

#Limitation doesn't go to closed
data$Changed.From[427] = 'Working'
data=data[-426,]
rownames(data) <- 1:nrow(data)


#Create rates matrix---------------------------
dimNames=levels(data$Changed.To)
#Extract time spent in each state and hopefully it's exponential
#store rates in vector (defect in this state per day)
rates=matrix( rep( 0, len=49), nrow = 7,ncol=7)
colnames(rates)=dimNames
rownames(rates)=dimNames



days=data.frame(defectnum=numeric(0),daysinstate=numeric(0))

k=1
stateto = 'Open'
statefrom = 'Rejected'
for (i in 1:nrow(data)){
  if (i!=1740 & data$Changed.To[i]==stateto & data$Changed.To[i+1]==statefrom){
    days[k,]=c(data$Defect.Number[i],as.integer(as.Date(data$Timestamp[i+1])-as.Date(data$Timestamp[i])))
    k = k+1
  }
}

freqtab = table(as.numeric(days$daysinstate))
plot(freqtab)

freqdataF = as.data.frame(freqtab)
colnames(freqdataF)=c('days','counts')
attach(freqdataF)
expofit <- lm(log(freqdataF$counts)~ as.numeric(freqdataF$days))
expofit$coefficients
summary(expofit) #R^2 = 0.66
detach(freqdataF)

fit <- fitdistr(days$daysinstate, "exponential")
ks.test(days$daysinstate, "pexp", fit$estimate) #goodness of fit test

#This is the qij matrix (transition rate matrix)
rates[stateto,statefrom]<-fit$estimate


#if not enough data, estimate with table
openStatesFeb = data[which(data$Changed.From=='Limitation'),]
opentabFeb = table(openStatesFeb$Changed.To)
opentabFeb
opentabFeb/nrow(openStatesFeb)

rates[stateto,statefrom]<-10/261

rates = read.csv('sev3&4RateMatrix.csv')
rates=as.matrix(rates)
rates$X <- NULL
rownames(rates)<-dimNames

#transition matrix
rates['Closed','Closed']<-1
for (i in 1:nrow(rates)){
  rates[i,i]= -sum(rates[i,])
}

pij_sev3and4=matrix( rep( 0, len=49), nrow = 7,ncol=7)
for (i in 2:nrow(rates)){
  rowsum = -rates[i,i]
  for (j in 1:nrow(rates)){
    pij_sev3and4[i,j] = rates[i,j]/rowsum
  }
}

#set diagonal to 0
for (i in 2:nrow(pij_sev3and4)){
  pij_sev3and4[i,i]= 0
}
colnames(pij_sev3and4)=dimNames
rownames(pij_sev3and4)=dimNames

rates[1,1] = 0
fMat =  rates[2:7,2:7]
fMat = -solve(fMat)

daysuntilclosed = matrix(rep(0,len=6),nrow=6,ncol=1)
rownames(daysuntilclosed)<-colnames(fMat)
colnames(daysuntilclosed)='Closed'

rates[1,1] = 0
fMat =  rates[2:7,2:7]
fMat = -solve(fMat)

daysuntilclosed = matrix(rep(0,len=6),nrow=6,ncol=1)
rownames(daysuntilclosed)<-colnames(fMat)
colnames(daysuntilclosed)='Closed'
for(i in 1:nrow(fMat)){
  daysuntilclosed[i,1]=sum(fMat[i,])
}

write.csv(rates,file="sev3&4RateMatrix.csv")
write.csv(pij_sev3and4,file="sev3&4TransMatrix.csv")
write.csv(fMat,file="sev3&4absorbtionMat.csv")


#------------side stuff--------------

febdata=read.csv('Clean Combined Feb.csv')
maydat=read.csv('Clean Combined May.csv')
length(unique(febdata$Defect.Number))
length(unique(maydat$Defect.Number))
alldata=read.csv('Combined Defect Data.csv')
fixeddates = maydat[maydat$Changed.From=='Verify' & maydat$Changed.To=='Closed',]
fixeddates$X<-NULL
write.csv(fixeddates,file="fixeddata_may.csv")

uniquesev=unique(alldata[,'Defect.Number'])

phases = matrix(rep('SDV',300),nrow=300,ncol=1)
for (i in 1:length(uniquesev)){
  phases[i]=alldata[alldata$Defect.Number==uniquesev[i],]$Phase.Found[1]
}
#4 = SDV, DEV=1, SIT=5, PostSS=3, Mfg=2
mat = matrix(c(uniquesev,phases),nrow=300,ncol=2)
table(phases)/300
#Dev = 0.15
#Post = 0.1
#SDV=0.66
#SIT = 0.09
#Mfg = 0.00333