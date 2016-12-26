#Author: Annie Tran
#install.packages("splitstackshape")
#Combine block data to form transition matrix
require(fitdistrplus)

#gonna do some cleaning and forming transition matrix (and rate)

remove(list=ls())
setwd('/Users/isarasuntichotinun/Desktop/ANNIE/ISE 560/Project/Data')
feb=read.csv('Feb/NCSU Data Set 1 - Block 2016 February - State Changes.csv')

feb$X<-NULL
feb$X.1<-NULL
feb$X.2<-NULL

may=read.csv('May/NCSU Data Set 1 - Block 2016 May - State Changes.csv')

original=rbind(feb,may)
combinedMat=rbind(feb,may)

#Cleanning and combine-------------------------------------#
#defects that go from closed to open
#remove them and have them stop at closed
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-132,]
combinedMat=combinedMat[-131,]
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-423,]
combinedMat[422,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-514,]
combinedMat[513,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-532,]
combinedMat[531,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-879,]
combinedMat[878,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-907,]
combinedMat[906,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-996,]
combinedMat[995,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-1095,]
combinedMat[1094,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-1113,]
combinedMat[1112,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-1129,]
combinedMat[1128,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-1503,]
combinedMat=combinedMat[-1502,]
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
combinedMat=combinedMat[-2041,]
combinedMat[2040,3]='Open'
rownames(combinedMat) <- 1:nrow(combinedMat)
dataerror=combinedMat[which(combinedMat$Changed.From=='Closed'),]
rm(dataerror)


#combine defect info df's:
defectinfoFeb=read.csv('Feb/NCSU Data Set 1 - Block 2016 February - Defect Info.csv')
defectinfoMay=read.csv('May/NCSU Data Set 1 - Block 2016 May - Defect Info.csv')
defectinfo=rbind(defectinfoFeb,defectinfoMay)

#df=data.frame(Defect.Number= numeric(0), Date.Opened= character(0), Severity = numeric(0),Phase.Found=character(0),is.Defer.Clone=character(0),Is.Limitation=character(0),Limitation.Type=character(0))
df=defectinfo

#creating a count vector to keep count of how many times to repeat a defect number
count=0
countvec=data.frame(Count=numeric(0))
j=1
val=defectinfo[j,1]

for (i in 1:nrow(combinedMat)){
  if (combinedMat[i,1]==val){
    count=count+1
  }else{
    if (i==2053){
      count=count+1
      countvec[j,1]=count
      break
    }
    else{
    countvec[j,1]=count
    count=1
    j=j+1
    val=defectinfo[j,1]
    
    }
  }
}

countvec[300,]=11
countnum=as.numeric(unlist(countvec))
#row.names=NULL is to so it doesn't throw the duplicate row names error
test=data.frame(apply(df, 2, function(c) rep(c,countnum)),row.names = NULL)

#dataframe that includes state changes and defect info like phase found and severity level for each defect
combinedMat=cbind(combinedMat,test)
combinedMat[,5]<-NULL

#remove transitions from open to open
combinedMat=combinedMat[!(combinedMat$Changed.From == 'Open' & combinedMat$Changed.To =='Open'),]
rownames(combinedMat) <- 1:nrow(combinedMat)

#remove transitions from rejected to rejected
combinedMat=combinedMat[!(combinedMat$Changed.From == 'Rejected' & combinedMat$Changed.To =='Rejected'),]
rownames(combinedMat) <- 1:nrow(combinedMat)

backup=combinedMat
# #Define some new states:
#   #If defect ends at limitation --> it's approved limitation (absorbing)
#   #If defect moves from limitation to other states --> denied limitation
# levels(combinedMat$Changed.To) = c(levels(combinedMat$Changed.To),'Approved Limitation','Denied Limitation')
# levels(combinedMat$Changed.From) = c(levels(combinedMat$Changed.From),'Denied Limitation')
# for (i in 1:nrow(combinedMat)){
#   if (i != 2050){
#     if (combinedMat$Defect.Number[i]!=combinedMat$Defect.Number[i+1]){
#       if (combinedMat$Changed.To[i]=='Limitation'){
#         combinedMat$Changed.To[i]='Approved Limitation'}
#     }
# 
#   }
# }
# 
# for (i in 1:nrow(combinedMat)){
#   if (i!=2050)
#   {
#     if (combinedMat$Defect.Number[i]==combinedMat$Defect.Number[i+1]){
#       if (combinedMat$Changed.To[i]=='Limitation' &  combinedMat$Changed.From[i+1]=='Limitation'){
#         combinedMat$Changed.To[i]='Denied Limitation'
#       }
#     }
#   }
# }
# 
# for (i in 1:nrow(combinedMat)){
#   if (i!=2050)
#     if (combinedMat$Changed.To[i]=='Denied Limitation'){
#       combinedMat$Changed.From[i+1]='Denied Limitation'
#     }
# }
# 
# backup=combinedMat
# combinedMat$Changed.To[2050]='Approved Limitation'
# combinedMat$Changed.To=factor(combinedMat$Changed.To)
# combinedMat$Changed.From=factor(combinedMat$Changed.From)

#Open to Limitation is not supposed to happen
#So remove those
#vector of defects that go from open to close:
v=combinedMat$Defect.Number[combinedMat$Changed.From=="Open" & combinedMat$Changed.To=="Limitation"]

#remove them
combinedMat$Changed.From[52]='Open'
combinedMat=combinedMat[-51,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat$Changed.To[199]='Closed'
combinedMat=combinedMat[-524,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat$Changed.To[649]='Working'
combinedMat$Changed.From[651]='Working'
combinedMat=combinedMat[-650,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat$Changed.To[797]='Closed'
combinedMat$Changed.From[824]='Open'
combinedMat=combinedMat[-823,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat$Changed.From[877]='Open'
combinedMat=combinedMat[-876,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat$Changed.From[967]='Open'
combinedMat=combinedMat[-966,]
rownames(combinedMat) <- 1:nrow(combinedMat)
newrow=combinedMat[1095,]
combinedMat=rbind(combinedMat[1:1094,],newrow,combinedMat[1095:2043,])
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat$Changed.To[1095]='Working'
combinedMat$Changed.From[1096]='Working'
combinedMat$Changed.To[1357]='Closed'
combinedMat$Changed.From[1391]='Working'
combinedMat=combinedMat[-1390,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat$Changed.To[1542]='Working'
combinedMat=combinedMat[-1543,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat$Changed.To[1677]='Closed'
combinedMat$Changed.To[1721]='Closed'
combinedMat$Changed.To[1779]='Closed'
combinedMat$Changed.From[1923]='Open'
combinedMat=combinedMat[-1922,]
rownames(combinedMat) <- 1:nrow(combinedMat)

#Remove double listed states
combinedMat=combinedMat[-31,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat=combinedMat[-39,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat=combinedMat[-41,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat=combinedMat[-43,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat=combinedMat[-46,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat=combinedMat[-57,]
rownames(combinedMat) <- 1:nrow(combinedMat)
combinedMat=combinedMat[-77,]
rownames(combinedMat) <- 1:nrow(combinedMat)

#Working to close is not supposed to happen
combinedMat$Changed.From[746]='Working'
combinedMat=combinedMat[-745,]
rownames(combinedMat) <- 1:nrow(combinedMat)

#Remove Rejected to Limitation
combinedMat$Changed.From[1739]='Working'
combinedMat=combinedMat[-1738,]
rownames(combinedMat) <- 1:nrow(combinedMat)

combinedMat$Changed.To[487]='Open'
combinedMat$Changed.From[488]='Open'

febclean = combinedMat[1:855,]
rownames(febclean) <- 1:nrow(febclean)
mayclean = combinedMat[856:2034,]
rownames(mayclean) <- 1:nrow(mayclean)

write.csv(febclean,file="Clean Combined Feb.csv",row.names = F)
write.csv(mayclean,file="Clean Combined May.csv",row.names = F)

combinedMat=rbind(febclean,mayclean)
write.csv(combinedMat,file="Combined Defect Data.csv")



#End Cleanning and combine-------------------------------------#


# #transitions from state "open"
# openStates= combinedMat[which(combinedMat$Changed.From=='Open'),]
# opentab = table(openStates$Changed.To)
# opentabDis = opentab/nrow(openStates)
# barplot(opentab,main="Counts")
# 
# #transitions from state "working"
# workingStates= combinedMat[which(combinedMat$Changed.From=='Working'),]
# workingtab = table(workingStates$Changed.To)
# workingtabDis = workingtab/nrow(workingStates)
# barplot(workingtab,main="Counts")
# 
# #transitions from state "verify"
# verifyStates= combinedMat[which(combinedMat$Changed.From=='Verify'),]
# verifytab = table(verifyStates$Changed.To)
# verifytabDis = verifytab/nrow(verifyStates)
# barplot(verifytab,main="Counts")
# 
# #transitions from state "fixed"
# FixedStates= combinedMat[which(combinedMat$Changed.From=='Fixed'),]
# Fixedtab = table(FixedStates$Changed.To)
# FixedtabDis = Fixedtab/nrow(FixedStates)
# barplot(Fixedtab,main="Counts")
# 
# #transitions from state "Rejected"
# RejectedStates= combinedMat[which(combinedMat$Changed.From=='Rejected'),]
# Rejectedtab = table(RejectedStates$Changed.To)
# RejectedtabDis = Rejectedtab/nrow(RejectedStates)
# barplot(Rejectedtab,main="Counts")
# 
# #transitions from state "Limitation"
# LimitationStates= combinedMat[which(combinedMat$Changed.From=='Limitation'),]
# Limitationtab = table(LimitationStates$Changed.To)
# LimitationtabDis = Limitationtab/nrow(LimitationStates)
# barplot(Limitationtab,main="Counts")

#Create transition matrix-----------------
dimNames=c(levels(combinedMat$Changed.To)) #store all the states
transMat=matrix( rep( 0, len=64), nrow = 8,dimnames = list(dimNames))
colnames(transMat)=dimNames

#this loops is to fill each entry of the transition matrix with frequency distritbution from the combined data
#it loops through each state and create a frequency table for each state
for (i in 1:length(dimNames)){
  statesto=combinedMat[which(combinedMat$Changed.From==dimNames[i]),]
  freqtable=table(statesto$Changed.To)
  freqdis=freqtable/nrow(statesto)
  for (j in 1:length(dimNames)){
    transMat[i,j]=freqdis[j]
  }
}
transMat[1,]=c(1,0,0,0,0,0,0,0) #closed is supposed to be absorbing
transMat[7,]=c(0,0,0,0,0,0,1,0)
transMat
#End Create transition matrix-----------------



#Create rates vector---------------------------
dimNames=levels(combinedMat$Changed.To)
#Extract time spent in each state and hopefully it's exponential
#store rates in vector (defect in this state per day)
rates=matrix( rep( 0, len=49), nrow = 7,ncol=7)
colnames(rates)=dimNames
rownames(rates)=dimNames

#Time spent in open before going to working
days=data.frame(defectnum=numeric(0),daysinstate=numeric(0))
k=1
stateto = 'Working'
statefrom = 'Fixed'
for (i in 1:nrow(combinedMat)){
  if (i!=1741 & combinedMat$Changed.To[i]==stateto & combinedMat$Changed.To[i+1]==statefrom){
    days[k,]=c(combinedMat$Defect.Number[i],as.integer(as.Date(combinedMat$Timestamp[i+1])-as.Date(combinedMat$Timestamp[i])))
    k = k+1
  }
    
}

freqtab = table(as.numeric(days$daysinstate))
plot(freqtab,main='Working to Fixed',xlab='days')
freqdataF = as.data.frame(freqtab)
# plot(freqtab)
# fit <- fitdistr(days$daysinstate, "exponential")
# ks.test(days$daysinstate, "pexp", fit$estimate) #goodness of fit test

colnames(freqdataF)=c('days','counts')
expofit <- lm(log(as.numeric(freqdataF$counts))~ as.numeric(freqdataF$days))
expofit$coefficients[2]
summary(expofit) 

#This is the qij matrix (transition rate matrix)
rates[stateto,statefrom]<- -expofit$coefficients[2]
rates['Closed','Closed']<-1
for (i in 1:nrow(rates)){
  rates[i,i]= -sum(rates[i,])
}

#Transition Matrix
pij=matrix( rep( 0, len=49), nrow = 7,ncol=7)
for (i in 2:nrow(rates)){
  rowsum = -rates[i,i]
  for (j in 1:nrow(rates)){
    pij[i,j] = rates[i,j]/rowsum
  }
}

#set diagonal to 0
for (i in 2:nrow(pij)){
  pij[i,i]= 0
}
colnames(pij)=dimNames
rownames(pij)=dimNames

pij['Closed','Closed'] = 1
#rough estimate of fraction of times limitation goes to working
sum(combinedMat$Changed.To=='Limitation')
sum(combinedMat$Changed.From=='Limitation' & combinedMat$Changed.To=='Working')
pij['Limitation','Working']=10/44
pij['Limitation','Limitation']=34/44

write.csv(rates,file="RateMatrix.csv")
write.csv(pij,file="Transition Matrix.csv")



#Time spent in each state (exponential with rate qi)
timeinstate=data.frame(defectnum=numeric(0),daysinstate=numeric(0))
k=1
for (i in 1:nrow(combinedMat)){
  if (combinedMat[i,'Changed.To']=='Fixed'){
    timeinstate[k,]=c(combinedMat$Defect.Number[i],as.integer(as.Date(combinedMat$Timestamp[i+1])-as.Date(combinedMat$Timestamp[i])))
    k = k+1
    }
}


freqtab = table(as.numeric(timeinstate$daysinstate))
plot(freqtab)
fit <- fitdistr(timeinstate$daysinstate, "exponential")
ks.test(timeinstate$daysinstate, "pexp", fit$estimate) #goodness of fit test
hist(timeinstate$daysinstate, freq = FALSE, breaks = 200, xlim = c(0, quantile(timeinstate$daysinstate, 0.99)))
curve(dexp(x, rate = fit$estimate), col = "red", add = TRUE)

rates[1,"Open"]=fit$estimate #qi
rates #(lambda for each state. Time in each state is exponential)
#End create rates vector---------------------------


#Transition rate matrix 
transrateMat=matrix( rep( 0, len=64), nrow = 8,dimnames = list(dimNames))
for (i in 1:nrow(transMat)){
  for (j in 1:ncol(transMat)){
    transrateMat[i,j]= transMat[i,j]*rates[i] #pij * qi = qij
  }
}
colnames(transrateMat)=dimNames

#Fill diagonals with negative entries because the rows 
#are supposed to add to 1
for (i in 1:nrow(transrateMat)){
  transrateMat[i,i]= -sum(transrateMat[i,])
}

transrateMat[1,1]=1
transrateMat[7,7]=1
#End Transition rate matrix------------------------

#Export combined matrix, rate vector
#transition matrix, transition rate matrix
write.csv(combinedMat,file="Combined Defect Data.csv")
write.csv(rates,file="RateVector.csv")
write.csv(transMat,file="Transition Matrix.csv")
write.csv(transrateMat,file="Transition Rate Matrix.csv")


#A different way to form transition matrix
combinedData=read.csv('Combined Defect Data.csv')
combinedData$Changed.To[867]='Open' #should not go from working to verify
combinedData=combinedData[-868,]
rownames(combinedData) <- 1:nrow(combinedData)

statefreq=matrix(0,nrow=7,ncol=7)
states=levels(combinedData$Changed.To)
for (i in 1:length(states)){
  statedata= combinedData[which(combinedData$Changed.From==states[i]),]
  tab = table(statedata$Changed.To)
  statefreq[i,] = tab
  }
colnames(statefreq)=states
rownames(statefreq)=states
statefreq

#number of days in this block release:
totaldays = as.integer(max(as.Date(combinedData$Timestamp))-min(as.Date(combinedData$Timestamp)))+1
transrateMat = statefreq/totaldays
transitionMat = matrix(0,nrow=7,ncol=7)
for (i in 2:length(states)){
  transitionMat[i,] = transrateMat[i,]/sum(transrateMat[i,])
}
for (i in 1:length(states)){
  transrateMat[i,i] = -sum(transrateMat[i,])
}

colnames(transitionMat)=states
rownames(transitionMat)=states
transitionMat[1,]=c(1,0,0,0,0,0,0)
transitionMat



