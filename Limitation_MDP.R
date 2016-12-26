#MDP for Limitation

remove(list=ls())
setwd('/Users/isarasuntichotinun/Desktop/ANNIE/ISE 560/Project/Data')

transitionMat = read.csv('Transition Matrix.csv')
transitionMat$X<-NULL
transitionMat=as.matrix(transitionMat)
rownames(transitionMat)<-colnames(transitionMat)

transitionrate = read.csv('RateMatrix.csv')
transitionrate$X<-NULL
transitionrate=as.matrix(transitionrate)
rownames(transitionrate)<-colnames(transitionrate)

#---------------Might be wrong------
qMat = transitionMat[2:7,2:7]
qMat = as.matrix(qMat)
identityMat = diag(6)
R = transitionMat[2:7,1]
R = as.matrix(R)
colnames(R) = 'Closed'

IQInv = solve(identityMat-qMat)

for(i in 1:nrow(IQInv)){
  print(sum(IQInv[i,]))
}
#-----------------------------------

#This looks better
transitionrate[1,1] = 0
fMat =  transitionrate[2:7,2:7]
fMat = -solve(fMat)

daysuntilclosed = matrix(rep(0,len=6),nrow=6,ncol=1)
rownames(daysuntilclosed)<-colnames(fMat)
colnames(daysuntilclosed)='Closed'
for(i in 1:nrow(fMat)){
  daysuntilclosed[i,1]=sum(fMat[i,])
}

#calculate (I-Q)^-1*R
R = as.matrix(pij[2:7,1])
fMat%*% R

#estimated mean time to 'Closed' for a defect in state
#'Fixed' = 14 days
#'Limitation' = 24 days
#'Open' = 15 days
#'Rejected' = 11 days
#'Verify' = 11 days
#'Working' = 16 days

#Decision for state 'Limitation': Do nothing, or mark as limitation
#If do nothing then defect will go back to state working
pDN = transitionMat
pDN['Limitation',]=pDN['Working',]
PMarked = transitionMat
PMarked['Limitation',]=PMarked['Open',]

write.csv(pDN,file="doNothinginLimMat.csv")
write.csv(PMarked,file="markedasLimMat.csv")
write.csv(fMat,file="absorbtionMat.csv")


#Supposed DN is the optimal policy
#for phase dev:
avec = c(-1,.48402,.29286,.12312,0,0,0,0.6714,-1,0.27162,0.45621,0,0.10512,0,
         .52119,0,-1,0,0,0,.37881,0,.05778,0,0,.84222,0,0,.47691,0,0,0,-1,0,0.42309,
         0.06714,0,0.27162,.45621,0,-.89488,0,0,0,0,0,0,0,.1)
a = matrix(avec,nrow=7,ncol=7)
a=t(a)
bvec=c(20315,21659,14939,18971,14939,24347,0)
b = matrix(bvec, nrow=7,ncol=1)

solve(a,b)


