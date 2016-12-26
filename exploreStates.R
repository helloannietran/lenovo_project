
getwd()
setwd('/Users/isarasuntichotinun/Desktop/ANNIE/ISE 560/Project/Data')
stateChanges = read.csv('Feb/NCSU Data Set 1 - Block 2016 February - State Changes.csv')
stateChanges2 = read.csv('May/NCSU Data Set 1 - Block 2016 May - State Changes.csv')

stateChanges$X<-NULL
stateChanges$X.1<-NULL
stateChanges$X.2<-NULL
stateChanges=combinedMat
#For Feb
#Open State
openStatesFeb = stateChanges[which(stateChanges$Changed.From=='Open'),]
opentabFeb = table(openStatesFeb$Changed.To)
opentabFebDis = opentabFeb/nrow(openStatesFeb)
barplot(opentabFeb,main="Counts")

levels(openStatesFeb$Changed.To)


#Closed State
closedStatesFeb = stateChanges[which(stateChanges$Changed.From=='Closed'),]
closedtabFeb = table(closedStatesFeb$Changed.To)
closedtabFebDis = closedtabFeb/nrow(closedStatesFeb)
barplot(closedtabFeb,main="Counts")

#Fixed State
fixedStatesFeb = stateChanges[which(stateChanges$Changed.From=='Fixed'),]
fixedtabFeb = table(fixedStatesFeb$Changed.To)
fixedtabFebDis = fixedtabFeb/nrow(fixedStatesFeb)
barplot(fixedtabFeb,main="Counts")

#Limitation
limStatesFeb = stateChanges[which(stateChanges$Changed.From=='Limitation'),]
limtabFeb = table(limStatesFeb$Changed.To)
limtabFebDis = limtabFeb/nrow(limStatesFeb)
barplot(limtabFeb,main="Counts")

#Rejected
rejectedStatesFeb = stateChanges[which(stateChanges$Changed.From=='Rejected'),]
rejtabFeb = table(rejectedStatesFeb$Changed.To)
rejtabFebDis = rejtabFeb/nrow(rejectedStatesFeb)
barplot(limtabFeb,main="Counts")

#For May
#Open State
openStatesMay = stateChanges2[which(stateChanges2$Changed.From=='Open'),]
opentabMay = table(openStatesMay$Changed.To)
opentabMayDis = opentabMay/nrow(openStatesMay)
barplot(tabMay,main="Counts")

#Closed State
closedStatesMay = stateChanges2[which(stateChanges2$Changed.From=='Closed'),]
closedtabMay = table(closedStatesMay$Changed.To)
closedtabMayDis = closedtabMay/nrow(closedStatesMay)
barplot(closedtabMay,main="Counts")




