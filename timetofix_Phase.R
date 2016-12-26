#time to fix a defect per phase
#Using the combined matrix of both block releases
require(fitdistrplus)

#Phase Dev
Phase=combinedMat[combinedMat$Phase.Found %in% c('SIT','Mfg','PostSS'),]

countdata=data.frame(unique(Phase$Defect.Number))
colnames(countdata)='count'
j=1
for (i in 1:length(countdata$count)){
  while (countdata$count[i]==Phase$Defect.Number[j] 
         & is.na(Phase$Defect.Number[j])==FALSE){
    j=j+1
  }
  countdata$count[i] = j-1
}

countdata$countbegin=unique(Phase$Defect.Number)

for (i in 1:length(countdata$count)){  
  if (i ==1){
    countdata$countbegin[i] = 1
  }else{
    countdata$countbegin[i] = countdata$count[i-1]+1
  }
}

timetofix = data.frame(unique(Phase$Defect.Number))
colnames(timetofix) = 'defectnum'
timetofix$days = unique(Phase$Defect.Number)
for (i in 1:length(countdata$count)){
  start=countdata$countbegin[i]
  end = countdata$count[i]
  timetofix$days[i]=as.integer(as.Date(Phase$Timestamp[end])-as.Date(Phase$Timestamp[start]))
}

freqtable = table(as.numeric(timetofix$days)) #doesn't look exponential
plot(freqtable)

#bad fit for Dev, pval = 0.016 for SDV, pval = 0.185 for SIT, 0.1506 for Mfg and PostSS
#Dev and SDV combined: pval = 0.01772
#SIT, Mfg, and PostSS combined: pval = 0.08038, significant if alpha = 0.1
fit <- fitdistr(timetofix$days, "exponential") #fit exponential distribution
ks.test(timetofix$days, "pexp", fit$estimate)
hist(timetofix$days, freq = FALSE, breaks = 100, xlim = c(0, quantile(timetofix$days, 0.99)))
curve(dexp(x, rate = fit$estimate), col = "red", add = TRUE)
1/fit$estimate
#24.5 days/defect for SDV
#42.4 days/defect for SIT
#55.3 days/defect fot Mfg and PostSS
#Dev and SDV combined: 25.68 days per defect
#SIT, Mfg, and PostSS combined: 49.23 days per defect

############################################
#Try regression with just phase
countdata=data.frame(unique(combinedMat$Defect.Number))
colnames(countdata)='count'
j=1
for (i in 1:length(countdata$count)){
  while (countdata$count[i]==combinedMat$Defect.Number[j] 
         & is.na(combinedMat$Defect.Number[j])==FALSE){
    j=j+1
  }
  countdata$count[i] = j-1
}

countdata$countbegin=unique(combinedMat$Defect.Number)

for (i in 1:length(countdata$count)){  
  if (i ==1){
    countdata$countbegin[i] = 1
  }else{
    countdata$countbegin[i] = countdata$count[i-1]+1
  }
}

timetofix = data.frame(unique(combinedMat$Defect.Number))
colnames(timetofix) = 'defectnum'
timetofix$days = unique(combinedMat$Defect.Number)
for (i in 1:length(countdata$count)){
  start=countdata$countbegin[i]
  end = countdata$count[i]
  timetofix$days[i]=as.integer(as.Date(combinedMat$Timestamp[end])-as.Date(combinedMat$Timestamp[start]))
}

phasedata=read.csv('Feb/NCSU Data Set 1 - Block 2016 February - Defect Info.csv')
phasedata2=read.csv('May/NCSU Data Set 1 - Block 2016 May - Defect Info.csv')
phasedata=rbind(phasedata,phasedata2)
phasedata=phasedata$Phase.Found
timetofix$phase=phasedata

attach(timetofix)
regmodel = lm(days~as.factor(phase))
summary(regmodel) #bad fit
detach(timetofix)
#regression doesn't work either.....

#Maybe workload and time to fix are correlated
volume=read.csv('Feb/NCSU Data Set 2 - Block 2016 February - Volume Trend.csv')
volume2=volume=read.csv('May/NCSU Data Set 2 - Block 2016 May - Volume Trend.csv')
volume = rbind(volume,volume2)

#store the total number of active defects during the time of fixing a defect
as.integer(as.Date(volume$Week.Of[1], format = "%m/%d/%Y"))




