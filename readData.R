library('lubridate')

info<-read.csv('Caretta_Hooked_Table_2011-2015.csv',stringsAsFactors=FALSE)
info$deployDate<-parse_date_time(info$Deployment.Date..yyyy.mm.dd.,'mdy')
deployDates<-info$deployDate
names(deployDates)<-info$PTTID

readWild<-function(x,...){
  tagData<-readLines(x)
  tagData<-tagData[tagData!=''&!grepl('^;',tagData)]
  tagData<-strsplit(tagData,',')
  #assuming ; lines are comments
  nCol<-length(tagData[[1]])
  tagData<-sapply(tagData,function(x)paste(x[1:nCol],collapse=','))
  tagData<-read.csv(textConnection(tagData),stringsAsFactors=FALSE,...)
  if(any(colnames(tagData)=='Date'))tagData$rDate<-parse_date_time(tagData$Date,'%H:%M:%S %d-%b-%y')
  if(any(colnames(tagData)=='Received'))tagData$rDate<-parse_date_time(tagData$Received,'%H:%M:%S %d-%b-%y')
  return(tagData)
}
calcDeployDay<-function(animals,rdate,deployDates){
  (as.numeric(rdate)-as.numeric(deployDates[as.character(animals)]))/24/60/60
}

tagData<-readWild('processed/PAT Data-Summary.csv')
statusData<-readWild('processed/PAT Data-Status.csv')
statusData$rReleaseTime<-parse_date_time(statusData$ReleaseTime,'%H:%M:%S %d-%b-%y')
releaseDates<-unlist(by(statusData,statusData$Ptt,function(x){
  tab<-sort(table(x$ReleaseTime[!is.na(x$rReleaseTime)]))
  if(length(tab)==0)return(NA)
  if(tail(tab,1)/sum(tab)<.7)warning(sprintf('Ambiguous release time in PTT %s',x[1,'Ptt']))
  return(tail(names(tab),1))
}))
pdt<-readWild('processed/PAT Data-PDTs.csv')
pdt<-pdt[pdt$BrokenThermistor!=0&is.na(pdt$Partial),]

info$releaseDate<-parse_date_time(sapply(info$PTTID,function(x)releaseDates[as.character(x)]),'%H:%M:%S %d-%b-%y')
info$releaseDays<-(as.numeric(info$releaseDate)-as.numeric(info$deployDate))/24/60/60

minMaxDepth<-readWild('processed/PAT Data-MinMaxDepth.csv')

if(any(!info$PTTID %in% tagData$DeployID))warning('Missing tag ',paste(info[!info$PTTID %in% tagData$DeployID,'PTTID'],collapse=''))
missingInfo<-tagData[!tagData$DeployID %in% info$PTTID,]
write.csv(missingInfo[,c('Ptt','Instr','ReleaseType')],'out/missingTags.csv',row.names=FALSE)
table(tagData$ReleaseType)

tagData<-tagData[tagData$DeployID %in% info$PTTID,]
statusData<-statusData[statusData$DeployID %in% info$PTTID,]
minMaxDepth<-minMaxDepth[minMaxDepth$DeployID %in% info$PTTID,]

statusData$deployDay<-calcDeployDay(statusData$Ptt,statusData$rDate,deployDates)
minMaxDepth$deployDay<-calcDeployDay(minMaxDepth$Ptt,minMaxDepth$rDate,deployDates)

#only ARGOS reprocess with geo
locations<-readWild('processed/PAT Data-Locations.csv')

histos<-readWild('processed/PAT Data-Histos.csv')
divePdt<-readWild('processed/PAT Data-DivePDT.csv')

