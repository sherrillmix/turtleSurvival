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
pdt<-pdt[is.na(pdt$Partial),]
depthColumns<-colnames(pdt)[grep('^Depth',colnames(pdt))]
goodRows<-apply(pdt[,depthColumns],1,function(x)any(!is.na(x)))
pdt<-pdt[goodRows,]
pdt$maxDepth<-apply(pdt[,depthColumns],1,max,na.rm=TRUE)
pdt$minDepth<-apply(pdt[,depthColumns],1,min,na.rm=TRUE)
pdt$source<-'pdt'


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
minMaxDepth$min<-minMaxDepth$MinDepth
minMaxDepth$max<-minMaxDepth$MaxDepth
#these two seem to have 16 or 24 meter resolution so compress it all to 0
minMaxDepth[minMaxDepth$min<=24 & minMaxDepth$MinSource=='LightLoc','min']<-0
minMaxDepth[minMaxDepth$min<=24 & minMaxDepth$MinSource=='PDT','min']<-0
minMaxDepth[minMaxDepth$max<=16 &!is.na(minMaxDepth$max)&!is.na(minMaxDepth$MaxSource) & minMaxDepth$MaxSource=='LightLoc','max']<-0
surfaceDepth<-10
#minMaxDepth[minMaxDepth$min<surfaceDepth&!is.na(minMaxDepth$min),'min']<-0
minMaxDepth$surface<-minMaxDepth$min<surfaceDepth
minMaxDepth$noDive<-minMaxDepth$max<surfaceDepth
minMaxDepth<-minMaxDepth[order(minMaxDepth$Ptt,minMaxDepth$rDate),]
belowSurface<-by(minMaxDepth,minMaxDepth$Ptt,function(x){
  belows<-binary2range(!x$surface&x$deployDay> -10)
  if(nrow(belows)>0){
    belows<-do.call(rbind,apply(belows,1,function(y){
      #if 2 days of no data then split
      bigDiffs<-which(diff(x$deployDay[y[1]:y[2]])>2)
      out<-data.frame('start'=c(y[1],bigDiffs+y[1]),'end'=c(bigDiffs+y[1]-1,y[2]))
      return(out)
    }))
  }
  belows$start<-x[belows$start,'deployDay']
  belows$end<-x[belows$end,'deployDay']
  belows<-belows[belows$end-belows$start>2,]
  return(belows)
})
onSurface<-by(minMaxDepth,minMaxDepth$Ptt,function(x){
  noDives<-binary2range(x$noDive&x$deployDay> -10)
  if(nrow(noDives)>0){
    noDives<-do.call(rbind,apply(noDives,1,function(y){
      #if 2 days of no data then split
      bigDiffs<-which(diff(x$deployDay[y[1]:y[2]])>2)
      out<-data.frame('start'=c(y[1],bigDiffs+y[1]),'end'=c(bigDiffs+y[1]-1,y[2]))
      return(out)
    }))
  }
  noDives$start<-x[noDives$start,'deployDay']
  noDives$end<-x[noDives$end,'deployDay']
  noDives<-noDives[noDives$end-noDives$start>2,]
  return(noDives)
})


#only ARGOS reprocess with geo
locations<-readWild('processed/PAT Data-Locations.csv')

histos<-readWild('processed/PAT Data-Histos.csv')
divePdt<-readWild('processed/PAT Data-DivePDT.csv')
divePdt$maxDepth<-divePdt$Depth
divePdt$minDepth<-NA
divePdt$source<-'divePdt'


statusData$minDepth<-0
statusData$maxDepth<-NA
statusData$source<-'status'

depthCols<-c('rDate','minDepth','maxDepth','source')
allDepths<-rbind(divePdt[,depthCols],pdt[,depthCols],statusData[,depthCols])

