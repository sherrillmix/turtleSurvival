library('lubridate')
source('~/scripts/R/dna.R')

info<-read.csv('Caretta_Hooked_Table_2011-2015.csv',stringsAsFactors=FALSE)
info$deployDate<-parse_date_time(info$Deployment.Date..yyyy.mm.dd.,'mdy')
info$ptt<-as.character(info$PTTID)
info[info$CCL.notch.to.tip=='N/A','CCL.notch.to.tip']<-NA
info$CCL.notch.to.tip<-as.numeric(info$CCL.notch.to.tip)
rownames(info)<-info$ptt
deployDates<-info$deployDate
names(deployDates)<-info$ptt

monoFish<-read.csv('Loggerhead_observer_type_March30_2016.csv',stringsAsFactors=FALSE)
rownames(monoFish)<-monoFish$Tag.Serial....Pat.Splash.ID.
info$observer<-monoFish[info$ID,'Observer.Fisherman']
info$observer[info$observer=='Mike James']<-'Observer'
info$mono<-monoFish[info$ID,'Length.of.Mono.Remaining.cm.']


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

tagData<-readWild(list.files('processed','.*-Summary.csv',full.names=TRUE))
statusData<-readWild(list.files('processed','.*-Status.csv',full.names=TRUE))
statusData$rReleaseTime<-parse_date_time(statusData$ReleaseTime,'%H:%M:%S %d-%b-%y')
#remove status data with crazy release dates prior to deployment
statusData<-statusData[statusData$rReleaseTime > deployDates[as.character(statusData$Ptt)]|is.na(statusData$rReleaseTime),]

releaseDates<-unlist(by(statusData,statusData$Ptt,function(x){
  tab<-sort(table(x$ReleaseTime[!is.na(x$rReleaseTime)]))
  if(length(tab)==0)return(NA)
  if(tail(tab,1)/sum(tab)<.7)warning(sprintf('Ambiguous release time in PTT %s',x[1,'Ptt']))
  return(tail(names(tab),1))
}))
pdt<-readWild(list.files('processed','.*-PDTs.csv',full.names=TRUE))
pdt<-pdt[is.na(pdt$Partial),]
depthColumns<-colnames(pdt)[grep('^Depth',colnames(pdt))]
goodRows<-apply(pdt[,depthColumns],1,function(x)any(!is.na(x)))
pdt<-pdt[goodRows,]
pdt$maxDepth<-apply(pdt[,depthColumns],1,max,na.rm=TRUE)
pdt$minDepth<-apply(pdt[,depthColumns],1,min,na.rm=TRUE)
pdt$source<-'pdt'


info$releaseDate<-parse_date_time(sapply(info$ptt,function(x)releaseDates[as.character(x)]),'%H:%M:%S %d-%b-%y')
info$releaseDays<-(as.numeric(info$releaseDate)-as.numeric(info$deployDate))/24/60/60

minMaxDepth<-readWild(list.files('processed','.*-MinMaxDepth.csv',full.names=TRUE))

if(any(!info$ptt %in% tagData$DeployID))warning('Missing tag ',paste(info[!info$ptt %in% tagData$DeployID,'ptt'],collapse=''))
missingInfo<-tagData[!tagData$DeployID %in% info$ptt,]
write.csv(missingInfo[,c('Ptt','Instr','ReleaseType')],'out/missingTags.csv',row.names=FALSE)
table(tagData$ReleaseType)

tagData<-tagData[tagData$DeployID %in% info$ptt,]
statusData<-statusData[statusData$DeployID %in% info$ptt,]
minMaxDepth<-minMaxDepth[minMaxDepth$DeployID %in% info$ptt,]

statusData$deployDay<-calcDeployDay(statusData$Ptt,statusData$rDate,deployDates)
minMaxDepth$deployDay<-calcDeployDay(minMaxDepth$Ptt,minMaxDepth$rDate,deployDates)
minMaxDepth$min<-minMaxDepth$MinDepth
minMaxDepth$max<-minMaxDepth$MaxDepth
#these two seem to have 16 or 24 meter resolution so compress it all to 0
minMaxDepth[minMaxDepth$min<=24 & minMaxDepth$MinSource=='LightLoc','min']<-0
minMaxDepth[minMaxDepth$min<=24 & minMaxDepth$MinSource=='PDT','min']<-0
minMaxDepth[minMaxDepth$max<=16 &!is.na(minMaxDepth$max)&!is.na(minMaxDepth$MaxSource) & minMaxDepth$MaxSource=='LightLoc','max']<-0
minMaxDepth[minMaxDepth$max<=24 &!is.na(minMaxDepth$max)&!is.na(minMaxDepth$MaxSource) & minMaxDepth$MaxSource=='PDT','max']<-0
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


locations<-readWild(list.files('processed','.*-Locations.csv',full.names=TRUE))
locations$deployDay<-calcDeployDay(locations$Ptt,locations$rDate,deployDates)
lightLoc<-readWild(list.files('processed','.*-LightLoc.csv',full.names=TRUE))


histos<-readWild(list.files('processed','.*-Histos.csv',full.names=TRUE))
divePdt<-readWild(list.files('processed','.*-DivePDT.csv',full.names=TRUE))
divePdt$maxDepth<-divePdt$Depth
divePdt$minDepth<-NA
divePdt$source<-'divePdt'


statusData$minDepth<-0
statusData$maxDepth<-NA
statusData$source<-'status'

#need to add more. not sure worth it
depthCols<-c('rDate','minDepth','maxDepth','source')
allDepths<-rbind(divePdt[,depthCols],pdt[,depthCols],statusData[,depthCols])


releaseTypes<-unlist(by(statusData,statusData$Ptt,function(x){
  broke<-sort(table(x$InitiallyBroken))
  if(length(broke)>0){
    if(tail(broke,1)/sum(broke)<.7)warning(sprintf('Ambiguous breakage in PTT %s',x[1,'Ptt']))
    if(tail(names(broke),1)=='1')return('Broken')
  }
  tab<-sort(table(x$ReleaseType[!is.na(x$rReleaseTime)&x$ReleaseType!='']))
  if(length(tab)==0)return(NA)
  if(tail(tab,1)/sum(tab)<.7)warning(sprintf('Ambiguous release type in PTT %s',x[1,'Ptt']))
  return(tail(names(tab),1))
}))
info$releaseType<-releaseTypes[info$ptt]

info$fate<-NA
#scheduled release 
info$fate[info$releaseType %in% c('Scheduled','Interval')]<-'Scheduled'
#labeled too deeps (misses some)
info$fate[info$releaseType=='Too Deep']<-'TooDeep'
#labeled floaters (misses some)
info$fate[info$releaseType=='Floater']<-'Float'
#labeled floaters (misses some)
info$fate[info$releaseType=='Broken']<-'Broken'
#still on turtle?
info$fate[is.na(info$releaseType)]<-'StillOn'
info$fate[is.na(info$releaseType)&now()-info$deployDate>400]<-'Lost'
#too deep or float or no surface
info$fate[is.na(info$fate)]<-sapply(info$ptt[is.na(info$fate)],function(x,surfaceDepth=10){
  thisRelease<-info[x,'releaseDays']
  thisBelow<-belowSurface[[as.character(x)]]
  thisBelow<-thisBelow[thisBelow$end>thisRelease-2&thisBelow$start<thisRelease,]
  thisSurface<-onSurface[[as.character(x)]]
  thisSurface<-thisSurface[thisSurface$end>thisRelease-2&thisSurface$start<thisRelease,]
  thisDepths<-minMaxDepth[minMaxDepth$Ptt==x & minMaxDepth$deployDay<thisRelease & minMaxDepth$deployDay>thisRelease-20 &!is.na(minMaxDepth$max),'max']
  thisDepths<-rev(thisDepths)
  #depth <1000 and all following depths at surface
  if(any(thisDepths>1000)&&all(head(thisDepths,min(which(thisDepths>1000))-1)<surfaceDepth))return('TooDeep')
  if(nrow(thisSurface)==0&nrow(thisBelow)==0)return(NA)
  if(nrow(thisSurface)>0&nrow(thisBelow)>0)stop(simpleError('Both float and sink'))
  if(nrow(thisSurface)>0)return('Float')
  if(nrow(thisBelow)>0)return('ConstantDepth')
},surfaceDepth)
#unknown
info$fate[is.na(info$fate)]<-'Unknown'
  
info$realRelease<-sapply(info$ptt,function(x,surfaceDepth=10){
  thisRelease<-info[x,'releaseDays']
  thisDepths<-minMaxDepth[minMaxDepth$Ptt==x & minMaxDepth$deployDay<thisRelease & minMaxDepth$deployDay>thisRelease-20 &!is.na(minMaxDepth$max),c('deployDay','max')]
  thisDepths[max(c(1,which(thisDepths$max>surfaceDepth))),'deployDay']
},surfaceDepth)
info$lastDay<-info$realRelease
info[info$fate %in% c('StillOn','Lost'),'lastDay']<-sapply(info[info$fate %in% c('StillOn','Lost'),'ptt'],function(x)max(c(0,minMaxDepth[minMaxDepth$Ptt==x,'deployDay'])))

info$hook<-ifelse(grepl('Deep',info$Lightly.or.Deeply.Hooked..based.on.pics.),'Deep',ifelse(grepl('Light',info$Lightly.or.Deeply.Hooked..based.on.pics.),'Light',ifelse(grepl('Not [hH]ooked',info$Lightly.or.Deeply.Hooked..based.on.pics.),'NoHook',NA)))
info$hook[info$Lightly.or.Deeply.Hooked..based.on.pics.=='Unknown']<-NA

sst<-readWild(list.files('processed','.*-SST.csv',full.names=TRUE))
sst$deployDay<-calcDeployDay(sst$Ptt,sst$rDate,deployDates)


tad<-histos[histos$HistType=='TAD',]
tad$deployDay<-calcDeployDay(tad$Ptt,tad$rDate,deployDates)
tad<-tad[tad$deployDay>-50,]
tad<-tad[tad$Ptt %in% unique(statusData$Ptt),]
binCols<-grep('^Bin',colnames(tad))
tad[,binCols][tad[,binCols]=='']<-NA
for(ii in binCols){
  if(class(tad[,ii])=='character')tad[,ii]<-as.numeric(tad[,ii])
}
source('findDepthOffset.R')
tadLimits<-histos[histos$HistType=='TADLIMITS',]
pttTadLimits<-lapply(unique(tad$Ptt),function(ptt){
  thisLimits<-tadLimits[tadLimits$Ptt==ptt,] 
  if(nrow(thisLimits)>1)stop(simpleError('Multiple limits found'))
  if(nrow(thisLimits)==0)return(NULL)
  thisBins<-thisLimits[,binCols]
  thisBins<-unlist(thisBins[,!is.na(thisBins)&thisBins!=''])
  thisBins[length(thisBins)]<-sub('>[0-9.]+','Inf',thisBins[length(thisBins)])
  if(length(thisBins)<10)return(NULL)
  return(as.numeric(thisBins))
  #may have been set nrow to 0 above if it looks like there is missing bins
})
names(pttTadLimits)<-unique(tad$Ptt)
badLimits<-sapply(pttTadLimits,is.null)
pttTadLimits[badLimits]<-lapply(names(pttTadLimits)[badLimits],function(ptt){
  pttDist<-abs(as.numeric(ptt)-as.numeric(names(pttTadLimits)[!badLimits]))
  sisterPtts<-names(pttTadLimits)[!badLimits][pttDist<=sort(pttDist)[5]&pttDist<20]
  message('Inferring TAD limits from sister tags ',paste(sisterPtts,collapse=', '),' for tag ',ptt)
  replaceLimits<-unique(pttTadLimits[sisterPtts])
  if(length(replaceLimits)>1)stop(simpleError('Disagreement in TAD limits between neighboring PTTs'))
  if(length(replaceLimits)==0)stop(simpleError('No neighboring PTT found to set TAD'))
  return(replaceLimits[[1]])
})
maxLength<-max(sapply(pttTadLimits,length))
pttTadLimits<-do.call(rbind,lapply(pttTadLimits,function(x)c(x,rep(NA,maxLength-length(x)))))
depthFixes<-mapply(function(ptt,day)depthOffsets[[as.character(ptt)]][as.character(day)],tad$Ptt,round(tad$deployDay))
if(any(sapply(depthFixes,length)==0))stop(simpleError('Problem finding depth adjustment'))
tad$depthFix<-depthFixes
tadBins<-pttTadLimits[as.character(tad$Ptt),]-tad$depthFix
colnames(tadBins)<-sprintf('Depth%d',1:ncol(tadBins))
tad<-cbind(tad,tadBins)
tad$tadOffsetBin<-mapply(function(ptt,day)tadOffsets[[as.character(ptt)]][as.character(day)],tad$Ptt,tad$deployDay)
tad$naiveSurface<-(tad$Bin1+tad$Bin2)/100
#take 1 bin below 'surface'
tad$surface<- apply(tad[,c(colnames(tad)[binCols],'tadOffsetBin')],1,function(x)sum(as.numeric(x[1:(x[length(x)]+1)]),na.rm=TRUE))/100




lowSurfaceTime<-by(tad,tad$Ptt,function(x){
  belows<-binary2range(x$surface<.02 & x$deployDay> -10)
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
  belows<-belows[belows$end-belows$start>4,]
  return(belows)
})
>>>>>>> 60823e9d41692777281ebb42b1b95965fd838642


write.csv(info[,c('ptt','hook','deployDate','lastDay','fate')],'out/fate.csv',row.names=FALSE)
