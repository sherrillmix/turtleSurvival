source('~/scripts/R/dna.R')
if(!exists('tagData'))source('readData.R')
statusCols<-c('HauledOut', 'BrokenThermistor', 'BrokenLink', 'NoDawnDusk', 'ReleaseType', 'InitiallyBroken', 'BurnMinutes', 'FastGPSPower', 'TWICPower', 'PowerLimit', 'WetDry', 'StatusWord', 'Resets', 'PreReleaseTilt', 'PreReleaseTiltSd', 'PreReleaseTiltCount', 'XmitQueue', 'FastGPSLocNumber', 'FastGPSFailures', 'BattDiscon')
pdf('out/minMaxDepth.pdf')
for(ii in unique(minMaxDepth$Ptt)){
  thisData<-minMaxDepth[minMaxDepth$DeployID==ii&!is.na(minMaxDepth$min)&!is.na(minMaxDepth$MaxDepth),]
  thisStatus<-statusData[statusData$Ptt==ii,]
  thisStatus<-thisStatus[apply(thisStatus[,statusCols],1,function(x)any(!is.na(x)&x!=''&x!=0)),]
  thisBelow<-belowSurface[[as.character(ii)]]
  thisInfo<-info[info$PTTID==ii,]
  xlim<-range(thisData$deployDay)
  ylim<-range(c(thisData$min,thisData$MaxDepth))
  plot(1,1,type='n',main=ii,xlim=xlim+c(-.2,.2)*diff(xlim),ylim=rev(ylim),xlab='Deploy day',ylab='Minimum depth')
  points(thisData$deployDay,thisData$min)
  lines(thisData$deployDay,thisData$MinDepth,col='gray')
  lines(thisData$deployDay,thisData$min)
  if(nrow(thisBelow)>0)segments(thisData[thisBelow$start,'deployDay'],20,thisData[thisBelow$end,'deployDay'],20,lwd=4,col='#FF000033')
  abline(v=thisInfo$releaseDays,col='#0000FF77',lty=2)
  plot(1,1,type='n',main=ii,xlim=xlim+c(-.2,.2)*diff(xlim),ylim=rev(ylim),xlab='Deploy day',ylab='Maximum depth')
  lines(thisData$deployDay,thisData$MaxDepth)
  points(thisData$deployDay,thisData$MaxDepth)
  abline(v=thisInfo$releaseDays,col='#0000FF77',lty=2)
  for(status in statusCols){
    selector<-!is.na(thisStatus[,status])&thisStatus[,status]>0&thisStatus[,status]!=''
    if(!any(selector))next()
    selectRanges<-binary2range(selector)
    selectRanges$startDay<-thisStatus[selectRanges$start,'deployDay']
    selectRanges$endDay<-thisStatus[selectRanges$end,'deployDay']
    yPos<-which(statusCols==status)/length(statusCols)*diff(ylim)
    text(apply(selectRanges[,c('startDay','endDay')],1,mean),yPos,paste(status,ifelse(thisStatus[selectRanges$start,status]!=1,thisStatus[selectRanges$start,status],'')),col='red',srt=45)
    abline(v=c(selectRanges$startDay,selectRanges$endDay),lty=3,col='red')
    segments(selectRanges$startDay,yPos,selectRanges$endDay,yPos,lty=3,col='#FF000077')
  }
}
dev.off()
