source('~/scripts/R/dna.R')
if(!exists('tagData'))source('readData.R')
statusCols<-c('HauledOut', 'BrokenThermistor', 'BrokenLink', 'NoDawnDusk', 'ReleaseType', 'ReleaseTime', 'InitiallyBroken', 'BurnMinutes', 'FastGPSPower', 'TWICPower', 'PowerLimit', 'WetDry', 'StatusWord', 'Resets', 'PreReleaseTilt', 'PreReleaseTiltSd', 'PreReleaseTiltCount', 'XmitQueue', 'FastGPSLocNumber', 'FastGPSFailures', 'BattDiscon')
pdf('out/minMaxDepth.pdf')
for(ii in unique(minMaxDepth$Ptt)){
  thisData<-minMaxDepth[minMaxDepth$DeployID==ii&!is.na(minMaxDepth$MinDepth)&!is.na(minMaxDepth$MaxDepth),]
  thisStatus<-statusData[statusData$Ptt==ii,]
  thisStatus<-thisStatus[apply(thisStatus[,statusCols],1,function(x)any(!is.na(x)&x!=''&x!=0)),]
  xlim<-range(thisData$deployDay)
  ylim<-range(c(thisData$MinDepth,thisData$MaxDepth))
  plot(1,1,type='n',main=ii,xlim=xlim+c(-.2,.2)*diff(xlim),ylim=rev(ylim),xlab='Deploy day',ylab='Depth')
  points(thisData$deployDay,thisData$MinDepth)
  lines(thisData$deployDay,thisData$MinDepth)
  points(thisData$deployDay,thisData$MinDepth)
  lines(thisData$deployDay,thisData$MaxDepth)
  points(thisData$deployDay,thisData$MaxDepth)
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
