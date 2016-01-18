source('~/scripts/R/dna.R')
if(!exists('tagData'))source('readData.R')
statusCols<-c('BrokenThermistor', 'BrokenLink', 'NoDawnDusk', 'ReleaseType', 'InitiallyBroken', 'FastGPSPower', 'TWICPower', 'PowerLimit', 'WetDry', 'Resets', 'PreReleaseTilt', 'PreReleaseTiltSd', 'PreReleaseTiltCount', 'XmitQueue', 'FastGPSLocNumber', 'FastGPSFailures', 'BattDiscon')
pdf('out/minMaxDepth.pdf')
for(ii in unique(info$ptt[order(info$fate,info$ptt)])){
  thisData<-minMaxDepth[minMaxDepth$Ptt==as.numeric(ii)&!is.na(minMaxDepth$min)&!is.na(minMaxDepth$max)&minMaxDepth$deployDay>-100,]
  if(nrow(thisData)==0){
    warning('No data for ',ii)
    next
  }
  thisStatus<-statusData[statusData$Ptt==as.numeric(ii),]
  thisStatus<-thisStatus[apply(thisStatus[,statusCols],1,function(x)any(!is.na(x)&x!=''&x!=0)),]
  thisBelow<-belowSurface[[as.character(ii)]]
  thisSurface<-onSurface[[as.character(ii)]]
  thisInfo<-info[info$ptt==ii,]
  xlim<-range(thisData$deployDay)
  ylim<-range(c(0,thisData$MinDepth,thisData$MaxDepth))
  plot(1,1,type='n',main=ii,xlim=xlim+c(-.2,.2)*diff(xlim),ylim=rev(ylim),xlab='Deploy day',ylab='Minimum depth')
  lines(thisData$deployDay,thisData$MinDepth,col='gray')
  points(thisData$deployDay,thisData$min)
  lines(thisData$deployDay,thisData$min)
  if(nrow(thisBelow)>0)segments(thisBelow$start,0,thisBelow$end,0,lwd=4,col='#FF000033')
  abline(v=thisInfo$releaseDays,col='#0000FF77',lty=2)
  mtext(ifelse(is.na(thisInfo$fate),'Unknown',thisInfo$fate),1,line=-2,cex=3)
  plot(1,1,type='n',main=ii,xlim=xlim+c(-.2,.2)*diff(xlim),ylim=rev(ylim),xlab='Deploy day',ylab='Maximum depth')
  lines(thisData$deployDay,thisData$MaxDepth,col='gray')
  lines(thisData$deployDay,thisData$max)
  points(thisData$deployDay,thisData$max)
  abline(v=thisInfo$releaseDays,col='#0000FF77',lty=2)
  if(nrow(thisSurface)>0)segments(thisSurface$start,par('usr')[4]/2,thisSurface$end,par('usr')[4]/2,lwd=4,col='#0000FF33')
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
