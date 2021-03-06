library(maps)
library(dnar)
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
  abline(v=thisInfo$releaseDays,col='#0000FF33',lty=2)
  abline(v=thisInfo$realRelease,col='#0000FF77',lty=2)
  mtext(ifelse(is.na(thisInfo$fate),'Unknown',thisInfo$fate),1,line=-2,cex=3)
  plot(1,1,type='n',main=ii,xlim=xlim+c(-.2,.2)*diff(xlim),ylim=rev(ylim),xlab='Deploy day',ylab='Maximum depth')
  lines(thisData$deployDay,thisData$MaxDepth,col='gray')
  lines(thisData$deployDay,thisData$max)
  points(thisData$deployDay,thisData$max)
  abline(v=thisInfo$releaseDays,col='#0000FF33',lty=2)
  abline(v=thisInfo$realRelease,col='#0000FF77',lty=2)
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

pdf('out/minMaxStatus.pdf')
par(mar=c(5.5,4,1,.1),mgp=c(2.5,.8,0),las=1)
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
  thisSurfaceTime<-lowSurfaceTime[[as.character(ii)]]
  thisInfo<-info[info$ptt==ii,]
  xlim<-range(thisData$deployDay)
  ylim<-range(c(0,thisData$MinDepth,thisData$MaxDepth))
  plot(1,1,type='n',main=ii,xlim=xlim+c(-.2,.2)*diff(xlim),ylim=rev(ylim),xlab='Deploy day',ylab='Depth')
  lines(thisData$deployDay,thisData$MinDepth,col='gray')
  points(thisData$deployDay,thisData$min)
  lines(thisData$deployDay,thisData$min)
  if(nrow(thisBelow)>0)segments(thisBelow$start,0,thisBelow$end,0,lwd=4,col='#FF000033')
  abline(v=thisInfo$releaseDays,col='#0000FF33',lty=2)
  abline(v=thisInfo$realRelease,col='#0000FF77',lty=2)
  mtext(ifelse(is.na(thisInfo$fate),'Unknown',thisInfo$fate),1,line=-2,cex=2)
  lines(thisData$deployDay,thisData$MaxDepth,col='gray')
  lines(thisData$deployDay,thisData$max)
  points(thisData$deployDay,thisData$max)
  if(nrow(thisSurface)>0)segments(thisSurface$start,par('usr')[4]/2,thisSurface$end,par('usr')[4]/2,lwd=4,col='#0000FF33')
  if(nrow(thisSurfaceTime)>0)segments(thisSurfaceTime$start,par('usr')[4]*.75,thisSurfaceTime$end,par('usr')[4]*.75,lwd=4,col='#00FF0033')
  legend('bottomleft',c('Low surface','No surface','Long surface','Tag release','Infer release','Raw mins'),lty=c(1,1,1,2,2,1),col=c('#00FF0033','#0000FF33','#FF000033','#0000FF33','#0000FF77','grey'),lwd=c(4,4,4,1,1,1),inset=c(-.14,-.21),ncol=2,xpd=NA,bty='n')
}
dev.off()

cols<-rev(heat.colors(1001))
pdf('out/tad.pdf',width=12)
for(ii in unique(info$ptt[order(info$fate,info$ptt)])){
  thisData<-tad[tad$Ptt==as.numeric(ii)&tad$deployDay>-1,]
  if(nrow(thisData)==0){
    warning('No tad data for ',ii)
    next
  }
  #thisBelow<-belowSurface[[as.character(ii)]]
  #thisSurface<-onSurface[[as.character(ii)]]
  #thisSurfaceTime<-lowSurfaceTime[[as.character(ii)]]
  thisInfo<-info[info$ptt==ii,]
  thisBins<-colnames(thisData)[grepl('^Bin',colnames(thisData))&apply(is.na(thisData),2,mean)<.1]
  thisDepths<-thisData[,colnames(thisData)[grepl('^Depth[0-9]+$',colnames(thisData))&apply(is.na(thisData),2,mean)<.1]]
  thisProps<-thisData[,thisBins]
  thisMinMax<-minMaxDepth[minMaxDepth$Ptt==as.numeric(ii)&!is.na(minMaxDepth$min)&!is.na(minMaxDepth$max)&minMaxDepth$deployDay>-100,]
  minMaxLim<-range(c(0,thisMinMax$min,thisMinMax$max))
  if(ncol(thisDepths)==length(thisBins)){
    thisDepths<-cbind('Depth0'=-10,thisDepths)
  }
  if(ncol(thisDepths)==length(thisBins)-1 && !all(thisDepths[,ncol(thisDepths)]==Inf)){
    thisDepths<-cbind('Depth0'=-10,thisDepths,'DepthInf'=Inf)
  }
  if(ncol(thisProps)<ncol(thisDepths)-1){
    dummy<-matrix(0,nrow(thisProps),ncol=ncol(thisDepths)-ncol(thisProps)-1)
    thisProps<-cbind(thisProps,dummy)
  }
  message('Bins ',length(thisBins),' Depths ',ncol(thisDepths))
  if(ncol(thisDepths)!=ncol(thisProps)+1)stop(simpleError('Depth and bin mismatch'))
  thisDepths[is.infinite(thisDepths[,ncol(thisDepths)]),ncol(thisDepths)]<-2000
  xlim<-c(-1,max(thisData$deployDay)+.5)
  ylim<-c(length(thisBins)+.5,.5)
  #plot(1,1,type='n',main=ii,xlim=xlim,ylim=c(sqrt(500),-sqrt(10)),xlab='Deploy day',ylab='Depth',yaxs='i',xaxs='i',yaxt='n')
  #sqrtWithNeg<-function(x)sqrt(abs(x))*sign(x)
  #rect(rep(thisData$deployDay,length(thisBins))-.125,sqrtWithNeg(unlist(thisDepths[,-ncol(thisDepths)])),rep(thisData$deployDay,length(thisBins))+.125,sqrtWithNeg(unlist(thisDepths[,-1])),col=cols[1+round(unlist(thisData[,thisBins])*10)],border=NA)
  plot(1,1,type='n',main=paste(ii,' Correct dry: ',thisInfo$correctDry,' Premature disable: ',thisInfo$prematureDisable),xlim=xlim,ylim=ylim,xlab='Deploy day',ylab='Depth bin',yaxs='i',xaxs='i',las=1)
  rect(rep(thisData$deployDay,length(thisBins))-.125,rep(1:length(thisBins),each=nrow(thisData))-.5,rep(thisData$deployDay,length(thisBins))+.125,rep(1:length(thisBins),each=nrow(thisData))+.5,col=cols[1+round(unlist(thisData[,thisBins])*10)],border=NA)
  abline(v=thisInfo$releaseDays,col='#0000FF33',lty=2)
  abline(v=thisInfo$realRelease,col='#0000FF77',lty=2)
  abline(h=0,lty=2)
  box()
  #if(nrow(thisSurface)>0)segments(thisSurface$start,par('usr')[4]/2,thisSurface$end,par('usr')[4]/2,lwd=4,col='#0000FF33')
  #if(nrow(thisSurfaceTime)>0)segments(thisSurfaceTime$start,par('usr')[4]*.75,thisSurfaceTime$end,par('usr')[4]*.75,lwd=4,col='#00FF0033')
  #if(nrow(thisBelow)>0)segments(thisBelow$start,0,thisBelow$end,0,lwd=4,col='#FF000033')
  lines(thisData$deployDay,thisData$tadOffsetBin)
  axis(2,1:length(thisDepths[1,])-.5,thisDepths[1,],cex.axis=.5,las=1,tcl=-.2,mgp=c(3,.3,0))
  par(new=TRUE)
  plot(1,1,type='n',xlab='',ylab='',xaxt='n',yaxt='n',xlim=xlim,ylim=rev(minMaxLim),xaxs='i')
  lines(thisMinMax$deployDay,thisMinMax$min,col='#0000FF66')
  lines(thisMinMax$deployDay,thisMinMax$max,col='#0000FF66')
  axis(4,pretty(minMaxLim),las=1,mgp=c(3,.3,0),tcl=-.2)
}
dev.off()


