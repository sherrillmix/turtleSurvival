
statusDepths<-statusData[!is.na(statusData$Depth)&statusData$deployDay>-5,]
depthRange<-range(statusDepths$Depth[!is.na(statusDepths$Depth)])
#NOTE generating plot and adjustment info in one step
pdf('out/depthOffset.pdf')
depthOffsets<-lapply(info$PTTID,function(ptt){
  message(ptt)
  thisInfo<-info[info$PTTID==ptt,]
  #thisDepth<-statusDepths[statusDepths$Ptt==ptt&statusDepths$deployDay<thisInfo$lastDay+5,]
  thisDepth<-statusDepths[statusDepths$Ptt==ptt,]
  times<-thisDepth$deployDay
  depths<-thisDepth$Depth + ifelse(is.na(thisDepth$ZeroDepthOffset),0,thisDepth$ZeroDepthOffset) * ifelse(is.na(thisDepth$DepthSensor),.5,thisDepth$DepthSensor)
  if(length(depths)==0){
    safeMax<-round(max(c(thisInfo$lastDay,thisInfo$releaseDays,100),na.rm=TRUE))
    out<-rep(0,safeMax+201)
    names(out)<--100:(safeMax+100)
    return(out)
  }
  plot(times,depths,main=ptt,ylim=depthRange,ylab='Depth',xlab='Days after deployment',xlim=c(-10,thisInfo$lastDay+10))
  abline(h=0,lty=2)
  depthReg<-lm(depths~times)
  coefs<-depthReg$coefficient[c('(Intercept)','times')]
  coefs[is.na(coefs)]<-0
  abline(coefs[1],coefs[2],col='#FF000077')
  approxDates<-seq(0,ceiling(max(c(times,thisInfo$lastDay)))+50,1)
  if(length(depths)==1){
    depthAdjust<-rep(depths,length(approxDates))
    depthAdjust2<-rep(depths,length(approxDates))
  } else {
    depthAdjust<-approx(times,depths,approxDates,rule=2)$y
    #depthAdjust2<-spline(times,depths,xout=approxDates)$y
    #depthAdjust2<-(depthAdjust+coefs[1]+coefs[2]*approxDates)/2
    weeks<-seq(0,max(times),10)
    weekMedians<-sapply(weeks,function(x){
      selector<-abs(times-x)<5
      if(any(selector))return(median(depths[abs(times-x)<7]))
      else return(NA)
    })
    if(sum(!is.na(weekMedians))>1)depthAdjust2<-approx(weeks,weekMedians,approxDates,rule=2)$y
    else depthAdjust2<-rep(median(depths),length(approxDates))
  }
  lines(approxDates,depthAdjust,col='#00FF0077')
  lines(approxDates,depthAdjust2,col='#0000FF77')
  abline(v=c(0,thisInfo$lastDay),lty=3)
  out<-depthAdjust2
  names(out)<-approxDates
  return(out)
})
dev.off()

names(depthOffsets)<-info$PTTID

tadOffsets<-lapply(info$PTTID,function(ptt){
  thisInfo<-info[info$PTTID==ptt,]
  #thisDepth<-statusDepths[statusDepths$Ptt==ptt&statusDepths$deployDay<thisInfo$lastDay+5,]
  thisTad<-tad[tad$Ptt==ptt,]
  times<-thisTad$deployDay
  minDepths<-sapply(times,function(x){
    nonZeros<-which(apply(thisTad[abs(times-x)<3,binCols],2,sum,na.rm=TRUE)>0)
    if(min(nonZeros)!=max(nonZeros))return(min(nonZeros))
    else return(NA)
  })
  if(any(is.na(minDepths))){
    minDepths[is.na(minDepths)]<-approx(times,minDepths,times[is.na(minDepths)],rule=2)$y
  }
  names(minDepths)<-times
  return(minDepths)
})
names(tadOffsets)<-info$PTTID


