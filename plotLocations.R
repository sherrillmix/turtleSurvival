
pdf('out/locations.pdf')
for(ii in unique(info$ptt[order(info$fate,info$ptt)])){
  thisData<-locations[locations$Ptt==as.numeric(ii)&!is.na(locations$Latitude)&!is.na(locations$Longitude)&locations$deployDay>-1,]
  thisInfo<-info[info$ptt==ii,]
  if(nrow(thisData)==0){
    warning('No data for ',ii)
    next
  }
  deployLoc<-thisData[min(which(thisData$deployDay>0&thisData$Quality %in% 3:0 | thisData$Type=='GPE')),]
  releaseLoc<-thisData[max(c(which(thisData$deployDay<thisInfo$realRelease),0)),]
  thisBelow<-belowSurface[[as.character(ii)]]
  thisSurface<-onSurface[[as.character(ii)]]
  #xlim<-range(thisData$Longitude)
  xlim<-c(-70,-45)
  #ylim<-range(thisData$Latitude)
  ylim<-c(-20,60)
  plot(1,1,type='n',main=ii,xlim=xlim+c(-.2,.2)*diff(xlim),ylim=ylim,xlab='',ylab='')
  with(thisData[thisData$deployDay<=thisInfo$realRelease & thisData$deployDay>-1,],lines(Longitude,Latitude,col='gray'))
  with(thisData[thisData$deployDay<=thisInfo$realRelease & thisData$deployDay>-1,],points(Longitude,Latitude,pch=ifelse(Type=='GPE',2,1)))
  points(releaseLoc$Longitude,releaseLoc$Latitude,cex=2,col='red')
  points(deployLoc$Longitude,deployLoc$Latitude,cex=2,col='red',pch=2)
  map(add=TRUE)
  plot(thisData$deployDay,thisData$Latitude,ylab='Latitude',xlab='Deploy day',type='b',main=ii,pch=ifelse(thisData$Type=='GPE',2,1),cex=.7)
  abline(v=thisInfo$realRelease,col='#0000FF77',lty=2)
  legend('bottomleft',c('Argos','Light'),pch=2:1,inset=.01)
}
dev.off()


