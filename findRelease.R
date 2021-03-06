rtfs<-list.files('Sherrill-Mix Caretta 2015/PAT_Programming','\\.rtf$',recursive=TRUE,full.names=TRUE)
text<-lapply(rtfs,readLines,skipNul=TRUE)
serials<-sapply(text,function(x)sapply(strsplit(x[grep('Serial Number ',x)],'[ \\]+'),'[[',6))
releases<-sapply(text,function(x)sapply(strsplit(x[grep('Date or Days after deployment:',x)],'[ \\]+'),'[[',9))
reportDate<-sapply(text,function(x)sapply(strsplit(x[grep('Report date:',x)],'[ \\]+'),function(x)paste(x[6:8],collapse=' ')))
maxDepth<-sub('m','',sapply(text,function(x)strsplit(x[grep('Immediately Release if depth.*True',x)],'[: ]')[[1]][13]))
constantTime<-sub('m','',sapply(text,function(x)strsplit(x[grep('And the event lasts longer than',x)],'[: ]')[[1]][17]))
constantDepth<-sub('m','',sapply(text,function(x)strsplit(x[grep('And the event lasts longer than',x)+2],'[: ]')[[1]][16]))
rDates<-dmy_hms(reportDate)
isMax<-rDates==ave(rDates,serials,FUN=max)
selector<-serials %in% info$ID & isMax
serials<-serials[selector]
releases<-releases[selector]
if(any(table(serials)>1))stop(simpleError('Duplicate entries'))
out<-structure(as.numeric(releases),.Names=serials)
outDrops<-data.frame('serial'=serials,'constantTime'=as.numeric(constantTime[selector])*24,constantDepth=constantDepth[selector],'maxDepth'=maxDepth[selector])


htms<-list.files('Sherrill-Mix Caretta 2015/PAT_Programming','\\.htm$',recursive=TRUE,full.names=TRUE)
text<-lapply(htms,readLines,skipNul=TRUE)
serials<-sapply(text,function(x)strsplit(x[grep("Tag's Serial Number",x)][1],'</?td>|\\&')[[1]][3])
prematures<-sapply(text,function(x)x[grep('Generate a premature',x)])
corrects<-sapply(text,function(x)x[grep('Automatic Correction',x)][1])
pcDate<-sub(' at ',' ',sapply(text,function(x)strsplit(x[grep('PC Date',x)],'</?td>')[[1]][2]))
rDates<-dmy_hms(pcDate)
releases<-sapply(text,function(x){
  splits<-strsplit(x[grep('Release day.*after deployment',x)],'<td>| ')
  if(length(splits)==0)return(NA)
  else return(splits[[1]][4])
})
drops<-do.call(rbind,lapply(text,function(x){
  splits<-strsplit(x[grep('Time at constant depth to initiate release',x)],'<td>| ')
  constantTime<-ifelse(length(splits)==0,NA,splits[[1]][9])
  splits<-strsplit(x[grep('Minimum depth to start premature release detection',x)],'</?td>| ')
  constantDepth<-ifelse(length(splits)==0,NA,sub('m','',splits[[1]][9]))
  splits<-strsplit(x[grep('Generate a premature release if all depths',x)],'</?td>| ')
  maxDepth<-ifelse(length(splits)==0,NA,ifelse(length(splits[[1]])==16,splits[[1]][15],sub('m','',splits[[1]][12])))
  if(!is.na(constantTime)&is.na(maxDepth))browser()
  return(data.frame('constantTime'=constantTime,'constantDepth'=constantDepth,'maxDepth'=maxDepth))
}))
isMax<-rDates==ave(rDates,serials,FUN=max)
selector<-serials %in% info$ID & isMax
serials<-serials[selector]
releases<-releases[selector]
drops<-drops[selector,]
drops$serial<-serials
outDrops<-rbind(outDrops,drops[,colnames(outDrops)])
prematureDisable<-grepl('is disabled',prematures[selector])
correctDry<-grepl('Using first dry reading',corrects[selector])
names(prematureDisable)<-names(correctDry)<-serials
if(any(table(serials)>1))stop('Duplicate entries')
if(any(serials %in% names(out)))stop('Data in both rtf and htm')
out<-c(out,structure(as.numeric(releases),.Names=serials))
info$releasePlan<-out[info$ID]
info$prematureDisable<-prematureDisable[info$ID]
info$correctDry<-correctDry[info$ID]
if(any(is.na(info$releasePlan))){
  for(ii in which(is.na(info$releasePlan))){
    fills<-info[year(info$deployDate)==year(info$deployDate)[ii]&info$Tag.Type==info$Tag.Type[ii]&!is.na(info$releasePlan),'releasePlan']
    if(all(fills==fills[1])){
      warning('Filling PTT ',info$PTTID[ii],' release date as ',fills[1],' based on ',length(fills),' similar tags')
      info$releasePlan[ii]<-fills[1]
    }else{
      stop('Disagreement in filling release date')
    }
  }
}

if(any(is.na(info$releasePlan)))stop('Missing ID in releases')
