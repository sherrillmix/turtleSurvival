rtfs<-list.files('Sherrill-Mix Caretta 2015/PAT_Programming','\\.rtf$',recursive=TRUE,full.names=TRUE)
text<-lapply(rtfs,readLines,skipNul=TRUE)
serials<-sapply(text,function(x)sapply(strsplit(x[grep('Serial Number ',x)],'[ \\]+'),'[[',6))
releases<-sapply(text,function(x)sapply(strsplit(x[grep('Date or Days after deployment:',x)],'[ \\]+'),'[[',9))
selector<-serials %in% info$ID
serials<-serials[selector]
releases<-releases[selector]

htms<-list.files('Sherrill-Mix Caretta 2015/PAT_Programming','\\.htm$',recursive=TRUE,full.names=TRUE)
text<-lapply(htms,readLines,skipNul=TRUE)
serials<-sapply(text,function(x)strsplit(x[grep("Tag's Serial Number",x)][1],'</?td>|\\&')[[1]][3])
prematures<-sapply(text,function(x)x[grep('Generate a premature',x)])
corrects<-sapply(text,function(x)x[grep('Automatic Correction',x)])
releases<-sapply(text,function(x){
  splits<-strsplit(x[grep('Release day.*after deployment',x)],'<td>| ')
  if(length(splits)==0)return(NULL)
  else return(splits[[1]][4])
})

