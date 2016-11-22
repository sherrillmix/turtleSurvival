
sasso<-read.csv('other/sasso.csv',stringsAsFactors=FALSE)
swimmer<-read.csv('other/swimmer.csv',stringsAsFactors=FALSE)

#note that swimmer text implies that no mortality was directly observed and that hooking location was used to infer mortality
swimmer$mortality<-grepl('\\*$',swimmer$hookLoc)
#note that sasso counts turtles that had reduced diving before release as mortalities
sasso$mortality<-sasso$fate=='died'
sasso[is.na(sasso$time),'time']<-0
swimmer[swimmer$time=='--','time']<-0
swimmer$time<-as.numeric(swimmer$time)
#all locations either control or flipper/mouth
sasso$isDeep<-FALSE
swimmer$isDeep<-grepl('Deep',swimmer$hookLoc)
