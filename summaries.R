if(!exists('statusData'))source('readData.R')
t(t(table(year(info$deployDate))))
t(t(table(info$Tag.Type)))
table(ifelse(is.na(info$observer),'NA',info$observer),info$hook)
sum(info$lastDay)
table(info$lastDay<30,info$fate,year(info$deployDate))
table(info$lastDay<30,info$fate)

summary(info$releaseType %in% c('Scheduled','Interval'))

