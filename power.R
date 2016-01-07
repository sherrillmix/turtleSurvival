#http://www.ncbi.nlm.nih.gov/pubmed/6354290
calcNeededDeaths<-function(alpha,power,p,effect)(qnorm(1-alpha)+qnorm(power))^2 / (p*(1-p)*log(effect)^2)
calcNeededDeaths(.05,.8,.5,4)

library(survival)
simulateCox<-function(n1,n2=n1,pDeath=c(.001,.004),possibleTimes=1:365){
	turtles<-data.frame('time'=sample(possibleTimes,n1+n2,TRUE),'group'=rep(c(1,2),c(n1,n2)))
	turtles$deathDay<-apply(turtles,1,function(x)min(c(Inf,which(rbinom(x[1],1,pDeath[x[2]])==1))))
	turtles$status<-turtles$deathDay<Inf
	turtles$censorDay<-ifelse(turtles$deathDay<Inf,turtles$deathDay,turtles$time)
	p<-summary(coxph(Surv(censorDay, status) ~ I(group-1), turtles))$coef[,'Pr(>|z|)']
	return(c(sum(turtles$status),p))
}
results<-replicate(1000,simulateCox(75,pDeath=c(.002,.004)))

bestData<-data.frame('time'=c(rep(365,27),40,100,80),'condition'=rep(c(0,1),c(27,3)),'group'=rep(c(0,1),c(15,15)))
summary(coxph(Surv(time,condition)~group,bestData))
