

pdf("../results/attempted_age.pdf",width=5,height=5)
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(trials_attempted ~age,data=data,pch=20,xlab="Age (years)",ylab='Trials attempted',type="n")
points(data$age[data$ed=="higher"], data$trials_attempted[data$ed=="higher"],pch=20,col="red")
points(data$age[data$ed=="lower"], data$trials_attempted[data$ed=="lower"],pch=20,col="black")
abline(lm(trials_attempted ~age,data=data,subset=c(data$ed=="lower")))
abline(lm(trials_attempted ~age,data=data,subset=c(data$ed=="higher")),col="red")
text(4,0.45,"Higher SES",col="red")
text(4,0.4,"Lower SES")
dev.off()

pdf("../results/reliab_pc.pdf",width=5,height=5)
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(pc_1 ~pc_2,data=data,pch=20,xlab="% correct odd",ylab='% correct even',xlim=c(0,1),ylim=c(0,1))
abline(lm(pc_1 ~ pc_2,data=data))
text(.7,.2,paste("r=",round(cor.test(data $rt_1 , data $rt_2)$est,3)))
dev.off()

pdf("../results/reliab_rt.pdf",width=5,height=5)
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(rt_1 ~rt_2,data=data,pch=20,xlab="RT odd",ylab='RT even',xlim=c(1000,4500),ylim=c(1000,4500))
abline(lm(rt_1 ~ rt_2,data=data))
text(4000,1000,paste("r=",round(cor.test(data $rt_1 , data $rt_2)$est,3)))
dev.off()


pdf("../results/pc_age.pdf",width=5,height=5)
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(pc~age,data=data,pch=20,xlab="Age (years)",ylab='Percent correct',type="n")
lines(c(-1,3),c(.5,.5),lty=3,col="gray")
points(data$age[data$ed=="higher"], data$pc[data$ed=="higher"],pch=20,col="red")
points(data$age[data$ed=="lower"], data$pc[data$ed=="lower"],pch=20,col="black")
abline(lm(pc~age,data=data,subset=c(data$ed=="lower")))
abline(lm(pc~age,data=data,subset=c(data$ed=="higher")),col="red")
text(4,0.45,"Higher SES",col="red")
text(4,0.4,"Lower SES")
dev.off()



pdf("../results/rt_age.pdf",width=5,height=5)
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(log(rt_corr)~age,data=data,pch=20,xlab="Age (years)",ylab='Percent correct',type="n")
points(data$age[data$ed=="higher"], log(data$rt_corr[data$ed=="higher"]),pch=20,col="red")
points(data$age[data$ed=="lower"], log(data$rt_corr[data$ed=="lower"]),pch=20,col="black")
abline(lm(log(rt_corr)~age,data=data,subset=c(data$ed=="lower")))
abline(lm(log(rt_corr)~age,data=data,subset=c(data$ed=="higher")),col="red")
text(4,8.2,"Higher SES",col="red")
text(4,8.15,"Lower SES")
dev.off()




myjitter=jitter(rep(1,length(data$pc_easy)))
myjitter=c(myjitter,myjitter+0.2,myjitter+0.4)
pdf("../results/pc_diff.pdf")
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(myjitter, c(data$pc_easy,data$pc_mod,data$pc_diff),pch=20,ylim=c(0,1),xlab="Frequency group",ylab="Proportion correct",xaxt="n",type="n",xlim=c(0.95,1.45))
	lines(c(-1,3),c(.5,.5),lty=3,col="gray")
axis(1,at=c(1,1.2,1.4),labels=c("Highest","Moderate","Lowest"))
points(myjitter[data$ed=="higher"], c(data$pc_easy[data$ed=="higher"], data$pc_mod[data$ed=="higher"], data$pc_diff[data$ed=="higher"]),pch=20,col="red")

points(c(1.05,1.235, 1.45),c(mean(data$pc_easy[data$ed=="higher"],na.rm=T), mean(data$pc_mod[data$ed=="higher"],na.rm=T), mean(data$pc_diff[data$ed=="higher"],na.rm=T)),pch=3,cex=3,col="red")

points(myjitter[data$ed=="lower"], c(data$pc_easy[data$ed=="lower"], data$pc_mod[data$ed=="lower"], data$pc_diff[data$ed=="lower"]),pch=20,col="black")
points(c(1.05,1.235, 1.45),c(mean(data$pc_easy[data$ed=="lower"],na.rm=T), mean(data$pc_mod[data$ed=="lower"],na.rm=T), mean(data$pc_diff[data$ed=="lower"],na.rm=T)),pch=3,cex=3,col="black")

text(1.2,0.1,"Higher SES",col="red")
text(1.2,0.05,"Lower SES")

dev.off()


pdf("../results/pc_lex.pdf")
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(myjitter, c(data$pc_noun,data$pc_verb,data$pc_adj),pch=20,ylim=c(0,1),xlab="Lexical category",ylab="Proportion correct",xaxt="n",type="n",xlim=c(0.95,1.45))
	lines(c(-1,3),c(.5,.5),lty=3,col="gray")
axis(1,at=c(1,1.2,1.4),labels=c("Noun","Verb","Adjective"))
points(myjitter[data$ed=="higher"], c(data$pc_noun[data$ed=="higher"], data$pc_verb[data$ed=="higher"], data$pc_adj[data$ed=="higher"]),pch=20,col="red")

points(c(1.05,1.235, 1.45),c(mean(data$pc_noun[data$ed=="higher"],na.rm=T), mean(data$pc_verb[data$ed=="higher"],na.rm=T), mean(data$pc_adj[data$ed=="higher"],na.rm=T)),pch=3,cex=3,col="red")

points(myjitter[data$ed=="lower"], c(data$pc_noun[data$ed=="lower"], data$pc_verb[data$ed=="lower"], data$pc_adj[data$ed=="lower"]),pch=20,col="black")
points(c(1.05,1.235, 1.45),c(mean(data$pc_noun[data$ed=="lower"],na.rm=T), mean(data$pc_verb[data$ed=="lower"],na.rm=T), mean(data$pc_adj[data$ed=="lower"],na.rm=T)),pch=3,cex=3,col="black")

text(1.2,0.1,"Higher SES",col="red")
text(1.2,0.05,"Lower SES")

dev.off()


pdf("../results/rt_diff.pdf")
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot( (data $rt_easy)/1000,(data $rt_diff)/1000,pch=20,xlim=c(1,4.5),ylim=c(1,4.5),xlab="Response time to words with highest frequencies",ylab="Response time to words with lowest frequencies",type="n")
lines(c(-1,5),c(-1,5),lty=2)
points((data $rt_easy[data$ed=="higher"])/1000,(data $rt_diff[data$ed=="higher"])/1000,pch=20,col="red")
points((data $rt_easy[data$ed=="lower"])/1000,(data $rt_diff[data$ed=="lower"])/1000,pch=20,col="black")

text(4,1.2,"Higher SES",col="red")
text(4,1.05,"Lower SES")

dev.off()