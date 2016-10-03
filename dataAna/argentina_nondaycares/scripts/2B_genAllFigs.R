

pdf("../results/attempted_age.pdf",width=5,height=5)
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(trials_attempted ~age,data=data,pch=20,xlab="Age (years)",ylab='Trials attempted',type="n")
points(data$age[data$SES=="Mid"], data$trials_attempted[data$SES=="Mid"],pch=20,col="red")
points(data$age[data$SES=="Low"], data$trials_attempted[data$SES=="Low"],pch=20,col="black")
abline(lm(trials_attempted ~age,data=data,subset=c(SES=="Mid")))
abline(lm(trials_attempted ~age,data=data,subset=c(SES=="Low")),col="red")
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
points(data$age[data$SES=="Mid"], data$pc[data$SES=="Mid"],pch=20,col="red")
points(data$age[data$SES=="Low"], data$pc[data$SES=="Low"],pch=20,col="black")
abline(lm(pc~age,data=data,subset=c(SES=="Low")))
abline(lm(pc~age,data=data,subset=c(SES=="Mid")),col="red")
text(4,0.45,"Higher SES",col="red")
text(4,0.4,"Lower SES")
dev.off()


pdf("../results/rt_age.pdf",width=5,height=5)
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(log(rt_corr)~age,data=data,pch=20,xlab="Age (years)",ylab='Percent correct',type="n")
points(data$age[data$SES=="Mid"], log(data$rt_corr[data$SES=="Mid"]),pch=20,col="red")
points(data$age[data$SES=="Low"], log(data$rt_corr[data$SES=="Low"]),pch=20,col="black")
abline(lm(log(rt_corr)~age,data=data,subset=c(SES=="Low")))
abline(lm(log(rt_corr)~age,data=data,subset=c(SES=="Mid")),col="red")
text(4,8.2,"Higher SES",col="red")
text(4,8.15,"Lower SES")
dev.off()


myjitter=jitter(as.numeric(as.factor(data$type)))
pdf("../results/pc_type.pdf",width=5,height=5)
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(myjitter, data$pc,pch=20,ylim=c(0,1),xaxt="n",xlab="Test",ylab="Proportion correct",type="n")
axis(1,at=c(1,2),labels=c("Imperative","Question"))
lines(c(-1,3),c(.5,.5),lty=3,col="gray")
points(myjitter[data$SES=="Mid"], data$pc[data$SES=="Mid"],pch=20,col="red")
points(myjitter[data$SES=="Low"], data$pc[data$SES=="Low"],pch=20,col="black")
text(1.5,0,"Higher SES",col="red")
text(1.5,0.05,"Lower SES")
dev.off()

#clearly no effect in RT
#plot(myjitter, log(data$rt_corr),pch=20,xaxt="n",xlab="Test",ylab="TR (ms)")
#axis(1,at=c(1,2),labels=c("Imperativo","Pregunta"))



myjitter=jitter(rep(1,length(data$pc_easy)))
myjitter=c(myjitter,myjitter+0.2,myjitter+0.4)
pdf("../results/pc_diff.pdf")
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(myjitter, c(data$pc_easy,data$pc_mod,data$pc_diff),pch=20,ylim=c(0,1),xlab="Frequency group",ylab="Proportion correct",xaxt="n",type="n",xlim=c(0.95,1.45))
	lines(c(-1,3),c(.5,.5),lty=3,col="gray")
axis(1,at=c(1,1.2,1.4),labels=c("Highest","Moderate","Lowest"))
points(myjitter[data$SES=="Mid"], c(data$pc_easy[data$SES=="Mid"], data$pc_mod[data$SES=="Mid"], data$pc_diff[data$SES=="Mid"]),pch=20,col="red")

points(c(1.05,1.235, 1.45),c(mean(data$pc_easy[data$SES=="Mid"],na.rm=T), mean(data$pc_mod[data$SES=="Mid"],na.rm=T), mean(data$pc_diff[data$SES=="Mid"],na.rm=T)),pch=3,cex=3,col="red")

points(myjitter[data$SES=="Low"], c(data$pc_easy[data$SES=="Low"], data$pc_mod[data$SES=="Low"], data$pc_diff[data$SES=="Low"]),pch=20,col="black")
points(c(1.05,1.235, 1.45),c(mean(data$pc_easy[data$SES=="Low"],na.rm=T), mean(data$pc_mod[data$SES=="Low"],na.rm=T), mean(data$pc_diff[data$SES=="Low"],na.rm=T)),pch=3,cex=3,col="black")

text(1.2,0.1,"Higher SES",col="red")
text(1.2,0.05,"Lower SES")

dev.off()


pdf("../results/pc_lex.pdf")
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(myjitter, c(data$pc_noun,data$pc_verb,data$pc_adj),pch=20,ylim=c(0,1),xlab="Lexical category",ylab="Proportion correct",xaxt="n",type="n",xlim=c(0.95,1.45))
	lines(c(-1,3),c(.5,.5),lty=3,col="gray")
axis(1,at=c(1,1.2,1.4),labels=c("Noun","Verb","Adjective"))
points(myjitter[data$SES=="Mid"], c(data$pc_noun[data$SES=="Mid"], data$pc_verb[data$SES=="Mid"], data$pc_adj[data$SES=="Mid"]),pch=20,col="red")

points(c(1.05,1.235, 1.45),c(mean(data$pc_noun[data$SES=="Mid"],na.rm=T), mean(data$pc_verb[data$SES=="Mid"],na.rm=T), mean(data$pc_adj[data$SES=="Mid"],na.rm=T)),pch=3,cex=3,col="red")

points(myjitter[data$SES=="Low"], c(data$pc_noun[data$SES=="Low"], data$pc_verb[data$SES=="Low"], data$pc_adj[data$SES=="Low"]),pch=20,col="black")
points(c(1.05,1.235, 1.45),c(mean(data$pc_noun[data$SES=="Low"],na.rm=T), mean(data$pc_verb[data$SES=="Low"],na.rm=T), mean(data$pc_adj[data$SES=="Low"],na.rm=T)),pch=3,cex=3,col="black")

text(1.2,0.1,"Higher SES",col="red")
text(1.2,0.05,"Lower SES")

dev.off()


pdf("../results/rt_diff.pdf")
par(mar=c(2.5,2.5,1,1)+0.1,mgp=c(1.5,0.5,0))
plot( (data $rt_easy)/1000,(data $rt_diff)/1000,pch=20,xlim=c(1,4.5),ylim=c(1,4.5),xlab="Response time to words with highest frequencies",ylab="Response time to words with lowest frequencies",type="n")
lines(c(-1,5),c(-1,5),lty=2)
points((data $rt_easy[data$SES=="Mid"])/1000,(data $rt_diff[data$SES=="Mid"])/1000,pch=20,col="red")
points((data $rt_easy[data$SES=="Low"])/1000,(data $rt_diff[data$SES=="Low"])/1000,pch=20,col="black")

text(4,1.2,"Higher SES",col="red")
text(4,1.05,"Lower SES")

dev.off()