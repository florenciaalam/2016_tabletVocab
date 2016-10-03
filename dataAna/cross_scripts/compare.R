read.table("../argentina_daycares/results/results.txt",header=T)->arg
arg$edMom <-7
arg$edMom[arg$SES=="Mid"] <-14
arg$edMom=jitter(arg$edMom)
arg$lang="monolingual"

read.table("../france/results/results.txt",header=T)->fr
fr$edMom=fr$edMom+12
fr$edMom[fr$edMom==20 & !is.na(fr$edMom)]<-jitter(fr$edMom[fr$edMom==20 & !is.na(fr$edMom)],.1)
arg=cbind("arg",arg[,c(names(arg)[names(arg) %in% names(fr)])])
names(arg)[1]<-"country"
fr=cbind("fr",fr[,c(names(arg)[names(arg) %in% names(fr)])])
names(fr)[1]<-"country"

all <- rbind(arg,fr)

all$l=ifelse(all$lang=="monolingual" | is.na(all$lang),1,0)
all$x=ifelse(all$country=="arg" & all$school=="cayetano",1,0)

#quick formula to get effect sizes -- need to check that it's OK
cbind(as.character(levels(all$school)),(aggregate(all$pc,by=list(all$school),mean,na.rm=T)$x-.5)/
aggregate(all$pc,by=list(all$school),sd,na.rm=T)$x)


cbind(aggregate(all$pc,by=list(all$country,all$l,all$x),mean,na.rm=T),(aggregate(all$pc,by=list(all$country,all$l,all$x),mean,na.rm=T)$x-.5)/
aggregate(all$pc,by=list(all$country,all$l,all$x),sd,na.rm=T)$x)

#GET AGES
aggregate(all$age,by=list(all$country,all$l,all$x),mean,na.rm=T)
aggregate(all$age,by=list(all$country,all$l,all$x),range,na.rm=T)


#jpeg("pc_pais.jpg",width=15,height=15,units="cm",res=300)
pdf("pc_eng.pdf",width=5,height=5)
par(mar=c(3.5,3.5,1,1)+0.1,mgp=c(0.5,0.5,0))
plot(pc ~ edMom,data=all,type="n",ylim=c(0.3,1),xlab="", ylab="",cex.lab=2,cex.axis=1.5)
lines(c(4,30),c(.5,.5),lty=3,col="black")
mtext("Maternal years of education",1,line=2,cex=2)
mtext("Percent correct",2,line=2,cex=2)

#mtext("Anos de educacion de la madre",1,line=2,cex=2)
#mtext(" ~                                             ",1,line=1.5,cex=2)
#mtext("                          /             ",1,line=1.2,cex=1)
#mtext("                          /             ",1,line=1.2,cex=1)
#mtext("Porcentaje correcto",2,line=2,cex=2)

points(pc ~ edMom,data=all,subset=c(country=="arg" & school=="pestalozzi"),pch=16,col="red",cex=1.5)
points(pc ~ edMom,data=all,subset=c(country=="arg"& school=="cayetano"),pch="+",col="blue",cex=1.5)
points(pc ~ edMom,data=all,subset=c(country=="fr"& (lang=="monolingual" & !is.na(lang))),pch=16,col="black",cex=1.5)
points(pc ~ edMom,data=all,subset=c(country=="fr" & (lang!="monolingual" | is.na(lang)) ),pch=16,col="gray",cex=1.5)
#points(pc ~ edMom,data=all,subset=c(country=="fr" & lang != "monolingual"),pch=20,col="red")

abline(lm(pc ~ edMom,data=all,subset=c(country=="arg")),xlim=c(5,15),col="purple",cex=6,lwd=4)
abline(lm(pc ~ edMom,data=all,subset=c(country=="fr" & (lang=="monolingual" & !is.na(lang)))),xlim=c(5,15),col="black",lwd=4)
abline(lm(pc ~ edMom,data=all,subset=c(country=="fr" & (lang!="monolingual" | is.na(lang)))),xlim=c(5,15),col="gray",lwd=4)
#abline(lm(pc ~ edMom,data=all,subset=c(country=="fr"& lang=="monolingual")))
#abline(lm(pc ~ edMom,data=all,subset=c(country=="fr"& lang!="monolingual")),col="red")

dev.off()

