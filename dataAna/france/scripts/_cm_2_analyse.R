# Analyse en R pour une étude sur la reconnaissance et l'apprentissage des mots 2015/2016
# mené sur Mandy -- SECTION ON VOCAB

# Base  Alex Cristia alecristia@gmail.com 2016-04-21
# Dernières modifs Charlotte Maniel 2016-07-07

#IMPORTANT: uncomment the next line if important changes are made to table data, questionnaire, correspondance tables
#source("_scripts/_createDataSheets.R") #combines csv's together, stores tablet data, questionnaire, and correspondance table in _results

#IMPORTANT: uncomment the next line if important changes are made to the data pre-processing
#source("_scripts/_cm_2_new_preprocess.R") #calculates performance and RT per child, combines with other personal data

### PRELIMINARIES: LOAD PACKAGES, DECLARE FUNCTIONS, SET PARAMETERS
library(lme4)
library(car)
library(psych)

r2d<-function(r){(2*abs(r))/sqrt(1-abs(r)^2)}	

### END PRELIMINARIES

# Read in & create tables to be used later
read.table("~/Dropbox/tabletAna/_results/cm_new_results.txt",header=T)->data
data$age.c=data$age-mean(data$age,na.rm=T)
data$langdummy = data$lang=="monolingual"
data$langdummy[is.na(data$langdummy)]<-F 
data$lang2 = ifelse(data$lang=="monolingual","monolingual","other") 
data$ed2=ifelse(data$edPar>7,"higher","lower")

rtstackcor=cbind(data[,c("id","ed","age.c","langdummy","school")], stack(data[,c("rt_corr","rt_incorr")]))
names(rtstackcor)[6:7]<-c("rt","resp")


pcstackdif=cbind(data[,c("id","ed","age.c","langdummy","school")], stack(data[,c("pc_easy","pc_mod","pc_diff")]))
names(pcstackdif)[6:7]<-c("pc","difficulty")

rtstackdif=cbind(data[,c("id","ed","age.c","langdummy","school")], stack(data[,c("rt_easy","rt_mod","rt_diff")]))
names(rtstackdif)[6:7]<-c("rt","difficulty")

pcstacklex=cbind(data[,c("id","ed","age.c","langdummy","school")], stack(data[,c("pc_noun","pc_adj","pc_verb")]))
names(pcstacklex)[6:7]<-c("pc","lexCat")

rtstacklex=cbind(data[,c("id","ed","age.c","langdummy","school")], stack(data[,c("rt_noun","rt_adj","rt_verb")]))
names(rtstacklex)[6:7]<-c("rt","lexCat")


#CHECKS
#boxplot(data$age~data$ed)
#need to add age to the models for sure

### 	ANALYed START
sink("~/Dropbox/tabletAna/_results/cm_new_stats_results.txt")
print(date())
print(dim(data))
print(summary(data))


print("# ** planned ANALYSIS 1 **	total number of trials completed")
#print(summary(lm(trials_completed ~ed+age.c,data=data))) 
print(summary(lm(trials_completed ~ed+age.c+langdummy,data=data))) 

#plot(trials_completed ~age.c,data=data)

print("trials_completed, mean & SD")
print(cbind(c("Higher","Lower"),round(cbind(
aggregate(data $trials_completed ,by=list(data$ed),mean)$x,
aggregate(data $trials_completed ,by=list(data$ed),sd)$x),
	3)))
	
print("trials_completed, range")
print(aggregate(data $trials_completed ,by=list(data$ed),range))


print("# ** planned ANALYSIS 2 **	total number of trials attempted")
#summary(lm(trials_attempted ~ed+age.c,data=data)) 
summary(lm(trials_attempted ~ed+age.c+langdummy,data=data))  

print("trials_attempted, mean & SD")
print(cbind(c("Higher","Lower"),round(cbind(
aggregate(data $trials_attempted ,by=list(data$ed),mean)$x,
aggregate(data $trials_attempted ,by=list(data$ed),sd)$x),
	3)))

print("trials_attempted, range")
print(aggregate(data $trials_attempted ,by=list(data$ed),range))

print("trials_attempted, distribution")
print(table(data $trials_attempted,data$ed))

print("# ** planned ANALYSIS 3 **	%correct & RT per group")
#print(summary(lm((pc - .5)~ed+age.c,data=data)))  
#print(summary(lm(log(rt_corr)~ed+age.c,data=data))) 

##### ATTENTION!!! THIS SECTION IS IN PROGRESS!!!
mod1=glm(pc~ed+age.c+langdummy,data=data,family=binomial,weight= trials_attempted) 
mod2=glm(pc~edMom+age.c+langdummy,data=data,family=binomial,weight= trials_attempted) 
##### ATTENTION!!! END SECTION IN PROGRESS!!!

print(summary(lm((pc - .5)~ed+age.c+langdummy,data=data)))  
print(summary(lm(log(rt_corr)~ed+age.c+langdummy,data=data)))  


print(summary(lm((pc - .5)~ed+age.c+langdummy+school,data=data)))  
print(summary(lm(log(rt_corr)~ed+age.c+langdummy+school,data=data)))  


print("# ** ANALYSIS 3bis **	PC & RT per group (edPar)")
print(summary(lm((pc - .5)~ed2+age.c+langdummy,data=data))) #edPar as a categorical variable
print(summary(lm(log(rt_corr)~ed2+age.c+langdummy,data=data))) #edPar as a categorical variable


print("# ** not planned **	RT per group * corr")
mixedRTr=lmer(log(rt) ~ ed*resp+age.c + langdummy + (1|id),data= rtstackcor)  
print(Anova(mixedRTr))
summary(mixedRTr)


print("# ** ANALYSIS 4 **	%correct & RT per group * difficulty (check - not sure if we declared it)
")
#mixedPCdif=lmer((pc - .5)~ed*difficulty+age.c + (1|id),data= pcstackdif) 
#print(Anova(mixedPCdif))

mixedPCdif=lmer((pc - .5)~ed*difficulty+age.c + langdummy + (1|id),data= pcstackdif) 
print(Anova(mixedPCdif))
print(summary(mixedPCdif))

mixedPCdif=lmer((pc - .5)~ed*difficulty+age.c + langdummy + school + (1|id),data= pcstackdif) 
print(Anova(mixedPCdif))


#mixedRTdif=lmer(log(rt) ~ ed*difficulty+age.c + (1|id),data= rtstackdif)  
#print(Anova(mixedRTdif))

mixedRTdif=lmer(log(rt) ~ ed*difficulty+age.c + langdummy + (1|id),data= rtstackdif)  
print(Anova(mixedRTdif))
print(summary(mixedRTdif))

mixedRTdif=lmer(log(rt) ~ ed*difficulty+age.c + langdummy + school + (1|id),data= rtstackdif)  
print(Anova(mixedRTdif))


print("# ** ANALYSIS 5 **	compare different conceptualizations of ed (maternal education, etc.)")
print("##TO DO")

print("# ** ANALYSIS 6 ** compare trials_completed, PC, RT as a fonction of sex")
print(summary(lm(trials_completed ~ed*sex+age.c+langdummy,data=data)))
print(summary(lm((pc - .5) ~ ed*sex + age,data=data)))
print(summary(lm(rt_corr ~ ed*sex + age,data=data)))

print("# ** analysis ** Differences by word type")
#mixedPClex=lmer((pc - .5)~ed* lexCat +age.c + (1|id),data= pcstacklex) 
#print(Anova(mixedPClex))

mixedPClex=lmer((pc - .5)~ed* lexCat +age.c + langdummy + (1|id),data= pcstacklex) 
print(Anova(mixedPClex))
print(summary(mixedPClex))

mixedPClex=lmer((pc - .5)~ed* lexCat +age.c + langdummy + school + (1|id),data= pcstacklex) 
print(Anova(mixedPClex))

#mixedRTlex=lmer(log(rt) ~ ed* lexCat +age.c + (1|id),data= rtstacklex)  
#print(Anova(mixedRTlex))

mixedRTlex=lmer(log(rt) ~ ed* lexCat +age.c + langdummy+ (1|id),data= rtstacklex)  
print(Anova(mixedRTlex))
print(summary(mixedRTlex))

mixedRTlex=lmer(log(rt) ~ ed* lexCat +age.c + langdummy + school + (1|id),data= rtstacklex)  
print(Anova(mixedRTlex))

print("# ** planned analysis **	Internal validity with cronbach alpha, using split trial") 
# (we had also planned for a retest reliability but we didn't include those extra 13 trials)
# Santos, J. R. A. (1999). Cronbachs alpha: A tool for asedsing the reliability of scales. Journal of Extension, 37, 1–5.
print(alpha(cbind(data$pc_1,data$pc_2)))
print(alpha(cbind(data$rt_1,data$rt_2)))


# ** minor analyses **	
print("# Test potential presence of side bias")
print(table(data$bias))  #19!!! out of 51 kids have a bias according to a crude measure that should be improved!!


print("# ** NUTSHELL **	Effect sizes")
dvs=c("trials_completed","trials_attempted","pc","rt_corr")
for(thisdv in dvs){
  print(paste(thisdv,"d=",r2d(cor.test(data [,thisdv] , data $edMom, method="spearman")$est),"\n","r=",cor.test(data [,thisdv] , data $edMom, method="spearman")$est))
}

print("# ** CDI analyses **	")
print(summary(lm(CDI_score~ed+age.c+langdummy,data=data)))

######  PLOTS   ###### 

#trials_completed ~ age & SES
plot(trials_completed ~ age, data=data,xlab="",ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1, type="n", ylim=c(0,42))
mtext("Age (ans)",1,line=2.5,cex=2)
mtext("Essais complétés",2,line=2.5,cex=2)
abline(h=41,lty=2,col="slategrey")
points(trials_completed ~age, data=data, subset=c(lang=="monolingual"),pch=25,col="red",bg="red")
points(trials_completed ~age, data=data, subset=c(lang2=="other"),pch=20,col="black",bg="black")
abline(lm(trials_completed ~age, data=data, subset=c(lang=="monolingual")),col="red")
abline(lm(trials_completed ~age, data=data, subset=c(lang2=="other")),col="black")

plot(trials_completed ~ edMom, data=data,xlab="",ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1, type="n", ylim=c(0,42))
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("Essais complétés",2,line=2.5,cex=2)
abline(h=41,lty=2,col="slategrey")
points(trials_completed ~edMom, data=data, subset=c(lang=="monolingual"),pch=25,col="red",bg="red")
points(trials_completed ~edMom, data=data, subset=c(lang2=="other"),pch=20,col="black",bg="black")
abline(lm(trials_completed ~edMom, data=data, subset=c(lang=="monolingual")),col="red")
abline(lm(trials_completed ~edMom, data=data, subset=c(lang2=="other")),col="black")


#boxplot and plot RT ~ SES (edMom or edPar)
boxplot(rt_corr ~ ed, data=data)
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
mtext("SES (Ed mère)",1,line=2.5,cex=2)
mtext("RT corr (ms)",2,line=2.5,cex=2)

boxplot(rt_corr ~ ed2, data=data)
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
mtext("SES (Ed par)",1,line=2.5,cex=2)
mtext("RT corr (ms) ",2,line=2.5,cex=2)

#plot PC and RT ~ edMom & creches
plot(pc ~ edMom,data=data,type="n") 
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(pc ~ edMom,data=data,xlab="",ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1)
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("Réponses correctes (%)",2,line=2.5,cex=2)
points(pc ~ edMom,data=data,subset=c(school=="massena"),pch=20,col="red") 
points(pc ~ edMom,data=data,subset=c(school=="fautrier"),pch=20,col="blue") 
points(pc ~ edMom,data=data,subset=c(school=="dumeril"),pch=20,col="gray") 
abline(h=0.5,lty=2,col="slategrey")
abline(lm(pc~edMom,data=data),col="black")

plot(rt_corr ~ edMom,data=data,type="n") 
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(rt_corr ~ edMom,data=data,xlab="",ylab="",pch=20,cex.lab=1,cex=3,cex.axis=1)
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("RT (ms)",2,line=2.5,cex=2)
points(rt_corr ~ edMom,data=data,subset=c(school=="massena"),pch=16,col="red") 
points(rt_corr ~ edMom,data=data,subset=c(school=="fautrier"),pch=16,col="blue") 
points(rt_corr ~ edMom,data=data,subset=c(school=="dumeril"),pch=16,col="gray")
abline(lm(rt_corr~edMom,data=data),col="black")

#plot RT frequent words ~ RT less frequent & creches
plot(rt_diff ~ rt_easy,data=data,xlab="",ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1)
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
mtext("RT mots faciles (ms)",1,line=2.5,cex=2)
mtext("RT mots difficiles (ms)",2,line=2.5,cex=2)
points(rt_diff ~ rt_easy,data=data,subset=c(school=="massena"),pch=20,col="red") 
points(rt_diff ~ rt_easy,data=data,subset=c(school=="fautrier"),pch=20,col="blue") 
points(rt_diff ~ rt_easy,data=data,subset=c(school=="dumeril"),pch=20,col="gray") 
lines(c(0,5000),c(0,5000),lty=2)
abline(lm(rt_diff ~ rt_easy,data=data),col="black")

#plot RT frequent words ~ RT less frequent words & SES
plot(rt_diff ~ rt_easy,data=data,xlab="",ylab="",type="n",ylim=c(1000,3800),xlim=c(1000,3800)) 
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
mtext("RT mots faciles (ms)",1,line=2.5,cex=2)
mtext("RT mots difficiles (ms)",2,line=2.5,cex=2)
points(rt_diff ~ rt_easy,data=data,subset=c(ed=="higher"),pch=20,col="deepskyblue3") 
abline(lm(rt_diff ~  rt_easy,data=data, subset=c(ed=="higher" & !is.na(lang))),col="deepskyblue3") 
points(rt_diff ~ rt_easy,data=data,subset=c(ed=="lower"),pch=22,col="sienna1")
abline(lm(rt_diff ~  rt_easy,data=data, subset=c(ed=="lower" & !is.na(lang))),col="sienna1") 
lines(c(0,5000),c(0,5000),lty=2)

#plot RT frequent words ~ RT less frequent words & lang
plot(rt_diff ~ rt_easy,data=data,xlab="",ylab="",type="n",ylim=c(1000,4000),xlim=c(1000,4000)) 
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
mtext("RT mots faciles (ms)",1,line=2.5,cex=2)
mtext("RT mots difficiles (ms)",2,line=2.5,cex=2)
points(rt_diff ~ rt_easy,data=data,subset=c(lang=="monolingual"),pch=25,col="red",bg="red")
abline(lm(rt_diff ~ rt_easy, data=data, subset=c(lang=="monolingual")), col="red")
points(rt_diff ~ rt_easy,data=data,subset=c(lang2=="other"),pch=20,col="black")
abline(lm(rt_diff ~ rt_easy, data=data, subset=c(lang2=="other")), col="black")
lines(c(0,5000),c(0,5000),lty=2)


#Plot PC and RT ~ age, difficulty level --> moche mais prend en compte le fait que chaque enfant a 3 points
plot(pc_easy ~ age, data=data,xlab="",ylab="",type="n",xlim=c(2,3.5), ylim=c(0,1))
mtext("Age (années)",1,line=2.5,cex=2)
mtext("Réponses correctes (%)",2,line=2.5,cex=2)
points(pc_easy ~ age,data=data,pch=20,col="forestgreen") 
abline(lm(pc_easy ~ age, data=data),col="forestgreen") 
points(pc_mod ~ age,data=data,pch=20,col="darkorange") 
abline(lm(pc_mod ~ age,data=data),col="darkorange") 
points(pc_diff ~ age,data=data,pch=20,col="firebrick2") 
abline(lm(pc_diff ~ age,data=data),col="firebrick2") 
abline(h=0.5,lty=2,col="slategrey")

plot(rt_easy ~ age, data=data,xlab="",ylab="",type="n")
mtext("Age (années)",1,line=2.5,cex=2)
mtext("RT corr (ms)",2,line=2.5,cex=2)
points(rt_easy ~ age ,data=data,pch=20,col="forestgreen") 
abline(lm(rt_easy ~ age,data=data),col="forestgreen")
points(rt_mod ~ age,data=data,pch=20,col="darkorange") 
abline(lm(rt_mod ~ age,data=data),col="darkorange")
points(rt_diff ~ age,data=data,pch=20,col="firebrick2")
abline(lm(rt_diff ~ age,data=data),col="firebrick2")

#Plot PC and RT ~ edMom, difficulty level --> moche mais prend en compte le fait que chaque enfant a 3 points
plot(pc_easy ~ edMom, data=data,xlab="",ylab="",type="n")
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("Réponses correctes (%)",2,line=2.5,cex=2)
points(pc_easy ~ edMom,data=data,pch=20,col="forestgreen") 
abline(lm(pc_easy ~ edMom, data=data),col="forestgreen") 
points(pc_mod ~ edMom,data=data,pch=20,col="darkorange") 
abline(lm(pc_mod ~ edMom,data=data),col="darkorange") 
points(pc_diff ~ edMom,data=data,pch=20,col="firebrick2") 
abline(lm(pc_diff ~ edMom,data=data),col="firebrick2") 
abline(h=0.5,lty=2,col="slategrey")

plot(rt_easy ~ edMom, data=data,xlab="",ylab="",type="n")
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("RT (ms)",2,line=2.5,cex=2)
points(rt_easy ~ edMom ,data=data,pch=20,col="forestgreen") 
abline(lm(rt_easy ~ edMom,data=data),col="forestgreen")
points(rt_mod ~ edMom,data=data,pch=20,col="darkorange") 
abline(lm(rt_mod ~ edMom,data=data),col="darkorange")
points(rt_diff ~ edMom,data=data,pch=20,col="firebrick2")
abline(lm(rt_diff ~ edMom,data=data),col="firebrick2")

#Plot PC and RT ~ age.c & word type (or lexcat)
plot(pc ~ age, data=data,xlab="",ylab="",type="n")
mtext("Age (années)",1,line=2.5,cex=2)
mtext("Réponses correctes (%)",2,line=2.5,cex=2)
points(pc ~ age,data=data,subset=c(dif$wordtype=="noun"),pch=20,col="red") 
abline(lm(pc ~ age,subset=c(dif$wordtype=="noun"),data=data), col="red")
points(pc ~ age,data=data,subset=c(dif$wordtype=="adj"),pch=20,col="blue") 
abline(lm(pc ~ age,subset=c(dif$wordtype=="adj"),data=data), col="blue")
points(pc ~ age,data=data,subset=c(dif$wordtype=="verb"),pch=20,col="black")
abline(lm(pc ~ age,subset=c(dif$wordtype=="verb"),data=data), col="black")
abline(h=0.5,lty=2,col="slategrey")

plot(rt_corr ~ age, data=data,xlab="",ylab="",type="n")
mtext("Age (années)",1,line=2.5,cex=2)
mtext("RT (ms)",2,line=2.5,cex=2)
points(rt_corr ~ age,data=data,subset=c(dif$wordtype=="noun"),pch=20,col="red") 
abline(lm(rt_corr ~ age,data=data,subset=c(dif$wordtype=="noun")),col="red")
points(rt_corr ~ age,data=data,subset=c(dif$wordtype=="adj"),pch=20,col="blue") 
abline(lm(rt_corr ~ age,data=data,subset=c(dif$wordtype=="adj")),col="blue")
points(rt_corr ~ age,data=data,subset=c(dif$wordtype=="verb"),pch=20,col="black") 
abline(lm(rt_corr ~ age,data=data,subset=c(dif$wordtype=="verb")),col="black")

#Plot PC and RT ~ edMom & word type (or lexcat)
plot(pc ~ edMom, data=data,xlab="",ylab="",type="n")
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("Réponses correctes (%)",2,line=2.5,cex=2)
points(pc ~ edMom,data=data,subset=c(dif$wordtype=="noun"),pch=20,col="red") 
abline(lm(pc ~ edMom,subset=c(dif$wordtype=="noun"),data=data), col="red")
points(pc ~ edMom,data=data,subset=c(dif$wordtype=="adj"),pch=20,col="blue") 
abline(lm(pc ~ edMom,subset=c(dif$wordtype=="adj"),data=data), col="blue")
points(pc ~ edMom,data=data,subset=c(dif$wordtype=="verb"),pch=20,col="black")
abline(lm(pc ~ edMom,subset=c(dif$wordtype=="verb"),data=data), col="black")
abline(h=0.5,lty=2,col="slategrey")

plot(rt_corr ~ edMom, data=data,xlab="",ylab="",type="n")
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("RT (ms)",2,line=2.5,cex=2)
points(rt_corr ~ edMom,data=data,subset=c(dif$wordtype=="noun"),pch=20,col="red") 
abline(lm(rt_corr ~ edMom,data=data,subset=c(dif$wordtype=="noun")),col="red")
points(rt_corr ~ edMom,data=data,subset=c(dif$wordtype=="adj"),pch=20,col="blue") 
abline(lm(rt_corr ~ edMom,data=data,subset=c(dif$wordtype=="adj")),col="blue")
points(rt_corr ~ edMom,data=data,subset=c(dif$wordtype=="verb"),pch=20,col="black") 
abline(lm(rt_corr ~ edMom,data=data,subset=c(dif$wordtype=="verb")),col="black")


#Plot PC and RT ~ edMom & lang
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(pc ~ edMom,data=data,xlab="",ylab="",pch=20,cex.lab=2,cex=3,cex.axis=1,type="n")
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("Réponses correctes (%)",2,line=2.5,cex=2)
points(pc ~ edMom,data=data,subset=c(lang=="monolingual" & !is.na(lang)),pch=25,bg="red",col="red") 
abline(lm(pc ~  edMom,subset=c(lang2=="monolingual" & !is.na(lang)),data=data),col="red") 
points(pc ~ edMom,data=data,subset=c(lang2=="other" & !is.na(lang)),pch=20,col="black")
abline(lm(pc ~  edMom,subset=c(lang2=="other" & !is.na(lang)),data=data),col="black") 
abline(h=0.5,lty=2,col="slategrey")

par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(rt_corr ~ edMom,data=data,xlab="",ylab="",pch=20,cex.lab=1,cex=3,cex.axis=1,type="n")
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("RT (ms)",2,line=2.5,cex=2)
points(rt_corr ~ edMom,data=data,subset=c(lang=="monolingual"),pch=25,bg="red",col="red") 
abline(lm(rt_corr ~  edMom,subset=c(lang=="monolingual" & !is.na(lang)),data=data),col="red")
points(rt_corr ~ edMom,data=data,subset=c(lang2=="other"),pch=20,col="black") 
abline(lm(rt_corr ~  edMom,subset=c(lang2=="other" & !is.na(lang)),data=data),col="black") 

#Plot PC and RT ~ edPar & lang
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(pc ~ edPar,data=data,xlab="",ylab="",pch=20,cex.lab=2,cex=3,cex.axis=1,type="n")
mtext("Ed Parents (années)",1,line=2.5,cex=2)
mtext("Réponses correctes (%)",2,line=2.5,cex=2)
points(pc ~ edPar,data=data,subset=c(lang=="monolingual" & !is.na(lang)),pch=25,bg="red",col="red") 
points(pc ~ edPar,data=data,subset=c(lang2=="other" & !is.na(lang)),pch=20,col="black")
abline(lm(pc ~  edPar,data=data),col="black") 
abline(h=0.5,lty=2,col="slategrey")

par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(rt_corr ~ edPar,data=data,xlab="",ylab="",pch=20,cex.lab=1,cex=3,cex.axis=1,type="n")
mtext("Ed Parents (années)",1,line=2.5,cex=2)
mtext("RT (ms)",2,line=2.5,cex=2)
points(rt_corr ~ edPar,data=data,subset=c(lang=="monolingual"),pch=25,bg="red",col="red") 
points(rt_corr ~ edPar,data=data,subset=c(lang2=="other"),pch=20,col="black") 
abline(lm(rt_corr ~  edPar,data=data),col="black") 

#Plot PC and RT ~ age & lang
par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(pc ~ age,data=data,xlab="",ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1,type="n")
mtext("Age (ans)",1,line=2.5,cex=2)
mtext("Réponses correctes (%)",2,line=2.5,cex=2)
points(pc ~ age,data=data,subset=c(lang=="monolingual"),pch=25,col="red",bg="red") 
abline(lm(pc ~  age,subset=c(lang=="monolingual" & !is.na(lang)),data=data),col="red",bg="red")
points(pc ~ age,data=data,subset=c(lang2=="other"),pch=16,col="black")
abline(lm(pc ~  age,subset=c(lang2=="other" & !is.na(lang)),data=data),col="black")
abline(h=0.5,lty=2,col="slategrey")

par(mar=c(4,4,1,1)+0.1,mgp=c(1.5,0.5,0))
plot(rt_corr ~ age,data=data,xlab="",ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1,type="n")
mtext("Age (ans)",1,line=2.5,cex=2)
mtext("RT (ms)",2,line=2.5,cex=2)
points(rt_corr ~ age,data=data,subset=c(lang=="monolingual"),pch=25,col="red",bg="red") 
abline(lm(rt_corr ~  age,subset=c(lang=="monolingual" & !is.na(lang)),data=data),col="red",bg="red")
points(rt_corr ~ age,data=data,subset=c(lang2=="other"),pch=16,col="black")
abline(lm(rt_corr ~  age,subset=c(lang2=="other" & !is.na(lang)),data=data),col="black")

##courbe PC and RT ~ sex
plot(pc ~ sex, data=data, xlab="", ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1,type="n")
mtext("Sexe",1,line=2.5,cex=2)
mtext("Réponses correctes (%)",2,line=2.5,cex=2)
abline(h=0.5,lty=2,col="slategrey")

plot(rt_corr ~ sex, data=data, xlab="", ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1,type="n")
mtext("Sexe",1,line=2.5,cex=2)
mtext("RT corr (ms)",2,line=2.5,cex=2)


##courbe RT ~ time    #rerun "_cm_1_new_preprocess.R" if needed
plot(data$object_touched_at ~ data$trial_number_session,xlab="",ylab="",ylim=c(1500,3000),pch=20,cex.lab=2,cex=2,cex.axis=1,type="n")
mtext("Essais",1,line=2.5,cex=2)
mtext("RT (ms)",2,line=2.5,cex=2)
abline(lm(data$object_touched_at ~ data$trial_number_session))


#courbe score CDI ~ edMom & age
plot(CDI_score ~ edMom, data=data, xlab="", ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1,type="n")
mtext("Ed mère (années)",1,line=2.5,cex=2)
mtext("Score CDI",2,line=2.5,cex=2)
points(CDI_score ~edMom, data=data, subset=c(lang=="monolingual"),pch=25,col="red",bg="red")
points(CDI_score ~edMom, data=data, subset=c(lang2=="other"),pch=20,col="black",bg="black")
abline(lm(CDI_score ~edMom, data=data),col="black")

plot(CDI_score ~ age, data=data, xlab="", ylab="",pch=20,cex.lab=2,cex=2,cex.axis=1,type="n")
mtext("Age (ans)",1,line=2.5,cex=2)
mtext("Score CDI",2,line=2.5,cex=2)
points(CDI_score ~age, data=data, subset=c(lang=="monolingual"),pch=25,col="red",bg="red")
points(CDI_score ~age, data=data, subset=c(lang2=="other"),pch=20,col="black",bg="black")
abline(lm(CDI_score ~age, data=data),col="black")

