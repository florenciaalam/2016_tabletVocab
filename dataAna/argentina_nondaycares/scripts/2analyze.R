# R analysis on word comprehension on Mandy's app
# Argentina

# Base  Alex Cristia alecristia@gmail.com 2016-04-25
# Last edit May 2016

# This script carries out the main analyed declared in the paper and prints out both figures included in the paper and also others not included there 

### PRELIMINARIES: LOAD PACKAGES, DECLARE FUNCTIONS, SET PARAMETERS
library(lme4)
library(car)
library(psych)

cohensd<-function(dv,iv){
	means=aggregate(dv,by=list(iv),mean,na.rm=T)
	sds=aggregate(dv,by=list(iv),sd,na.rm=T)
	(means[1,2]-means[2,2])/sqrt(sum(sds[1:2,2])/2)
	}
	
d2r<-function(d){sqrt( ( d ^2 )/(d ^2 + 4))}	

### END PRELIMINARIES

# Read in & create tables to be used later
read.table("../results/results.txt",header=T)->data
data$type=substr(data$profile,nchar(as.character(data$profile)),nchar(as.character(data$profile)))
data$type<-ifelse(data$type=="g","preg","imp")
data$age.c=data$age-mean(data$age,na.rm=T)

pcstackdif=cbind(data[,c("id","SES","age.c")], stack(data[,c("pc_easy","pc_mod","pc_diff")]))
names(pcstackdif)[4:5]<-c("pc","difficulty")

rtstackdif=cbind(data[,c("id","SES","age.c")], stack(data[,c("rt_easy","rt_mod","rt_diff")]))
names(rtstackdif)[4:5]<-c("rt","difficulty")

pcstacklex=cbind(data[,c("id","SES","age.c")], stack(data[,c("pc_noun","pc_adj","pc_verb")]))
names(pcstacklex)[4:5]<-c("pc","lexCat")

rtstacklex=cbind(data[,c("id","SES","age.c")], stack(data[,c("rt_noun","rt_adj","rt_verb")]))
names(rtstacklex)[4:5]<-c("rt","lexCat")

rtstackcor=cbind(data[,c("id","SES","age.c")], stack(data[,c("rt_corr","rt_incorr")]))
names(rtstackcor)[4:5]<-c("rt","resp")


#CHECKS
#boxplot(data$age~data$type*data$SES)
#need to add age to the models for sure

### 	ANALYSES START
sink("../results/allresults.txt")
print(date())
print(dim(data))
print(summary(data))

print("CHECK EFFECT OF INSTRUCTIONS")

# ** check ** effect of instructions
print(summary(lm((pc - .5)~type*SES+age.c,data=data)))
#boxplot(data$pc~data$type*data$SES)

#hist(log(data$rt_corr))#log looks ND
print(summary(lm(log(rt_corr)~type*SES+age.c,data=data)))
#boxplot(log(data$rt_corr) ~data$type*data$SES)

print("# ** planned analysis 1 **	total number of trials completed")
print(summary(lm(trials_completed ~SES+age.c,data=data))) #no difference across groups, ages 

print("trials_completed, mean & SD")
print(cbind(c("Low","Mid"),round(cbind(
aggregate(data $trials_completed ,by=list(data$SES),mean)$x,
aggregate(data $trials_completed ,by=list(data$SES),sd)$x),
	3)))
	
print("trials_completed, range")
print(aggregate(data $trials_completed ,by=list(data$SES),range))


print("# ** planned analysis 2 **	total number of trials attempted")
summary(lm(trials_attempted ~SES+age.c,data=data)) #no difference across groups, ages 

print("trials_attempted, mean & SD")
print(cbind(c("Low","Mid"),round(cbind(
aggregate(data $trials_attempted ,by=list(data$SES),mean)$x,
aggregate(data $trials_attempted ,by=list(data$SES),sd)$x),
	3)))

print("trials_attempted, range")
print(aggregate(data $trials_attempted ,by=list(data$SES),range))

print("trials_attempted, distribution")
print(table(data $trials_attempted,data$SES))

print("# ** planned analysis 3 **	%correct & RT per group")
print(summary(lm((pc - .5)~SES+age.c,data=data)))  #difference in percent correct
print(summary(lm(log(rt_corr)~SES+age.c,data=data)))  #no difference in RT

print("# ** not planned **	RT per group * corr")
mixedRTr=lmer(log(rt) ~ SES*resp+age.c  + (1|id),data= rtstackcor)  #marginal age & sig difficulty, but no ed & no interaction
print(Anova(mixedRTr))
summary(mixedRTr)


print("# ** analysis 4 **	%correct & RT per group * difficulty (check - not sure if we declared it)
")
mixedPCdif=lmer((pc - .5)~SES*difficulty+age.c + (1|id),data= pcstackdif) #SES, age & difficulty, AND interaction
print(Anova(mixedPCdif))
print(summary(mixedPCdif))

mixedRTdif=lmer(log(rt) ~ SES*difficulty+age.c + (1|id),data= rtstackdif)  #age & difficulty, but no interaction
print(Anova(mixedRTdif))
print(summary(mixedRTdif))

print("# ** analysis 5 **	compare different conceptualizations of SES (maternal education, etc.)")
print("##TO DO")

print("# ** analysis ** Differences by word type")
mixedPClex=lmer((pc - .5)~SES* lexCat +age.c + (1|id),data= pcstacklex) #SES, age & lex cat, AND SES*lex interaction
print(Anova(mixedPClex))

mixedRTlex=lmer(log(rt) ~ SES* lexCat +age.c + (1|id),data= rtstacklex)  #age & lex cat, but no interaction
print(Anova(mixedRTlex))
print(summary(mixedRTlex))


print("# ** planned analysis **	Internal validity with cronbach alpha, using split trial") 
# (we had also planned for a retest reliability but we didn't include those extra 13 trials)
# Santos, J. R. A. (1999). Cronbachs alpha: A tool for asedsing the reliability of scales. Journal of Extension, 37, 1–5.
print(alpha(cbind(data$pc_1,data$pc_2)))
print(alpha(cbind(data$rt_1,data$rt_2)))
# I'm getting an error in the Span data - not sure why!
#print(alpha(cbind(data$pc_1[!is.na(data$pc_1) & !is.na(data$pc_2)],data$pc_2[!is.na(data$pc_1) & !is.na(data$pc_2)])))
cor.test(data$pc_1,data$pc_2)
for(thisprof in levels(data$profile) ) {print(thisprof) ; print(cor.test(data$pc_1[data$profile==thisprof],data$pc_2[data$profile==thisprof]))}


# ** minor analyses **	
print("# Test potential presence of side bias")
print(table(data$bias))  #19!!! out of 51 kids have a bias according to a crude measure that should be improved!!


print("# ** NUTSHELL **	Effect sizes")
dvs=c("trials_completed","trials_attempted","pc","rt_corr")
for(thisdv in dvs){
	print(paste(thisdv,"d=",cohensd(data[,thisdv], data $SES),"\n","r=",d2r(cohensd(data[,thisdv], data $SES))))
}


sink()
source("2B_genAllFigs.R")
