# R analysis on word comprehension on Mandy's app
# Argentina

# Base  Alex Cristia alecristia@gmail.com 2016-04-25
# Last edit May 2016

# This script carries out the main analyed declared in the paper and prints out both figures included in the paper and also others not included there 

### PRELIMINARIES: LOAD PACKAGES, DECLARE FUNCTIONS, SET PARAMETERS
library(lme4)
library(car)
library(psych)

r2d<-function(r){(2*abs(r))/sqrt(1-abs(r)^2)}	

### END PRELIMINARIES

# Read in & create tables to be used later
read.table("../results/results.txt",header=T)->data
data$age.c=data$age-mean(data$age,na.rm=T)
data$langdummy = data$lang=="monolingual"
data$langdummy[is.na(data$langdummy)]<-F #LAIA, I think there is an issue with the bilingual category -- there should be an "other" (not mono, not bi)

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
sink("../results/stats_results.txt")
print(date())
print(dim(data))
print(summary(data))



print("# ** planned analysis 1 **	total number of trials completed")
#print(summary(lm(trials_completed ~ed+age.c,data=data))) #no difference across groups, marginal ages 
print(summary(lm(trials_completed ~ed+age.c+langdummy,data=data))) #all NS 

#plot(trials_completed ~age.c,data=data)

print("trials_completed, mean & SD")
print(cbind(c("Higher","Lower"),round(cbind(
aggregate(data $trials_completed ,by=list(data$ed),mean)$x,
aggregate(data $trials_completed ,by=list(data$ed),sd)$x),
	3)))
	
print("trials_completed, range")
print(aggregate(data $trials_completed ,by=list(data$ed),range))


print("# ** planned analysis 2 **	total number of trials attempted")
#summary(lm(trials_attempted ~ed+age.c,data=data)) #no difference across groups, sign fx of age 
summary(lm(trials_attempted ~ed+age.c+langdummy,data=data)) #no difference across groups, sign fx of age 

print("trials_attempted, mean & SD")
print(cbind(c("Higher","Lower"),round(cbind(
aggregate(data $trials_attempted ,by=list(data$ed),mean)$x,
aggregate(data $trials_attempted ,by=list(data$ed),sd)$x),
	3)))

print("trials_attempted, range")
print(aggregate(data $trials_attempted ,by=list(data$ed),range))

print("trials_attempted, distribution")
print(table(data $trials_attempted,data$ed))

print("# ** planned analysis 3 **	%correct & RT per group")
#print(summary(lm((pc - .5)~ed+age.c,data=data)))  #difference in percent correct by age but not ed
#print(summary(lm(log(rt_corr)~ed+age.c,data=data)))  #no difference in RT

##### ATTENTION!!! THIS SECTION IS IN PROGRESS!!!
mod1=glm(pc~ed+age.c+langdummy,data=data,family=binomial,weight= trials_attempted) 
mod2=glm(pc~edMom+age.c+langdummy,data=data,family=binomial,weight= trials_attempted) 
##### ATTENTION!!! END SECTION IN PROGRESS!!!

print(summary(lm((pc - .5)~ed+age.c+langdummy,data=data)))  #difference in percent correct by age and language, but not ed
print(summary(lm(log(rt_corr)~ed+age.c+langdummy,data=data)))  #no difference in RT


print(summary(lm((pc - .5)~ed+age.c+langdummy+school,data=data)))  #difference in percent correct by age and language, but not ed
print(summary(lm(log(rt_corr)~ed+age.c+langdummy+school,data=data)))  #no difference in RT


print("# ** not planned **	RT per group * corr")
mixedRTr=lmer(log(rt) ~ ed*resp+age.c + langdummy + (1|id),data= rtstackcor)  #marginal age & sig difficulty, but no ed & no interaction
print(Anova(mixedRTr))
summary(mixedRTr)


print("# ** analysis 4 **	%correct & RT per group * difficulty (check - not sure if we declared it)
")
#mixedPCdif=lmer((pc - .5)~ed*difficulty+age.c + (1|id),data= pcstackdif) # difficulty, NO age, no interaction
#print(Anova(mixedPCdif))

mixedPCdif=lmer((pc - .5)~ed*difficulty+age.c + langdummy + (1|id),data= pcstackdif) #age & difficulty & langdummy, no interaction
print(Anova(mixedPCdif))
print(summary(mixedPCdif))

mixedPCdif=lmer((pc - .5)~ed*difficulty+age.c + langdummy + school + (1|id),data= pcstackdif) #age & difficulty & langdummy, no interaction
print(Anova(mixedPCdif))


#mixedRTdif=lmer(log(rt) ~ ed*difficulty+age.c + (1|id),data= rtstackdif)  #marginal age & sig difficulty, but no ed & no interaction
#print(Anova(mixedRTdif))

mixedRTdif=lmer(log(rt) ~ ed*difficulty+age.c + langdummy + (1|id),data= rtstackdif)  #marginal age & sig difficulty, but no ed & no interaction
print(Anova(mixedRTdif))
print(summary(mixedRTdif))

mixedRTdif=lmer(log(rt) ~ ed*difficulty+age.c + langdummy + school + (1|id),data= rtstackdif)  #marginal age & sig difficulty, but no ed & no interaction
print(Anova(mixedRTdif))



print("# ** analysis 5 **	compare different conceptualizations of ed (maternal education, etc.)")
print("##TO DO")

print("# ** analysis ** Differences by word type")
#mixedPClex=lmer((pc - .5)~ed* lexCat +age.c + (1|id),data= pcstacklex) #age & lex cat, BUT no ed nor ed*lex interaction
#print(Anova(mixedPClex))

mixedPClex=lmer((pc - .5)~ed* lexCat +age.c + langdummy + (1|id),data= pcstacklex) #age & lex cat & dummylang, BUT no ed nor ed*lex interaction
print(Anova(mixedPClex))
print(summary(mixedPClex))

mixedPClex=lmer((pc - .5)~ed* lexCat +age.c + langdummy + school + (1|id),data= pcstacklex) #age & lex cat & dummylang, BUT no ed nor ed*lex interaction
print(Anova(mixedPClex))

#mixedRTlex=lmer(log(rt) ~ ed* lexCat +age.c + (1|id),data= rtstacklex)  # lex cat, no ed or age and no interaction
#print(Anova(mixedRTlex))

mixedRTlex=lmer(log(rt) ~ ed* lexCat +age.c + langdummy+ (1|id),data= rtstacklex)  # lex cat & langdummy, no ed or age and no interaction
print(Anova(mixedRTlex))
print(summary(mixedRTlex))

mixedRTlex=lmer(log(rt) ~ ed* lexCat +age.c + langdummy + school + (1|id),data= rtstacklex)  # lex cat & langdummy, no ed or age and no interaction
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
	print(paste(thisdv,"d=",r2d(cor.test(data $trials_completed , data $edMom, method="spearman")$est),"\n","r=",cor.test(data $trials_completed , data $edMom, method="spearman")$est))
}



sink()
source("2B_genAllFigs.R")
