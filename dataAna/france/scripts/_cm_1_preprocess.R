# Analyse en R pour une étude sur la reconnaissance et l'apprentissage des mots 2015/2016
# mené sur Mandy -- SECTION ON VOCAB

# Base  Alex Cristia alecristia@gmail.com 2016-04-21
# Dernières modifs Charlotte Maniel 2016-07-12


### PRELIMINARIES: LOAD PACKAGES, DECLARE FUNCTIONS, SET PARAMETERS
minTrialN=9 #MINIMUM USABLE TRIALS TO BE INCLUDED IN OVERALL ANALYSES 
minTrialNtype=4  #minimum amount of trials for each type   #notice that this means we will never look at crossing lexical type x difficulty level

extractVwithNA<-function(allkids,res_incl){
#add kids with no response data so that table can be square
#and then return the averaged parameter
 	names(res_incl)<-c("id","x")
	if(sum(!(allkids %in% as.character(res_incl$id)))>0){
	  nakids=cbind(as.character(allkids [!(allkids %in% as.character(res_incl$id))]),NA)
	  colnames(nakids)<-c("id","x")
	  res_incl=rbind(res_incl,nakids)
	} 
  res_incl = res_incl[order(res_incl $id),]		
  as.numeric(as.character(res_incl$x))
}

library(xlsx)


### END PRELIMINARIES

#*************************************************************************************************************************************
#### CSV DATA
## Read in the CSV
dirlist = dir(path="~/Dropbox/tabletAna/data")

data=NULL
for(thisdir in dirlist){
	dir(paste("~/Dropbox/tabletAna/data",thisdir,sep="/"),pattern='csv')->allcsv
	for(thisf in allcsv){
		print(thisf)
		read.csv(paste("~/Dropbox/tabletAna/data",thisdir,thisf,sep="/"))->thiscsv
		data =rbind(data,cbind(thisdir,thisf,thiscsv)) 
		}
}
dim(data)  
write.table(data,"~/Dropbox/tabletAna/data.txt",row.names=F,sep="\t")
#*************************************************************************************************************************************
sumtab=NULL
for(thisuuid in levels(data$uuid)) sumtab=rbind(sumtab,cbind(thisuuid,sum(data$uuid==thisuuid),data[data$uuid==thisuuid,c("subject_id","config_profile","session_started_at","thisdir","thisf")][1,]))
write.table(sumtab,"~/Dropbox/tabletAna/_results/cm_sumtabCROSSac.txt",row.names=F,quote=F,sep="\t")
#several IDs have two sets of data!!
names(table(sumtab$subject_id))[table(sumtab$subject_id)>1]

#*************************************************************************************************************************************

read.table("~/Dropbox/tabletAna/data.txt",header=T)-> data

#remove non-relevant data
data=data[grep("C",data$level_name),   ]

#clean up subject id
data$subject_id=factor(data$uuid)

#*$* THROUGOUT SCRIPT, WE WILL COMPOSE A RESULTS TABLE, STARTING FROM NOTHING:
results=NULL


#*$* add ID info to the results table
results$id=sort(data$subject_id[data$trial_number_session==1])

#*$* add backgound info to the results table
results$school = data$thisdir[data$trial_number_session==1][order(data$subject_id[data$trial_number_session==1])]
results$date = data$date[data$trial_number_session==1][order(data$subject_id[data$trial_number_session==1])]


#*$*profile
results$profile = data$config_profile[data$trial_number_session==1][order(data$subject_id[data$trial_number_session==1])]


#### STEP 1: CLEAN UP
## Take into acount only test trials 
data[-grep("Training", data $level_name),   ]-> data

#clean up names of objects
#data$object_asked=gsub("_.*","",data$object_asked)

dim(data) #4793 trials 
#why am I not getting the same number???


#### STEP 2: RESULTS COLLAPSING ACROSS TRIAL TYPES
#*$*trials completed
results$trials_completed=as.numeric(table(data$subject_id)) 

#hist(data$object_touched_at)
#hist(data$object_touched_at[data $object_touched_at<9000])

## Exclusion criteria
#maximum time to answer
data[data $object_touched_at<7000,]-> data

#dim(data) #4537 trials 

#*$* trials attempted
results$trials_attempted=as.numeric(table(data$subject_id))

#apply minimum amount of trials over the whole experiment
bbMinOK=names(table(data $subject_id)[table(data $subject_id)>minTrialN])
data[data $subject_id %in% bbMinOK,]-> data

#dim(data) #4529 trials


### ANALYSIS ACCURACY
#*$* percent correct (overall)
results$pc=extractVwithNA(levels(data$subject_id),	aggregate(data $correct,by=list(data $subject_id),mean))


###XXXXX###
#Calculate exclusion matrix  
exclude = table(data$subject_id,data$correct) < minTrialNtype   #Apply minimum number of trials 

### ANALYSIS RESPONSE TIMES 
results$rt_corr=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$correct==1],by=list(data $subject_id[data$correct==1]),median))
results$rt_corr[exclude[,"1"]]<-NA

results$rt_incorr=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$correct==0],by=list(data $subject_id[data$correct==0]),median))
results$rt_incorr[exclude[,"0"]]<-NA


#*$* for reliability calculations, re-do proportion correct & RT corr from odd and even trials
data$spl=NA
for(each_child in levels(data$subject_id)) if(sum(data$subject_id==each_child) == length(rep(1:2,sum(data$subject_id==each_child)/2))) data$spl[data$subject_id==each_child]<-rep(1:2, sum(data$subject_id==each_child)/2) else data$spl[data$subject_id==each_child]<-c(rep(1:2, sum(data$subject_id==each_child)/2),1)

results$pc_1=extractVwithNA(levels(data$subject_id),aggregate(data $correct[data$spl==1],by=list(data $subject_id[data$spl==1]),mean))
results$pc_2=extractVwithNA(levels(data$subject_id),aggregate(data $correct[data$spl==2],by=list(data $subject_id[data$spl==2]),mean))

results$rt_1=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$spl==1],by=list(data $subject_id[data$spl==1]),median))
results$rt_2=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$spl==2],by=list(data $subject_id[data$spl==2]),median))

#*$* Write in the number of left responses
results$propLeft=as.numeric(as.character(table(data$subject_id,data$object_touched_position)[,1]))/results$trials_attempted
results$bias<-ifelse(results$propLeft <.4 | results$propLeft>.6,1,0)  #this should be improved!!

#### STEP 3: RESULTS SEPARATING DIFFERENT TRIAL TYPES

# Read in ancillary table
dif=read.xlsx("~/Dropbox/tabletAna/data/Difficultes.xlsx",1)

## Add useful info to the data sheet
#classify trials by word type & difficulty level
data$dif=NA
data$dif[as.character(data$object_asked) %in% as.character(dif$word[dif$level=="easy"])]<-"easy"
data$dif[as.character(data$object_asked) %in% as.character(dif$word[dif$level=="moderate"])]<-"moderate"
data$dif[as.character(data$object_asked) %in% as.character(dif$word[dif$level=="difficult"])]<-"difficult"
data$dif=factor(data$dif)

data$lex=NA
data$lex[as.character(data$object_asked) %in% as.character(dif$word[dif$wordtype =="adj"])]<-"adj"
data$lex[as.character(data$object_asked) %in% as.character(dif$word[dif$wordtype =="noun"])]<-"noun"
data$lex[as.character(data$object_asked) %in% as.character(dif$word[dif$wordtype =="verb"])]<-"verb"

#### by difficulty level
#Ns
results$N_easy=as.numeric(table(data$subject_id,data$dif)[,"easy"])
results$N_mod=as.numeric(table(data$subject_id,data$dif)[,"moderate"])
results$N_diff= as.numeric(table(data$subject_id,data$dif)[,"difficult"])


#Calculate exclusion matrix
exclude = table(data$subject_id,data$dif) < minTrialNtype   #Apply minimum number of trials 


#*$* percent correct
results$pc_easy=extractVwithNA(levels(data$subject_id),aggregate(data $correct[data$dif=="easy"],by=list(data $subject_id[data$dif== "easy"]),mean))
results$pc_easy[exclude[,"easy"]]<-NA

results$pc_mod=extractVwithNA(levels(data$subject_id),aggregate(data $correct[data$dif=="moderate"],by=list(data $subject_id[data$dif== "moderate"]),mean))
results$pc_mod[exclude[,"moderate"]]<-NA

results$pc_diff=extractVwithNA(levels(data$subject_id),aggregate(data $correct[data$dif=="difficult"],by=list(data $subject_id[data$dif== "difficult"]),mean))
results$pc_diff[exclude[,"difficult"]]<-NA


#Calculate exclusion matrix
exclude = table(data$subject_id[data$correct==1],data$dif[data$correct==1]) < minTrialNtype   #Apply minimum number of trials 

#*$* rt corr only
results$rt_easy=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$dif=="easy" & data$correct==1],by=list(data $subject_id[data$dif== "easy" & data$correct==1]), median))
results$rt_easy[exclude[,"easy"]]<-NA

results$rt_mod=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$dif=="moderate" & data$correct==1],by=list(data $subject_id[data$dif== "moderate" & data$correct==1]), median))
results$rt_mod[exclude[,"moderate"]]<-NA

results$rt_diff=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$dif=="difficult" & data$correct==1],by=list(data $subject_id[data$dif== "difficult" & data$correct==1]),median))
results$rt_diff[exclude[,"difficult"]]<-NA


#### by lexical type
#Ns
results$N_noun=as.numeric(table(data$subject_id,data$lex)[,"noun"])
results$N_adj=as.numeric(table(data$subject_id,data$lex)[,"adj"])
results$N_verb=as.numeric(table(data$subject_id,data$lex)[,"verb"])


#Calculate exclusion matrix
exclude = table(data$subject_id,data$lex) < minTrialNtype


#*$* percent correct
results$pc_noun=extractVwithNA(levels(data$subject_id),aggregate(data $correct[data$lex =="noun"],by=list(data $subject_id[data$lex== "noun"]),mean))
results$pc_noun[exclude[,"noun"]]<-NA

results$pc_adj =extractVwithNA(levels(data$subject_id),aggregate(data $correct[data$lex =="adj"],by=list(data $subject_id[data$lex== "adj"]),mean))
results$pc_adj[exclude[,"adj"]]<-NA

results$pc_verb =extractVwithNA(levels(data$subject_id),aggregate(data $correct[data$lex =="verb"],by=list(data $subject_id[data$lex== "verb"]),mean))
results$pc_verb[exclude[,"verb"]]<-NA


#*$* rt corr only
results$rt_noun=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$lex=="noun" & data$correct==1],by=list(data $subject_id[data$lex== "noun" & data$correct==1]), median))
results$rt_noun[exclude[,"noun"]]<-NA

results$rt_adj=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$lex=="adj" & data$correct==1],by=list(data $subject_id[data$lex== "adj" & data$correct==1]), median))
results$rt_adj[exclude[,"adj"]]<-NA

results$rt_verb=extractVwithNA(levels(data$subject_id),aggregate(data $object_touched_at[data$lex=="verb" & data$correct==1],by=list(data $subject_id[data$lex== "verb" & data$correct==1]),median))
results$rt_verb[exclude[,"verb"]]<-NA


#Squarify and write out
resultsdf=data.frame(results)
write.table(resultsdf,"~/Dropbox/tabletAna/_results/results.txt",row.names=F,quote=F,sep="\t")

#And also write out the final data
write.table(data,"~/Dropbox/tabletAna/_results/data_final.txt",row.names=F,quote=F,sep="\t")

## add individual information
read.table("~/Dropbox/tabletAna/_results/quest.txt",header=T)->quest
read.table("~/Dropbox/tabletAna/_results/corresp.txt",header=T)->corresp

merge(resultsdf,corresp,by.x="id",by.y="CSV")->resultsdf
merge(resultsdf,quest[c(1,2,3,4,5,17,18,19,20,23,24)],by.x="Anon",by.y="CA")->resultsdf
resultsdf$age=(as.Date(resultsdf$Date,"%d/%m/%Y") - as.Date(resultsdf$DOB,"%d/%m/%Y"))/365.25
resultsdf<-resultsdf[resultsdf$Remove=="no",]
resultsdf$ed=ifelse(resultsdf$edMom>7,"higher","lower")


write.table(resultsdf,"~/Dropbox/tabletAna/_results/cm_new_results.txt",row.names=F,sep="\t")
