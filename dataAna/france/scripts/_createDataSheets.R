# Analyse en R pour une étude sur la reconnaissance et l'apprentissage des mots 2015/2016
# mené sur Mandy 

# Base  Alex Cristia alecristia@gmail.com 2016-04-21
# Dernières modifs XX WRITE YOUR NAME HERE IF YOU EDIT THIS!

setwd("~/Dropbox/tabletana/")


#*************************************************************************************************************************************
#### CSV DATA
## Read in the CSV
dirlist = dir(path="data")

data=NULL
for(thisdir in dirlist){
	dir(paste("data",thisdir,sep="/"),pattern='csv')->allcsv
	for(thisf in allcsv){
		print(thisf)
		read.csv(paste("data",thisdir,thisf,sep="/"))->thiscsv
		data =rbind(data,cbind(thisdir,thisf,thiscsv)) 
		}
}
dim(data)  
write.table(data,"_results/data.txt",row.names=F,sep="\t")



#***********************************
###Read in data from other sources
#***********************************
library(RCurl)

#Read in questionnaire
u_quest <- "https://docs.google.com/spreadsheets/d/18GV9dZh-SMDuqylD3emg30NQmcLa2t-JYFr24hPuNC0/pub?gid=0&single=true&output=csv"
tc_quest <- getURL(u_quest, ssl.verifypeer=FALSE)
quest <- read.csv(textConnection(tc_quest))
write.table(quest,"_results/quest.txt",row.names=F,sep="\t")

#Read in correspondance table
u_corr <- "https://docs.google.com/spreadsheets/d/1As4Hue9Uskk3s_SVuHEtCUFYHc6NX2NxH1S4gtig8d8/pub?gid=0&single=true&output=csv"
tc_corr <- getURL(u_corr, ssl.verifypeer=FALSE)
corresp <- read.csv(textConnection(tc_corr))
write.table(corresp,"_results/corresp.txt",row.names=F,sep="\t")

#***********************************
###Create some tables for checking purposes
#***********************************
c("uuid","subject_id","session_started_at","config_profile","thisdir","thisf","date")->interestingColumns
tab=NULL
for(thisunique in levels(data$session_started_at)) tab=rbind(tab,data[data$session_started_at==thisunique,interestingColumns][1,])
write.table(tab,"_results/tabsum.txt",row.names=F,sep="\t")

#merge information together just to check whether all adds up
merge(data,corresp,by.x="uuid",by.y="CSV",all.x=T)->x
head(x)

#c("subject_id","Child_ID","session_started_at","config_profile","Profile","date","Date","Anon","thisdir","thisf")->interestingColumns
c("date","Date","subject_id","session_started_at","config_profile","Anon","thisdir","thisf")->interestingColumns
tab=NULL
for(thisunique in levels(x$session_started_at)) tab=rbind(tab,x[x$session_started_at==thisunique,interestingColumns][1,])
write.table(tab,"_results/tabsumMerged.txt",row.names=F,sep="\t")