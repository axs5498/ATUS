#television classification check 

#script for converting into time sample

rm(list=ls())

setwd("~/Dropbox/Ongoing_Work/Time_Use/Inputs")
atusact<-read.table("atusact_0314.dat",header=TRUE,sep=",")
atussum<-read.table("atussum_0314.dat",header=TRUE,sep=",")

#atusact<-read.table("C:\\Users\\axs5498\\Dropbox\\Ongoing_Work\\Time_Use\\Inputs\\atusact_0314.dat",header=TRUE,sep=",")
#atussum<-read.table("C:\\Users\\axs5498\\Dropbox\\Ongoing_Work\\Time_Use\\Inputs\\atussum_0314.dat",header=TRUE,sep=",")
#atussum<-atussumm
id_weight<-atussum[,c('TUCASEID','TUFNWGTP')]




setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results")
#setwd("C:\\Users\\axs5498\\Dropbox\\Ongoing_Work\\Time_Use\\activitydemand\\Results\\")


act_codes<-read.table("activitycodes.txt")
maj_act_codes<-unique(floor(act_codes/10000))
sub_act_codes<-unique(floor(act_codes/100))


#code for doing statistics of start time 
actfile<-atusact
actfile<-actfile[,c('TUCASEID','TUACTDUR24','TRTIER1P','TUCUMDUR24','TRCODEP')] #further subset selected vars
actfile$TUCASEID<-format(actfile$TUCASEID,digits = 10) # display TUCASEID 
actfile<-merge(actfile,id_weight,by.y = "TUCASEID")
actfile$year<-as.numeric(substr(actfile$TUCASEID,1,4))

#subsetting people who watched television and 2013
tvlexicon<-c(120303,120304);

actfile_main<-actfile #saving a copy

actfile<-actfile_main[actfile_main$year %in% c(2013),]

actfile<-actfile[actfile$TRCODEP %in% c(120303,120304),]


actfile$startid<-round(actfile$TUCUMDUR24/5.625-actfile$TUACTDUR24/5.625+1,0) 
actfile$startid[actfile$startid>256]<-256
actfile$stopid<-round(actfile$TUCUMDUR24/5.625,0)


#initializing matrix 
classfile<-data.frame(matrix(nrow = length(unique(actfile$TUCASEID)),ncol = 256))
tucaseid<-data.frame(unique(actfile$TUCASEID))
colnames(tucaseid)<-'TUCASEID'
s_weight<-merge(tucaseid,id_weight,by.y ="TUCASEID")

#identifying frequency of each person doing the sected major activity 
freq<-data.frame(table(actfile$TUCASEID))
freq<-freq$Freq

#the loop to get the class_file
j<-1

#j<-312614


no_of_rows<-length(tucaseid$TUCASEID)

for (i in 1:no_of_rows)
{  
  #for (j in 1:freq[i])
  while (actfile$TUCASEID[j]==tucaseid$TUCASEID[i])
  {
    classfile[i,actfile$startid[j]:actfile$stopid[j]]<-1
    j<-j+1
    
    if (j>length(actfile$TUCASEID)) break 
  } 
} 

classfile[is.na(classfile)]<-0
classfile$TUCASEID<-tucaseid$TUCASEID
write.csv(classfile,'newclass_tv_2013.csv')


classfile$TUCASEID<-NULL 

notv<-data.frame()
notv<-actfile_main[!actfile_main$TUCASEID %in% actfile$TUCASEID,]
notv<-notv[notv$year %in% c(2013),]
notv_caseid<-unique(notv$TUCASEID)




#write.csv(classfile,filename1)
write.csv(classfile,'hhoccupancy_2014_sample.csv')




