#script for converting into time sample

rm(list=ls())

setwd("~/Dropbox/Ongoing_Work/Time_Use/Inputs")
atusact<-read.table("atusact_0314.dat",header=TRUE,sep=",")
atussum<-read.table("atussum_0314.dat",header=TRUE,sep=",")

#atusact<-read.table("C:\\Users\\axs5498\\Dropbox\\Ongoing_Work\\Time_Use\\Inputs\\atusact_0314.dat",header=TRUE,sep=",")
#atussum<-read.table("C:\\Users\\axs5498\\Dropbox\\Ongoing_Work\\Time_Use\\Inputs\\atussum_0314.dat",header=TRUE,sep=",")
atussum<-atussumm
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
actfile$startid<-actfile$TUCUMDUR24/10-actfile$TUACTDUR24/10+1 
actfile$startid[actfile$startid>144]=144
actfile$stopid<-actfile$TUCUMDUR24/10

maj_activity_names<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/maj_activity_names.csv",header = FALSE)
maj_activity_names$Var1<-maj_act_codes$V1
maj_activity_names<-rename(maj_activity_names,c("Var1"="TRTIER1P"))
actfile<-merge(x=actfile,y=maj_activity_names,by='TRTIER1P',all.x = TRUE)


#find index of repeats 
chk<-table(actfile$TUCASEID)
id_resp<-as.data.frame(chk)
rownames(id_resp)<-id_resp$Var1
id_resp<-cumsum(id_resp$Freq)

#TUACTDUR24R<-matrix(ncol = 1,nrow = length(file$TUCASEID))
id<-1:2
save_file<-actfile

actfile<-actfile[order(actfile[,2],actfile[,7]),]


for (i in 1:length(id_resp)) # FOR EACH RESPONDENT 
{
  id[2]<-id_resp[i]
  #identifying the repeats 
  if(actfile$TRCODEP[id[1]]==actfile$TRCODEP[id[2]]) # if the first and last id matches
  {
    actfile$TUACTDUR24[id[2]]<-actfile$TUACTDUR24[id[1]]+actfile$TUACTDUR24[id[2]] # add the values
    actfile$TUACTDUR24[id[1]]<-NA # place the remaining
  } 
  id[1]<-id[2]+1
}



dev.off() 
plots<-list() 
for (i in 1:18)
{   
  a<-maj_act_codes$V1[i]
  #filename<-paste0('hist_duration_of_maj_activity_',maj_act_names$variable[i],'.pdf')
  
  
  f<-ggplot(data = subset(actfile,TRTIER1P==a&TUACTDUR24>0),aes(startid))
  fig<-f+geom_histogram(aes(y=..count../sum(..count..)),binwidth=12)+
    ylab('ratio of respondents')+
    xlab('time')+
    scale_x_continuous(breaks=seq(0,144,by=12),labels=c('4AM','6AM','8AM','10AM','12PM','2PM','4PM','6PM','8PM','10PM','12AM','2AM','4AM')
    )+ 
    scale_y_continuous(breaks=seq(0,0.3,by=.1),limits=c(0,0.3),labels=c('0%','10%','20%','30%'))+
    ggtitle(maj_activity_names$V1[i])+
    theme(text = element_text(size=7), 
          axis.text.x = element_text(colour="grey20",size=6,angle=45,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=7,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=7,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=7,angle=90,hjust=.5,vjust=.5,face="plain"))   
   
  plots[[i]]<-fig 
  
  #ggsave(file=filename)
}  

dev.off()
pdf(file = "myplot.pdf",width = 13.33,height = 7.5)
library(Rmisc)
multiplot(plotlist=plots,cols=6)
dev.off()





for (i in 1:18)
{
  
  f<-ggplot(data = actfile,aes(V1,x=startid,TUFNWGTP))
  pic<-f+geom_histogram(aes(y=..count..,weight=TUFNWGTP))+
  
}


for (a in 11)#:length(maj_act_codes$V1)) # for the number of major activity codes 
{
  aa<-maj_act_codes$V1[a] #getting the maj activity code
  filename1<-paste0('freqfile_majact_',aa,'.csv') # creating a filename 
  
  actfile<-subset(atusact,TRTIER1P==aa) #subsetting file based on maj activity code
  actfile<-actfile[,c('TUCASEID','TUACTDUR24','TRTIER1P','TUCUMDUR24')] #further subset selected vars
  actfile$TUCASEID<-format(actfile$TUCASEID,digits = 10) # display TUCASEID 
  actfile<-merge(actfile,id_weight,by.y = "TUCASEID")
  
  
  #creating news columns to identify start and stop index
  actfile$startid<-actfile$TUCUMDUR24/10-actfile$TUACTDUR24/10+1 
  actfile$startid[actfile$startid>144]=144
  actfile$stopid<-actfile$TUCUMDUR24/10
  
  #initializing matrix 
  classfile<-data.frame(matrix(nrow = length(unique(actfile$TUCASEID)),ncol = 144))
  tucaseid<-data.frame(unique(actfile$TUCASEID))
  colnames(tucaseid)<-'TUCASEID'
  s_weight<-merge(tucaseid,id_weight,by.y ="TUCASEID")
  
  #identifying frequency of each person doing the sected major activity 
  freq<-data.frame(table(actfile$TUCASEID))
  freq<-freq$Freq
  
  #the loop to get the class_file
  j<-1
  
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
    write.csv(classfile,filename1)
  
#   #new matrix for weighted centroid 
#   for (i in 1:144)
#   {
#     w_centroid[a-2,i]<-sum(apply(cbind(classfile[,i],s_weight$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(s_weight$TUFNWGTP)
#   }
#  
  
  rm(classfile)
}


#program to find the weighted centroid


#new matrix for weighted centroid 
w_centroid<-matrix(nrow = length(maj_act_codes$V1),ncol = 144)
filename2<-paste0('centroid_majact.csv')#filename for the matrix

#input all freqfile as classfile 
#dataFiles<-lapply(Sys.glob("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/Classification_Files/freqfile_majact_*.csv"), read.csv) #input atussumm for each maj category

myFiles <- list.files(pattern="freqfile_majact_*.csv")

j<-1
for (i in 1:18)
{
  filter<-read.csv(myFiles[i])
  filter<-filter[,-1]
  
  a<-strsplit(myFiles[i],"[.]")
  aaa<-strsplit(a[[1]][1],"_")
  aa<-aaa[[1]][3]
   #getting the maj activity code
  
  actfile<-subset(atusact,TRTIER1P==aa) #subsetting file based on maj activity code
  actfile<-actfile[,c('TUCASEID','TUACTDUR24','TRTIER1P','TUCUMDUR24')] #further subset selected vars
  actfile$TUCASEID<-format(actfile$TUCASEID,digits = 10) # display TUCASEID 
  actfile<-merge(actfile,id_weight,by.y = "TUCASEID")
  
  ID<-unique(actfile$TUCASEID)
  
  filter$TUCASEID<-ID
  
  filter<-merge(x=filter,y=atussum[,c('TUCASEID','TELFS','TUFNWGTP','TRSPFTPT')],by='TUCASEID',all.x = TRUE)
  
  
  file_employed_ft<-subset(filter,TELFS==1&TRSPFTPT==1)
  file_employed_pt<-subset(filter,TELFS==1&TRSPFTPT==2)
  file_unemp<-subset(filter,TELFS!=1)
  file_retired<-subset(filter,TELFS==5)
  
  
  
  
  for (i in 1:144)
  {
    w_centroid[j,i]<-sum(apply(cbind(file_employed_ft[,i+1],file_employed_ft$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_employed_ft$TUFNWGTP)
    w_centroid[j+1,i]<-sum(apply(cbind(file_employed_pt[,i+1],file_employed_pt$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_employed_pt$TUFNWGTP)
    w_centroid[j+2,i]<-sum(apply(cbind(file_unemp[,i+1],file_unemp$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_unemp$TUFNWGTP)
    
    w_centroid[j+3,i]<-sum(apply(cbind(file_retired[,i+1],file_retired$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_retired$TUFNWGTP)
#     w_centroid[5,i]<-sum(apply(cbind(file_notemployed[,i+1],file_notemployed$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_notemployed$TUFNWGTP)
#     w_centroid[6,i]<-sum(apply(cbind(file_employed_ft[,i+1],file_employed_ft$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_employed_ft$TUFNWGTP)
#     w_centroid[7,i]<-sum(apply(cbind(file_employed_pt[,i+1],file_employed_pt$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_employed_pt$TUFNWGTP)
#     
  }
  
  j<-j+4
  filename<-paste0('weight_maj_act_byemp',aa,'.csv')
  
} 

write.csv(w_centroid,filename)

library(reshape2)
w_centroid_emp<-read.csv('~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/Classification_Files/weight_maj_act_byemp.csv')
w_centroid_emp$legend<-chk2
w_centroid_emp$X<-NULL
w_centroid_emp$codes<-rep(c(1,10:16,18,2:5,50,6:9),each=4)
colnames(w_centroid_emp)<-c(1:144,'legend','codes')
z<-melt(w_centroid_emp,id=c("legend","codes")) 

w_centroid_avg<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/centroid_majact.csv")
w_centroid_avg<-w_centroid_avg[,-1] 
w_centroid_avg$legend<-'average' 
w_centroid_avg$codes<-c(1:16,18,50) 
colnames(w_centroid_avg)<-c(1:144,'legend','codes') 
zz<-melt(w_centroid_avg,id=c("legend","codes"))

zzz<-rbind(zz,z)
library(plyr)
maj_activity_names<-rename(maj_activity_names,c("variable"="codes"))
zzz<-merge(x=zzz,y=maj_activity_names,by='codes',all.x=TRUE)


zzz$variable<-as.numeric(zzz$variable)

f<-ggplot(data=zzz,aes(y=value,x=variable,colour=legend))
fig<-f+geom_line()+facet_wrap(~V1,nrow=6)
fig+theme(text = element_text(size=8),
          axis.text.x = element_text(colour="grey20",size=8,angle=45,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=8,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=8,angle=90,hjust=.5,vjust=.5,face="plain"),
          strip.text=element_text(size = 8))+
  ylab('probability of performing activity')+
  xlab('time in (hh:mm)')+
  ggtitle('activity pattern')+
  scale_x_continuous(breaks=seq(0,144,by=12),limits=c(0,144),
                     labels=c('4AM','6AM','8AM','10AM','12PM','2PM','4PM','6PM','8PM','10PM','12AM','2AM','4AM'))+   
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1),labels=c('0%','50%','100%'))
#geom_text(data=reg_outputs,aes(x,y,label=label),inherit.aes=FALSE,size=3)   


ggsave(file="all.pdf",width = 13.33,height = 7.5)








#loop to calculate centroid 
for (a in 1:length(maj_act_codes$V1))
{
  aa<-maj_act_codes$V1[a] #getting the maj activity code
  name<-paste0('~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/freqfile_majact_',aa,'.csv')
  #edit the class file to contain only 
  classfile<-read.csv(name,header=TRUE)
  classfile<-classfile[,-1]
  
  
  
  #find s_weight
  aa<-maj_act_codes$V1[a] #getting the maj activity code
  filename1<-paste0('freqfile_majact_',aa,'.csv') # creating a filename 
  actfile<-subset(atusact,TRTIER1P==aa) #subsetting file based on maj activity code
  actfile<-actfile[,c('TUCASEID','TUACTDUR24','TRTIER1P','TUCUMDUR24')] #further subset selected vars
  actfile$TUCASEID<-format(actfile$TUCASEID,digits = 10) # display TUCASEID 
  actfile<-merge(actfile,id_weight,by.y = "TUCASEID")
  
  tucaseid<-data.frame(unique(actfile$TUCASEID))
  colnames(tucaseid)<-'TUCASEID'
  s_weight<-merge(tucaseid,id_weight,by.y ="TUCASEID")
  for (i in 1:144)
  {
    w_centroid[a,i]<-sum(apply(cbind(classfile[,i],s_weight$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(s_weight$TUFNWGTP)
  }
}

write.csv(w_centroid,filename2)

#how to print 18 different figures 
centroid<-read.csv('centroid_majact.csv',header=TRUE)
centroid<-centroid[,-1]
centroid$names<-maj_activity_names$V1
colnames(centroid)<-c(1:144,'names')

melt_centroid<-melt(centroid)
melt_centroid$variable<-as.numeric(melt_centroid$variable)

f<-ggplot(data=melt_centroid,aes(y=value,x=variable))
fig<-f+geom_line()+facet_wrap(~names,nrow=6)
fig+theme(text = element_text(size=8),
        axis.text.x = element_text(colour="grey20",size=8,angle=45,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=8,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=8,angle=90,hjust=.5,vjust=.5,face="plain"),
        strip.text=element_text(size = 8))+
  ylab('probability of performing activity')+
  xlab('time in (hh:mm)')+
  ggtitle('activity pattern')+
  scale_x_continuous(breaks=seq(0,144,by=12),limits=c(0,144),
                     labels=c('4AM','6AM','8AM','10AM','12PM','2PM','4PM','6PM','8PM','10PM','12AM','2AM','4AM'))+   
  scale_y_continuous(breaks=seq(0,1,by=.5),limits=c(0,1),labels=c('0%','50%','100%'))
  #geom_text(data=reg_outputs,aes(x,y,label=label),inherit.aes=FALSE,size=3)   


ggsave(file="average.pdf",width = 13.33,height = 7.5)


