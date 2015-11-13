#script for converting into time sample

rm(list=ls())

setwd("~/Dropbox/Ongoing_Work/Time_Use/Inputs")
atusact<-read.table("atusact_0314.dat",header=TRUE,sep=",")
atussum<-read.table("atussum_0314.dat",header=TRUE,sep=",")

#atusact<-read.table("C:\\Users\\axs5498\\Dropbox\\Ongoing_Work\\Time_Use\\Inputs\\atusact_0314.dat",header=TRUE,sep=",")
#atussum<-read.table("C:\\Users\\axs5498\\Dropbox\\Ongoing_Work\\Time_Use\\Inputs\\atussum_0314.dat",header=TRUE,sep=",")

id_weight<-atussum[,c('TUCASEID','TUFNWGTP')]




setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results")
#setwd("C:\\Users\\axs5498\\Dropbox\\Ongoing_Work\\Time_Use\\activitydemand\\Results\\")


act_codes<-read.table("activitycodes.txt")
maj_act_codes<-unique(floor(act_codes/10000))
sub_act_codes<-unique(floor(act_codes/100))



#starting to sample

filename1<-paste0('freq_hhoccupancy.csv') # creating a filename 


actfile<-atusact[,c('TUCASEID','TUACTDUR24','TRTIER1P','TRTIER2P','TUCUMDUR24','TEWHERE')] #further subset selected vars
actfile$TUCASEID<-format(actfile$TUCASEID,digits = 10) # display TUCASEID 
actfile<-merge(actfile,id_weight,by.y = "TUCASEID")

actfile$year<-strtrim(actfile$TUCASEID,4)
actfile_2014<-subset(actfile,year==2014)

actfile_2014$TEWHERE[which(actfile_2014$TRTIER2P==101|actfile_2014$TRTIER2P==102|actfile_2014$TRTIER2P==104)]<-1

actfile_2014<-subset(actfile_2014,TEWHERE==1) #subsetting file based on precense at home 

#creating news columns to identify start and stop index
actfile_2014$startid<-actfile_2014$TUCUMDUR24/10-actfile_2014$TUACTDUR24/10+1 
actfile_2014$startid[actfile_2014$startid>144]=144
actfile_2014$stopid<-actfile_2014$TUCUMDUR24/10

#initializing matrix 
classfile<-data.frame(matrix(nrow = length(unique(actfile_2014$TUCASEID)),ncol = 144))
tucaseid<-data.frame(unique(actfile_2014$TUCASEID))
colnames(tucaseid)<-'TUCASEID'
s_weight<-merge(tucaseid,id_weight,by.y ="TUCASEID")

#identifying frequency of each person doing the sected major activity 
freq<-data.frame(table(actfile_2014$TUCASEID))
freq<-freq$Freq

#the loop to get the class_file
j<-1

#j<-312614

no_of_rows<-length(tucaseid$TUCASEID)

for (i in 1:no_of_rows)
{  
  #for (j in 1:freq[i])
  while (actfile_2014$TUCASEID[j]==tucaseid$TUCASEID[i])
  {
    classfile[i,actfile_2014$startid[j]:actfile_2014$stopid[j]]<-1
    j<-j+1
    
    if (j>length(actfile_2014$TUCASEID)) break 
  } 
} 

classfile[is.na(classfile)]<-0
#write.csv(classfile,filename1)
write.csv(classfile,'hhoccupancy_2014_sample.csv')

classfile<-read.csv('hhoccupancy_2014_sample.csv')
classfile<-classfile[,-1]


#index of TELFS 
classfile$TUCASEID<-unique(actfile_2014$TUCASEID)
classfile<-merge(classfile,id_weight,by.y = "TUCASEID")

classfile<-merge(x=classfile,y=atussum[,c('TUCASEID','TELFS','TRHOLIDAY')],by='TUCASEID',all.x = TRUE)
classfile<-merge(x=classfile,y=atussum[,c('TUCASEID','TRSPFTPT')],by='TUCASEID',all.x = TRUE)


classfile$TELFSNEW<-classfile$TELFS
classfile$TELFSNEW[which(classfile$TELFS!=1)]<-2

file_employed<-subset(classfile,TELFS==1)
file_retired<-subset(classfile,TELFS==5)
file_notemployed<-subset(classfile,TELFSNEW==2)
file_employed_notholiday<-subset(classfile,TELFS==1&TRHOLIDAY==0)
file_employed_holiday<-subset(classfile,TELFS==1&TRHOLIDAY==1)
file_employed_ft<-subset(classfile,TELFS==1&TRSPFTPT==1)
file_employed_pt<-subset(classfile,TELFS==1&TRSPFTPT==2)



#program to find the weighted centroid

w_centroid<-data.frame(matrix(nrow = 7,ncol = 144))
rownames(w_centroid)<-c('employed','employed_notholiday','employed_holiday','retired','notemployed','fulltime','parttime')
colnames(w_centroid)<-1:144

for (i in 1:144)
{
  w_centroid[1,i]<-sum(apply(cbind(file_employed[,i+1],file_employed$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_employed$TUFNWGTP)
  w_centroid[2,i]<-sum(apply(cbind(file_employed_notholiday[,i+1],file_employed_notholiday$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_employed_notholiday$TUFNWGTP)
  w_centroid[3,i]<-sum(apply(cbind(file_employed_holiday[,i+1],file_employed_holiday$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_employed_holiday$TUFNWGTP)
  
  w_centroid[4,i]<-sum(apply(cbind(file_retired[,i+1],file_retired$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_retired$TUFNWGTP)
  w_centroid[5,i]<-sum(apply(cbind(file_notemployed[,i+1],file_notemployed$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_notemployed$TUFNWGTP)
  w_centroid[6,i]<-sum(apply(cbind(file_employed_ft[,i+1],file_employed_ft$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_employed_ft$TUFNWGTP)
  w_centroid[7,i]<-sum(apply(cbind(file_employed_pt[,i+1],file_employed_pt$TUFNWGTP), 1, prod,na.rm=TRUE))/sum(file_employed_pt$TUFNWGTP)
  
}


write.csv(w_centroid,'hhoccupancy_2014.csv')


