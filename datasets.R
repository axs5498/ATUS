#the objective of the script is to develop new datasets from ATUS data for further
#processing. 

#Author: Ashok Sekar

#datasets that summarizes the total time spent per activity 
#classified by respondentid and year.  
#other useful information include start and stop time with whom activity was done 
#and preserving the activity sequence. 


#There are two sets of datasets to be created. 
#first - datasets the list total time per spent per day for each activity  
#second - datasets that lists total time spend per instance of performing the activity 

#withing each dataset activity codes are summarized as major categories, sub categories
#and all activities. 
rm(list=ls())

#code for creating dataset that summarizes total time spent per day. 
#load file
atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")

#defining global variables 
years<-2003:2014 # the years we are working with 

# list of all the activities sorted e.g., 10101...509090
act_codes<-read.table("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activitycodes.txt")  

maj_act_codes<-unique(floor(act_codes/10000)) # list of major categories

sub_act_codes<-unique(floor(act_codes/100)) # list of sub categories 

ind_maj_act_codes<-0 # index of maj 
ind_sub_act_codes<-0 # index of sub

for (i in 1:length(maj_act_codes$V1)) # code to find out the index of maj and sub 
{
  ind_maj_act_codes[i]<-max(which(floor(act_codes/10000)==maj_act_codes$V1[i]))
  for (j in 1:length(sub_act_codes$V1))
  {
    ind_sub_act_codes[j]<-max(which(floor(act_codes/100)==sub_act_codes$V1[j]))
  }
}

#generating the datasets for total time per day 
#variable names: 
#all activity = tottime_peractivity
#sub categories = tottime_subactivity
#maj categories = tottime_majactivity


#code for tottime_peractivity
tottime_peractivity<-(atussumm[,25:455]) #subsetting only necessary data 
tottime_peractivity[tottime_peractivity==0]<-NA #convert zeros to NA to avoid in calculation of means
colnames(tottime_peractivity)<-act_codes$V1 # changing colnames without "t"
rownames(tottime_peractivity)<-atussumm$TUCASEID

#code for tottime_subactivity 

tottime_subactivity<-data.frame(matrix(ncol = length(sub_act_codes$V1),nrow = length(tottime_peractivity$`10101`)))
colnames(tottime_subactivity)<-sub_act_codes$V1
rownames(tottime_subactivity)<-atussumm$TUCASEID
j<-1
for (i in 1:length(sub_act_codes$V1))
{
  if (j!=ind_sub_act_codes[i])
  {
    #chk<-rowSums(is.na(tottime_peractivity[,j:ind_sub_act_codes[i]]))
    #chk1<-which(chk!=ind_sub_act_codes[i]-j+1)
    #tottime_subactivity[chk1,i]<-rowMeans(tottime_peractivity[chk1,j:ind_sub_act_codes[i]],na.rm = TRUE)
    tottime_subactivity[,i]<-rowSums(tottime_peractivity[,j:ind_sub_act_codes[i]],na.rm = TRUE)
    }
  else
  {
    #chk2<-is.na(tottime_peractivity[,ind_sub_act_codes[i]])
    #chk3<-which(chk2==FALSE)
    tottime_subactivity[,i]<-tottime_peractivity[,j]
  } 
  j<-ind_sub_act_codes[i]+1
}


#code for tottime_majactivity
tottime_majactivity<-data.frame(matrix(ncol = length(maj_act_codes$V1),nrow = length(tottime_peractivity$`10101`)))
colnames(tottime_majactivity)<-maj_act_codes$V1
rownames(tottime_majactivity)<-atussumm$TUCASEID
j<-1
for (i in 1:length(maj_act_codes$V1))
{
  #chk<-rowSums(is.na(tottime_peractivity[,j:ind_maj_act_codes[i]]))
  #chk1<-which(chk!=ind_maj_act_codes[i]-j+1)
  #tottime_majactivity[chk1,i]<-rowMeans(tottime_peractivity[chk1,j:ind_maj_act_codes[i]],na.rm = TRUE)
  tottime_majactivity[,i]<-rowSums(tottime_peractivity[,j:ind_maj_act_codes[i]],na.rm = TRUE)
  j<-ind_maj_act_codes[i]+1
}




#set output directory
setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/")

write.csv(tottime_peractivity,file = "activityduration_perday_allactivity.csv",row.names=TRUE)
write.csv(tottime_majactivity,file = "activityduration_perday_majactivity.csv",row.names=TRUE)
write.csv(tottime_subactivity,file = "activityduration_perday_subactivity.csv",row.names=TRUE)


##################################

#the second part of the code is to generate a dataset for perinstance. 
#need to note here that along with the per instance data weights per respondent
#should be generated 

rm(list = ls()) # removing all variables generated earlier 

#load file 
atusact<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusact_0314.dat",header=TRUE,sep=",")

#global variables 
#defining global variables 
years<-2003:2014 # the years we are working with 

# list of all the activities sorted e.g., 10101...509090
act_codes<-read.table("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activitycodes.txt")  


#filtering and formating only necessary variables in the dataset 
file<-atusact[c(1:3,11:13)]

#in atusact file each row is an activity of respondent id in a sequence. 
#in the first and last activity in the sequence there is a "timesplit" 
#e.g., sleeping from 4AM - 7AM and 12AM-4AM 
#therefore the database should not include the timesplit for quantifying descriptive
#stats 

#find index for each respondent for each year to id timesplit 
id_resp<-0
chk<-table(file$TUCASEID)
id_resp<-as.data.frame(chk)
rownames(id_resp)<-id_resp$Var1
id_resp<-cumsum(id_resp$Freq)


#generating the dataset by identiyfing the activities that are "time split" 

id<-1:2
for (i in 1:length(id_resp)) # FOR EACH RESPONDENT 
{
  id[2]<-id_resp[i]
  #identifying the repeats 
  if(file$TRCODEP[id[1]]==file$TRCODEP[id[2]]) # if the first and last id matches
  {
    file$TUACTDUR24[id[1]]<-file$TUACTDUR24[id[1]]+file$TUACTDUR24[id[2]] # add the values
    file$TUACTDUR24[id[2]]<-NA # place the remaining
  } 
  id[1]<-id[2]+1
}


###add weights to the file 
# file$weights<-0 ### this part is commented since it takes too long. data 
##stored as variable == 
# id<-1:2
# for (i in 1:length(id_resp))
# {
#   id[2]<-id_resp[i]
#   file$weights[id[1]:id[2]]<-actsumm$TUFNWGTP[i]
#   id[1]<-id[2]+1
# }
#reformatted_weights_atusact<-file$weights

wgts<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/reformatted_wgts_atusact.txt",header = FALSE)
file$weights<-wgts$V1


#set output directory
setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/")
write.csv(file,file = "activityduration_perinstance_allactivity.csv",row.names=TRUE)

