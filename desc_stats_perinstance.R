#clear all variables in workspace
rm(list = ls())

#set input direcotry
setwd("~/Dropbox/Ongoing_Work/Time_Use/Inputs")

####load file

atusact<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusact_0314.dat",header=TRUE,sep=",")
actsumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")

#set result directory
setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/")
act_codes<-read.table("activitycodes.txt")


#defining global variables
years<-2003:2014

#subset dataset with only the following variables: 
# CASEID, majactivity, subactivity, allactivities, activitynumber 

file<-atusact[c(1:3,11:13)]


###find index of maj and sub activities 
maj_act_codes<-unique(floor(act_codes/10000))
sub_act_codes<-unique(floor(act_codes/100))


#get the index of the activity codes 

ind_maj_act_codes<-0
ind_sub_act_codes<-0

for (i in 1:length(maj_act_codes$V1))
{
  ind_maj_act_codes[i]<-max(which(floor(act_codes/10000)==maj_act_codes$V1[i]))
  for (j in 1:length(sub_act_codes$V1))
  {
    ind_sub_act_codes[j]<-max(which(floor(act_codes/100)==sub_act_codes$V1[j]))
  }
}



###find index for each year 

file$years<-substr(file$TUCASEID,1,4)
id_year<-0
for(i in 1:length(years))
{
  id_year[i]<-max(which(file$years==years[i]))
}

###find index for each respondent for each year 
id_resp<-0

chk<-table(file$TUCASEID)
id_resp<-as.data.frame(chk)
rownames(id_resp)<-id_resp$Var1
id_resp<-cumsum(id_resp$Freq)


###identigying timesplit items and merging them 
TUACTDUR24R<-matrix(ncol = 1,nrow = length(file$TUCASEID))
id<-1:2

save_file<-file

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


####find descriptive statistics for each year 

mean_tottime_peractivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(act_codes$V1)))
rownames(mean_tottime_peractivity_peryear)<-act_codes$V1
colnames(mean_tottime_peractivity_peryear)<-years

sd_tottime_peractivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(act_codes$V1)))
rownames(sd_tottime_peractivity_peryear)<-act_codes$V1
colnames(sd_tottime_peractivity_peryear)<-years

median_tottime_peractivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(act_codes$V1)))
rownames(median_tottime_peractivity_peryear)<-act_codes$V1
colnames(median_tottime_peractivity_peryear)<-years

max_tottime_peractivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(act_codes$V1)))
rownames(max_tottime_peractivity_peryear)<-act_codes$V1
colnames(max_tottime_peractivity_peryear)<-years

min_tottime_peractivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(act_codes$V1)))
rownames(min_tottime_peractivity_peryear)<-act_codes$V1
colnames(min_tottime_peractivity_peryear)<-years

desstat_tottime_peractivity_allyears<-data.frame(matrix(ncol=5,nrow=length(act_codes$V1)))
rownames(desstat_tottime_peractivity_allyears)<-act_codes$V1
colnames(desstat_tottime_peractivity_allyears)<-c('min','median','max','mean','sd')


###descriptive stats per year for all activitites 
library("matrixStats")
for (j in 1:length(act_codes$V1))
{
  chk_allyear<-subset.data.frame(file,TRCODEP==act_codes$V1[j])
  
  desstat_tottime_peractivity_allyears$min[j]<-min(chk_allyear$TUACTDUR24,na.rm = TRUE)
  desstat_tottime_peractivity_allyears$median[j]<-weightedMedian(chk_allyear$TUACTDUR24,chk_allyear$weights, na.rm = TRUE)
  desstat_tottime_peractivity_allyears$max[j]<-max(chk_allyear$TUACTDUR24,na.rm = TRUE)
  desstat_tottime_peractivity_allyears$mean[j]<-weightedMean(chk_allyear$TUACTDUR24,chk_allyear$weights,na.rm = TRUE)
  desstat_tottime_peractivity_allyears$sd[j]<-weightedSd(chk_allyear$TUACTDUR24,chk_allyear$weights,na.rm = TRUE)
  
  for (i in 1:length(years))
  {
    chk<-subset.data.frame(file,TRCODEP==act_codes$V1[j] & file$years==year[i])
    median_tottime_peractivity_peryear[j,i]<-weightedMedian(chk$TUACTDUR24,chk$weights,na.rm=TRUE)
    mean_tottime_peractivity_peryear[j,i]<-weightedMean(chk$TUACTDUR24,chk$weights,na.rm=TRUE)
    sd_tottime_peractivity_peryear[j,i]<-weightedSd(chk$TUACTDUR24,chk$weights,na.rm=TRUE)
    min_tottime_peractivity_peryear[j,i]<-min(chk$TUACTDUR24,na.rm=TRUE)
    max_tottime_peractivity_peryear[j,i]<-max(chk$TUACTDUR24,na.rm=TRUE)
  }
}

library("xlsx")
wb<-createWorkbook()
desstat_allyears<-createSheet(wb,sheetName="desstat_allyears")
mean_peryear<-createSheet(wb,sheetName="mean_peryear")
median_peryear<-createSheet(wb,sheetName="median_peryear")
sd_peryear<-createSheet(wb,sheetName="sd_peryear")
min_peryear<-createSheet(wb,sheetName="min_peryear")
max_peryear<-createSheet(wb,sheetName="max_peryear")

addDataFrame(min_tottime_peractivity_peryear, min_peryear)
addDataFrame(median_tottime_peractivity_peryear, median_peryear)
addDataFrame(mean_tottime_peractivity_peryear, mean_peryear)
addDataFrame(sd_tottime_peractivity_peryear, sd_peryear)
addDataFrame(max_tottime_peractivity_peryear, max_peryear)
addDataFrame(desstat_tottime_peractivity_allyears, desstat_allyears)

saveWorkbook(wb,"descriptivestats.xlsx")


#initialize dataframes to identify descriptive states for sub activities 

mean_tottime_persubactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(sub_act_codes$V1)))
rownames(mean_tottime_persubactivity_peryear)<-sub_act_codes$V1
colnames(mean_tottime_persubactivity_peryear)<-years

sd_tottime_persubactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(sub_act_codes$V1)))
rownames(sd_tottime_persubactivity_peryear)<-sub_act_codes$V1
colnames(sd_tottime_persubactivity_peryear)<-years

median_tottime_persubactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(sub_act_codes$V1)))
rownames(median_tottime_persubactivity_peryear)<-sub_act_codes$V1
colnames(median_tottime_persubactivity_peryear)<-years

max_tottime_persubactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(sub_act_codes$V1)))
rownames(max_tottime_persubactivity_peryear)<-sub_act_codes$V1
colnames(max_tottime_persubactivity_peryear)<-years

min_tottime_persubactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(sub_act_codes$V1)))
rownames(min_tottime_persubactivity_peryear)<-sub_act_codes$V1
colnames(min_tottime_persubactivity_peryear)<-years

desstat_tottime_persubactivity_allyears<-data.frame(matrix(ncol=5,nrow=length(sub_act_codes$V1)))
rownames(desstat_tottime_persubactivity_allyears)<-sub_act_codes$V1
colnames(desstat_tottime_persubactivity_allyears)<-c('min','median','max','mean','sd')

#################majactivities

mean_tottime_permajactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(maj_act_codes$V1)))
rownames(mean_tottime_permajactivity_peryear)<-maj_act_codes$V1
colnames(mean_tottime_permajactivity_peryear)<-years

sd_tottime_permajactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(maj_act_codes$V1)))
rownames(sd_tottime_permajactivity_peryear)<-maj_act_codes$V1
colnames(sd_tottime_permajactivity_peryear)<-years

median_tottime_permajactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(maj_act_codes$V1)))
rownames(median_tottime_permajactivity_peryear)<-maj_act_codes$V1
colnames(median_tottime_permajactivity_peryear)<-years

max_tottime_permajactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(maj_act_codes$V1)))
rownames(max_tottime_permajactivity_peryear)<-maj_act_codes$V1
colnames(max_tottime_permajactivity_peryear)<-years

min_tottime_permajactivity_peryear<-data.frame(matrix(ncol=length(years), nrow=length(maj_act_codes$V1)))
rownames(min_tottime_permajactivity_peryear)<-maj_act_codes$V1
colnames(min_tottime_permajactivity_peryear)<-years

desstat_tottime_permajactivity_allyears<-data.frame(matrix(ncol=5,nrow=length(maj_act_codes$V1)))
rownames(desstat_tottime_permajactivity_allyears)<-maj_act_codes$V1
colnames(desstat_tottime_permajactivity_allyears)<-c('min','median','max','mean','sd')

###descriptive stats per year for maj  activitites 
library("matrixStats")
for (j in 1:length(maj_act_codes$V1))
{
  chk_allyear_maj<-subset.data.frame(file,TRTIER1P==maj_act_codes$V1[j])
  
  ###maj
  desstat_tottime_permajactivity_allyears$min[j]<-min(chk_allyear_maj$TUACTDUR24,na.rm = TRUE)
  desstat_tottime_permajactivity_allyears$median[j]<-weightedMedian(chk_allyear_maj$TUACTDUR24,chk_allyear_maj$weights, na.rm = TRUE)
  desstat_tottime_permajactivity_allyears$max[j]<-max(chk_allyear_maj$TUACTDUR24,na.rm = TRUE)
  desstat_tottime_permajactivity_allyears$mean[j]<-weightedMean(chk_allyear_maj$TUACTDUR24,chk_allyear_maj$weights,na.rm = TRUE)
  desstat_tottime_permajactivity_allyears$sd[j]<-weightedSd(chk_allyear_maj$TUACTDUR24,chk_allyear_maj$weights,na.rm = TRUE)
  
  
  for (i in 1:length(year))
  {
    #maj
    chk_maj<-subset.data.frame(file,TRTIER1P==maj_act_codes$V1[j] & file$years==year[i])
    median_tottime_permajactivity_peryear[j,i]<-weightedMedian(chk_maj$TUACTDUR24,chk_maj$weights,na.rm=TRUE)
    mean_tottime_permajactivity_peryear[j,i]<-weightedMean(chk_maj$TUACTDUR24,chk_maj$weights,na.rm=TRUE)
    sd_tottime_permajactivity_peryear[j,i]<-weightedSd(chk_maj$TUACTDUR24,chk_maj$weights,na.rm=TRUE)
    min_tottime_permajactivity_peryear[j,i]<-min(chk_maj$TUACTDUR24,na.rm=TRUE)
    max_tottime_permajactivity_peryear[j,i]<-max(chk_maj$TUACTDUR24,na.rm=TRUE)
    
   
  }
}

library("xlsx")
wb_maj<-createWorkbook()
desstat_allyears<-createSheet(wb_maj,sheetName="desstat_allyears")
mean_peryear<-createSheet(wb_maj,sheetName="mean_peryear")
median_peryear<-createSheet(wb_maj,sheetName="median_peryear")
sd_peryear<-createSheet(wb_maj,sheetName="sd_peryear")
min_peryear<-createSheet(wb_maj,sheetName="min_peryear")
max_peryear<-createSheet(wb_maj,sheetName="max_peryear")

addDataFrame(min_tottime_permajactivity_peryear, min_peryear)
addDataFrame(median_tottime_permajactivity_peryear, median_peryear)
addDataFrame(mean_tottime_permajactivity_peryear, mean_peryear)
addDataFrame(sd_tottime_permajactivity_peryear, sd_peryear)
addDataFrame(max_tottime_permajactivity_peryear, max_peryear)
addDataFrame(desstat_tottime_permajactivity_allyears, desstat_allyears)

saveWorkbook(wb_maj,"descriptivestats_maj.xlsx")


########descriptive stats for sub activities


###descriptive stats per year for maj and sub activitites 
library("matrixStats")
for (j in 1:length(sub_act_codes$V1))
{
  chk_allyear_sub<-subset.data.frame(file,TRTIER2P==sub_act_codes$V1[j])
  
  ###sub
  desstat_tottime_persubactivity_allyears$min[j]<-min(chk_allyear_sub$TUACTDUR24,na.rm = TRUE)
  desstat_tottime_persubactivity_allyears$median[j]<-weightedMedian(chk_allyear_sub$TUACTDUR24,chk_allyear_sub$weights, na.rm = TRUE)
  desstat_tottime_persubactivity_allyears$max[j]<-max(chk_allyear_sub$TUACTDUR24,na.rm = TRUE)
  desstat_tottime_persubactivity_allyears$mean[j]<-weightedMean(chk_allyear_sub$TUACTDUR24,chk_allyear_sub$weights,na.rm = TRUE)
  desstat_tottime_persubactivity_allyears$sd[j]<-weightedSd(chk_allyear_sub$TUACTDUR24,chk_allyear_sub$weights,na.rm = TRUE)
  
  
  for (i in 1:length(years))
  {
    
    #sub
    chk_sub<-subset.data.frame(file,TRTIER2P==sub_act_codes$V1[j] & file$years==year[i])
    median_tottime_persubactivity_peryear[j,i]<-weightedMedian(chk_sub$TUACTDUR24,chk_sub$weights,na.rm=TRUE)
    mean_tottime_persubactivity_peryear[j,i]<-weightedMean(chk_sub$TUACTDUR24,chk_sub$weights,na.rm=TRUE)
    sd_tottime_persubactivity_peryear[j,i]<-weightedSd(chk_sub$TUACTDUR24,chk_sub$weights,na.rm=TRUE)
    min_tottime_persubactivity_peryear[j,i]<-min(chk_sub$TUACTDUR24,na.rm=TRUE)
    max_tottime_persubactivity_peryear[j,i]<-max(chk_sub$TUACTDUR24,na.rm=TRUE)
    
  }
}

library("xlsx")
wb_sub<-createWorkbook()
desstat_allyears<-createSheet(wb_sub,sheetName="desstat_allyears")
mean_peryear<-createSheet(wb_sub,sheetName="mean_peryear")
median_peryear<-createSheet(wb_sub,sheetName="median_peryear")
sd_peryear<-createSheet(wb_sub,sheetName="sd_peryear")
min_peryear<-createSheet(wb_sub,sheetName="min_peryear")
max_peryear<-createSheet(wb_sub,sheetName="max_peryear")

addDataFrame(min_tottime_persubactivity_peryear, min_peryear)
addDataFrame(median_tottime_persubactivity_peryear, median_peryear)
addDataFrame(mean_tottime_persubactivity_peryear, mean_peryear)
addDataFrame(sd_tottime_persubactivity_peryear, sd_peryear)
addDataFrame(max_tottime_persubactivity_peryear, max_peryear)
addDataFrame(desstat_tottime_persubactivity_allyears, desstat_allyears)

saveWorkbook(wb_sub,"descriptivestats_sub.xlsx")










##initialize variables
index<-1:2
obs_count<-0

#run the loop to identify descriptive statistics per day 
for (i in 1:12)
{
  index[2]<-max(which(atussumm$TUYEAR==years[i]))
  
  for (j in 1:431)
  {
    k=j+24
    mean_tottime_peractivity_peryear[j,i]<-matrixStats::weightedMean(atussumm[index[1]:index[2],k],atussumm$TUFNWGTP[index[1]:index[2]],na.rm=TRUE)
    sd_tottime_peractivity_peryear[j,i]<-matrixStats::weightedSd(atussumm[index[1]:index[2],k],atussumm$TUFNWGTP[index[1]:index[2]],na.rm=TRUE)
    median_tottime_peractivity_peryear[j,i]<-matrixStats::weightedMedian(atussumm[index[1]:index[2],k],atussumm$TUFNWGTP[index[1]:index[2]],na.rm=TRUE)
    min_tottime_peractivity_peryear[j,i]<-min(atussumm[index[1]:index[2],k],na.rm=TRUE)
    max_tottime_peractivity_peryear[j,i]<-max(atussumm[index[1]:index[2],k],na.rm=TRUE)
  }
  obs_count[i]<-index[2]
  index[1]<-index[2]+1
}



