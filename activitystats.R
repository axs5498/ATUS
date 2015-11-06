#clear all variables in workspace
rm(list = ls())
#this is a comment line 
#set input direcotry
setwd("~/Dropbox/Ongoing_Work/Time_Use/Inputs")

####load file

atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")


atussumm[atussumm==0]<-NA
#set result directory
setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/")

#defining global variables
years<-2003:2014

############min, median and max results 
#stats of all activities not accounnting for frequency 

#selecting the subset of data for processing
tottime_peractivity<-(atussumm[,25:455])


####descriptive statistics:::: the aim of this part is to identify the the 
####following descriptive statistics of the dataset for each year

# total time spend doing activity:  mean, median, SD, min, max 
# total time spent of each freq of the activity: mean, median, SD, min, max 
#number of people performing each activity, no of times each activity is performed


#initialize dataframes 

min_tottime_peractivity_peryear<-data.frame(matrix(ncol=12, nrow=431))
rownames(min_tottime_peractivity_peryear)<-colnames(tottime_peractivity)
colnames(min_tottime_peractivity_peryear)<-years

mean_tottime_peractivity_peryear<-data.frame(matrix(ncol=12, nrow=431))
rownames(mean_tottime_peractivity_peryear)<-colnames(tottime_peractivity)
colnames(mean_tottime_peractivity_peryear)<-years

sd_tottime_peractivity_peryear<-data.frame(matrix(ncol=12, nrow=431))
rownames(sd_tottime_peractivity_peryear)<-colnames(tottime_peractivity)
colnames(sd_tottime_peractivity_peryear)<-years

median_tottime_peractivity_peryear<-data.frame(matrix(ncol=12, nrow=431))
rownames(median_tottime_peractivity_peryear)<-colnames(tottime_peractivity)
colnames(median_tottime_peractivity_peryear)<-years

max_tottime_peractivity_peryear<-data.frame(matrix(ncol=12, nrow=431))
rownames(max_tottime_peractivity_peryear)<-colnames(tottime_peractivity)
colnames(max_tottime_peractivity_peryear)<-years

desstat_tottime_peractivity_allyears<-data.frame(matrix(ncol=5,nrow=431))
rownames(desstat_tottime_peractivity_allyears)<-colnames(tottime_peractivity)
colnames(desstat_tottime_peractivity_allyears)<-c('min','median','max','mean','sd')

##initialize variables
index<-1:2
obs_count<-0

#run the loop to identify descriptive statistics per day 
for (j in 1:431)
{
  k=j+24
  
  desstat_tottime_peractivity_allyears$min<-min()
  desstat_tottime_peractivity_allyears$median<-
  desstat_tottime_peractivity_allyears$max<-
  desstat_tottime_peractivity_allyears$mean<-
  desstat_tottime_peractivity_allyears$sd<-
    
    
    
  for (i in 1:12)
    {
    index[2]<-max(which(atussumm$TUYEAR==years[i]))
    
    mean_tottime_peractivity_peryear[j,i]<-matrixStats::weightedMean(atussumm[index[1]:index[2],k],atussumm$TUFNWGTP[index[1]:index[2]],na.rm=TRUE)
    sd_tottime_peractivity_peryear[j,i]<-matrixStats::weightedSd(atussumm[index[1]:index[2],k],atussumm$TUFNWGTP[index[1]:index[2]],na.rm=TRUE)
    median_tottime_peractivity_peryear[j,i]<-matrixStats::weightedMedian(atussumm[index[1]:index[2],k],atussumm$TUFNWGTP[index[1]:index[2]],na.rm=TRUE)
    min_tottime_peractivity_peryear[j,i]<-min(atussumm[index[1]:index[2],k],na.rm=TRUE)
    max_tottime_peractivity_peryear[j,i]<-max(atussumm[index[1]:index[2],k],na.rm=TRUE)
    }
  obs_count[i]<-index[2]
  index[1]<-index[2]+1
}


library("xlsx")
wb_sub<-createWorkbook()
desstat_allyears<-createSheet(wb_sub,sheetName="desstat_allyears")
mean_peryear<-createSheet(wb_sub,sheetName="mean_peryear")
median_peryear<-createSheet(wb_sub,sheetName="median_peryear")
sd_peryear<-createSheet(wb_sub,sheetName="sd_peryear")
min_peryear<-createSheet(wb_sub,sheetName="min_peryear")
max_peryear<-createSheet(wb_sub,sheetName="max_peryear")

addDataFrame(min_tottime_peractivity_peryear, min_peryear)
addDataFrame(median_tottime_peractivity_peryear, median_peryear)
addDataFrame(mean_tottime_peractivity_peryear, mean_peryear)
addDataFrame(sd_tottime_peractivity_peryear, sd_peryear)
addDataFrame(max_tottime_peractivity_peryear, max_peryear)
addDataFrame(desstat_tottime_peractivity_allyears, desstat_allyears)

saveWorkbook(wb_sub,"descriptivestats_perday_allactivity.xlsx")











############# quantify linear fit of the data. time ~ years 
allactivity_lm<-data.frame(matrix(ncol=4 ,nrow=431))
rownames(allactivity_lm)<-colnames(tottime_peractivity)
colnames(allactivity_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

###find the fit 
for (i in 1:length(colnames(tottime_peractivity)))
{
  #to identify the number of "NA" in the dataset  
  chk<-is.na(tottime_peractivity[,i])
  value=length(chk[chk==FALSE])
  
  if(value>100) #random statement we can change this to be more complex. 
  {
    fit<-lm(tottime_peractivity[,i]~atussumm$TUYEAR) # linear fit 
  
    #collecting the fitted values
    allactivity_lm[i,1]<-summary(fit)$coef[2,1]
    allactivity_lm[i,2]<-summary(fit)$coef[2,2]
    allactivity_lm[i,3]<-summary(fit)$coef[2,3]
    allactivity_lm[i,4]<-summary(fit)$adj.r.squared
    
  }
  else
  {
    fit<-0
  }
}



#####operations with the activity file 
### to quantify frequency of activity, des. stat for each frequency

uniq_caseid<-unique(atusact$TUCASEID)#finding the unique id 

#####this is not needed have saved varible as activity codes 
# #creating new variable that lists activity codes without "t" 
# act_codes<-substr(colnames(tottime_peractivity),2,7)
# f_act_codes<-substr(act_codes,1,1)
# id<-max(which(f_act_codes=="0"))
# act_codes[1:id]<-substr(act_codes[1:id],2,6)
# class(act_codes)<-'numeric'
# act_codes<-as.data.frame(act_codes)
# write.table(act_codes,file="activitycodes",sep = ",",row.names = FALSE,col.names = FALSE)

####loop to find descriptive statistics per day per instance of activity 

###first list the number of instances per year 
k<-1
freq_act_peryear<-data.frame(matrix(ncol=12, nrow=431))
rownames(freq_act_peryear)<-colnames(tottime_peractivity)
colnames(freq_act_peryear)<-years

id[1]<-1
for(i in obs_count) # for each year
{
  
  id[2]<-max(which(atusact$TUCASEID==actsumm$TUCASEID[i]))
  m<-1
  for(j in act_codes) #for each activity
  {
    #the number of times the activity was reported excluding time split
    freq_act_peryear[m,k]<-sum(atusact$TRCODEP[id[1]:id[2]]==j)
    m<-m+1
    # the average, min and max duration of the activity 
  
    #number of people who performed the activity 
  }
  k<-k+1
  id[1]<-id[2]+1
}


####correct the number of instance with time split 

number_activity_perperson<-data.frame(matrix(ncol=12, nrow=length(unique(file$TRCODEP))))
rownames(number_activity_perperson)<-colnames(unique(file$TRCODEP))
colnames(number_activity_perperson)<-years

id<-1:2
j<-1
m<-1
timesplit<-0

for(i in length(years)) # for each year
{
  id[2]<-max(which(atusact$TUCASEID==actsumm$TUCASEID[obs_count[i]]))  #find the id for each year in activity file 
  
  m<-1
  for (k in j:obs_count[i]) # for each person in that year k = number of unique case id. 
  {
    number_activity_perperson[i,m]=sum(atusact$TUCASEID[id[1]:id[2]]==actsumm$TUCASEID[k]) #count the unique values
    m<-m+1
  }
  j<-obs_count[i]+1 #reintialize j with the starting index for the next year
}


#for each person 
  find the id for each person 
  check if the ids of first and last activity same 
  paste activity name 
  
  





for(i in 1:length(caseid_count$cumfreq))
{
  id[2]<-caseid_count$cumfreq[i]
  if(atusact$TRCODEP[id[1]]==atusact$TRCODEP[id[2]])
  {
    timesplit[j]<-atusact$TRCODEP[id[1]]
    j=j+1
  }
  id[1]<-id[2]+1
}
timesplit_stat<-as.data.frame(table(timesplit))

##correction 





###output of all the descriptive statistics
sink("results_peractivity_allyears.txt")
mean_tottime_peractivity_peryear
sink(file=NULL)


