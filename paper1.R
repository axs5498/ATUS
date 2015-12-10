#R code of the ATUS paper 1 

#author: Ashok Sekar
#date: Nov 30 2015 


#necessary packages 
require(lubridate)
require(censReg)
require(survival)
require(matrixStats)

#input all year datasets 
setwd("~/Dropbox/Ongoing_Work/Time_Use/Inputs") #set input directory 
atussumm<-read.table("atussum_0314.dat",header=TRUE,sep=",")
atusact<-read.table("atusact_0314.dat",header=TRUE,sep=",")
atusresp<-read.table("atusresp_0314.dat",header=TRUE,sep=",")

#list of all activities with their descriptions 
allact_desc<-read.csv('allactivitycodes_description.csv')
subact_desc<-read.csv('subactivitycodes_description.csv')
majact_desc<-read.csv('majactivitycodes_description.csv')

#list of codes to change from activity file 
codestochange<-read.csv('codestochange.csv')


 


#edits to input data 
#codes needed 
atusact$year<-as.numeric(substr(atusact$TUCASEID,1,4))
atusact<-merge(x=atusact,y=atussumm[,c("TUCASEID","TUFNWGTP")],by='TUCASEID',all.x = TRUE)
atusact$year_edit<-atusact$year-2002
atussumm$year_edit<-atussumm$TUYEAR-2002


#finding total weights for each year
totweight_year<-matrix()
for (i in 1:12) 
{
  totweight_year[i]<-sum(atussumm$TUFNWGTP[atussumm$TUYEAR==years[i]])
}


#working with time data 
#adding a new variable to sort out the day issue with start and stop time 
atusact$dateformat2<-ifelse(atusact$TUCUMDUR24>1199,60*60*24,0) #code time 
atusact$dateformat1<-ifelse((atusact$TUCUMDUR24-atusact$TUACTDUR24)>1199,60*60*24,0) #code time 

#creating the new start and stop time
atusact$starttime<-as.POSIXct(atusact$TUSTARTTIM,format="%H:%M:%S")+atusact$dateformat1
atusact$stoptime<-as.POSIXct(atusact$TUSTOPTIME,format="%H:%M:%S")+atusact$dateformat2

#saving as intervals
atusact$act_interval<-as.interval(atusact$starttime,atusact$stoptime)

#custom time intervals, adjust values here for various time-intervals 
z<-c("04:00:00")
j<-0
time_interval<-NULL 
for (i in 1:25) #24 denotes interval of each hour plus one for 4:00 AM again 
{
  time_interval[i]<-as.POSIXct(z,format="%H:%M:%S")+j
  j<-j+60*60 # denotes number of seconds in each hour 
}
time_interval<-as.POSIXct(time_interval,origin = "1970-01-01") 





#finding time intervals within the dataset 
#logic: find if there is intersection, then calculate the difference 
#value<-int_overlaps(act_interval,as.interval(time_interval[1],time_interval[2]-1))

#list of activities/inputs
allact<-unique(atusact$TRCODEP)
majact<-unique(atusact$TRTIER1P)
subjact<-unique(atusact$TRTIER2P)
years<-unique(atusact$year)

#initializing inputs 
interval_mean<-data.frame()
interval_sd<-data.frame()
tot_prate<-data.frame()
temp_mean<-data.frame()
temp_prate<-data.frame()
temp_count<-data.frame()
list_mean<-list()
list_sd<-list()
list_prate<-list()
list_tobit<-list()
list_list_tobit<-list()
list_id_year<-list()
list_id<-list()
list_count<-list()


#the main loop 
for (a in 1:length(allact)) #change the number of activities 
{
  mainFile<-subset(atusact,TRCODEP==allact[a])
  for (y in 1:length(years)) 
  {
    fileYear<-subset(mainFile,year==years[y])
    #tot_mean[y,1]<-weighted.mean(file$TUACTDUR24,file$TUFNWGTP)
    tot_prate[y,1]<-sum(fileYear$TUFNWGTP[fileYear$TUACTDUR24!=0])/sum(fileYear$TUFNWGTP)
    
    for (t in 1:(length(time_interval)-1))
    {
      #mean and sd of duration of each activity
      ##list id of person whose activity matched within the time interval
      id_year<-int_overlaps(fileYear$act_interval,as.interval(time_interval[t],time_interval[t+1]-1)) #id that provides when time interval intersects  
      ##duration of the activity within the time interval 
      temp1<-as.difftime(intersect(as.interval(time_interval[t],time_interval[t+1]-1),fileYear$act_interval))
      temp1<-as.numeric(temp1,units="mins")
      ##mean of the duration in each interval 
      interval_mean[y,t]<-weighted.mean(temp1[id_year],fileYear$TUFNWGTP[id_year])
      interval_sd[y,t]<-weightedSd(temp1[id_year],fileYear$TUFNWGTP[id_year])
      #calculating participation rate and count
      temp_count[y,t]<-length(fileYear$TUCASEID[id_year])
      temp_prate[y,t]<-sum(fileYear$TUFNWGTP[id_year])/sum(fileYear$TUFNWGTP)
    }
  }
  interval_mean$tot<-rowSums(interval_mean)
  #interval_sd$tot<-rowSums(interval_Sd^2)
  temp_prate$tot<-tot_prate$V1
  temp_count$tot<-rowSums(temp_count)
  
  list_count[[a]]<-temp_count
  list_sd[[a]]<-interval_sd
  list_mean[[a]]<-interval_mean
  list_prate[[a]]<-temp_prate
}


#saving files with appropriate names 
allactivities_count<-list_count
allactivities_prate<-list_prate
allactivities_mean<-list_mean
allactivities_sd<-list_sd


save.image(file = "~/Dropbox/Ongoing_Work/Time_Use/Paper1/paper1.Rdata")

#------------------------------------------------------------------------------------
#home analysis 
#homeFile<-atusact[atusact$TEWHERE %in% c(-1,1,3),] #selecting activities conducted at home

#initializing inputs 
interval_mean<-data.frame()
interval_sd<-data.frame()
tot_mean<-data.frame()
tot_prate<-data.frame()
temp_mean<-data.frame()
temp_prate<-data.frame()
temp_count<-data.frame()
list_mean<-list()
list_sd<-list()
list_prate<-list()
list_tobit<-list()
list_list_tobit<-list()
list_id_year<-list()
list_id<-list()
list_count<-list()

temp<-c(-1,1,3)
for (a in 1:3) #change the number of activities 
{
  homeFile<-atusact[atusact$TEWHERE %in% temp[a],]
  for (y in 1:length(years)) 
  {
    fileYear<-subset(homeFile,year==years[y])
    #tot_mean[y,a]<-weighted.mean(fileYear$TUACTDUR24,fileYear$TUFNWGTP)
    tot_prate[y,1]<-sum(fileYear$TUFNWGTP[fileYear$TUACTDUR24!=0])/sum(fileYear$TUFNWGTP)
    
    for (t in 1:(length(time_interval)-1))
    {
      #mean and sd of duration of each activity
      ##list id of person whose activity matched within the time interval
      id_year<-int_overlaps(fileYear$act_interval,as.interval(time_interval[t],time_interval[t+1]-1)) #id that provides when time interval intersects  
      ##duration of the activity within the time interval 
      temp1<-as.difftime(intersect(as.interval(time_interval[t],time_interval[t+1]-1),fileYear$act_interval))
      temp1<-as.numeric(temp1,units="mins")
      ##mean of the duration in each interval 
      interval_mean[y,t]<-sum(temp1[id_year]*fileYear$TUFNWGTP[id_year])/totweight_year[y]
      interval_sd[y,t]<-weightedSd(temp1[id_year],fileYear$TUFNWGTP[id_year])
      #calculating participation rate and count
      temp_count[y,t]<-length(fileYear$TUCASEID[id_year])
      temp_prate[y,t]<-sum(fileYear$TUFNWGTP[id_year])/sum(fileYear$TUFNWGTP)
    }
  }
  #interval_mean$tot<-rowSums(interval_mean)
  #interval_sd$tot<-rowSums(interval_Sd^2)
  #temp_prate$tot<-tot_prate$V1
  #temp_count$tot<-rowSums(temp_count)
  
  list_count[[a]]<-temp_count
  list_sd[[a]]<-interval_sd
  list_mean[[a]]<-interval_mean
  list_prate[[a]]<-temp_prate
}

#saving files with appropriate names 
homeactivities_count<-list_count
homeactivities_prate<-list_prate
homeactivities_mean<-list_mean
homeactivities_sd<-list_sd




##for now do a heat map for time spent at home 

chk1<-homeactivities_mean[[1]]+homeactivities_mean[[2]]+homeactivities_mean[[3]]
rownames(chk1)<-c(2003:2014)
colnames(chk1)<-c('4-5AM','5-6AM','6-7AM','7-8AM','8-9AM','9-10AM','10-11AM','11-12PM','12-1PM','1-2PM','2-3PM','3-4PM','4-5PM','5-6PM','6-7PM','7-8PM','8-9PM','9-10PM','10-11PM','11-12AM','12-1AM','1-2AM','2-3AM','3-4AM')
chk1$Total<-rowSums(chk1)

chk1_matrix<-data.matrix(chk1)
heatmap(chk1_matrix,Rowv=NA,Colv=NA,col=heat.colors(256),scal="column",xlab = 'time',ylab = 'year',main = 'Heat map of time spent at home')

##do a heat map for 


for (a in 1:length(allact))
{
  
  mainFile<-subset(atusact,TRCODEP==allact[a])
  
  for (t in 1:(length(time_interval)-1))
  {
    #calculating tobit
    ##list of id of respondents whose activity matched within the time interval 
    id<-int_overlaps(mainFile$act_interval,as.interval(time_interval[t],time_interval[t+1]-1)) #id that provides when time interval intersects  
    ##duration of the activitiy within the time interval 
    temp2<-as.difftime(intersect(as.interval(time_interval[t],time_interval[t+1]-1),mainFile$act_interval))
    temp2<-as.numeric(temp2,units="hours")
    
    temp3<-survreg(Surv(temp2[id],temp2[id]>0,type = "left") ~ year_edit[id], 
                   weights = TUFNWGTP[id], data = mainFile, dist = "gaussian")
    list_tobit[[t]]<-summary(temp3)$table
    
#     ##calculate the tobit 
#     if (sum(temp2[id]==0))
#     {
#       list_tobit[[t]]<-c('all values were zero')
#     }
#     else 
#     {
#       temp3<-survreg(Surv(temp2[id],temp2[id]>0,type = "left") ~ year_edit[id], 
#                      weights = TUFNWGTP[id], data = mainFile, dist = "gaussian")
#       list_tobit[[t]]<-summary(temp3)$table
#     }
  }
  
  #list_list_tobit[[a]]<-list_tobit
  
  rm(mainFile)
}



#list of activities done at home
#for each activity what is the % of instance done at each location? 
phome<-data.frame()
newFile<-atusact[c("TEWHERE") %in% c(-1,1),]
for (a in 1:length(allact))
{
  phome[a]<-round(dim(newFile[c("TRCODEP") %in% allact[a],])[1]/dim(atusact[c("TEWHERE") %in% allact[a],])[1],2)
}


#code to quantify important activities 
##what are important activities? their mean should be relatively big, participation rate also big, 
##slope is also big, activity should be conducted at home


#analysis to get presentable result 

