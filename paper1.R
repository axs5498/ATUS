#R code of the ATUS paper 1 

#author: Ashok Sekar
#date: Nov 30 2015 


#necessary packages 
require(lubridate)
require(censReg)

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





#working with time data 
#adding a new variable to sort out the day issue with start and stop time 
atusact$dateformat<-ifelse(atusact$TUCUMDUR24>1199,60*60*24,0) #code time

#creating the new start and stop time
atusact$starttime<-as.POSIXct(atusact$TUSTARTTIM,format="%H:%M:%S")+atusact$dateformat
atusact$stoptime<-as.POSIXct(atusact$TUSTOPTIME,format="%H:%M:%S")+atusact$dateformat

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
tot_mean<-data.frame()
tot_prate<-data.frame()
temp_mean<-data.frame()
temp_prate<-data.frame()
list_mean<-list()
list_prate<-list()

#the main loop 
for (a in 1:length(allact)) #change the number of activities 
{
  file1<-subset(atusact,TRCODEP==allact[a])
  for (y in 1:length(years)) 
  {
    file<-subset(file1,year==years[y])
    tot_mean[y,1]<-weighted.mean(file$TUACTDUR24,file$TUFNWGTP)
    tot_prate[y,1]<-sum(file$TUFNWGTP[file$TUACTDUR24!=0])/sum(file$TUFNWGTP)
    
    for (t in 1:(length(time_interval)-1))
    {
      #mean and prate calculation 
      id<-int_overlaps(file$act_interval,as.interval(time_interval[t],time_interval[t+1]-1)) #id that provides when time interval intersects  
      temp1<-as.difftime(intersect(as.interval(time_interval[t],time_interval[t+1]-1),file$act_interval))
      temp1<-as.numeric(temp1,units="mins") 
      temp_mean[y,t]<-weighted.mean(temp1[id],file$TUFNWGTP[id])
      temp_prate[y,t]<-sum(file$TUFNWGTP[id])/sum(file$TUFNWGTP) 
    }
  }
  temp_mean$tot<-tot_mean$V1
  temp_prate$tot<-tot_prate$V1
  
  list_mean[[a]]<-temp_mean
  list_prate[[a]]<-temp_prate
}


save.image(file = "~/Dropbox/Ongoing_Work/Time_Use/Paper1/paper1.Rdata")

