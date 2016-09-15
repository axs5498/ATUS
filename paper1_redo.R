#PAPER 1 SCRIPT 
#REDO AFTER FORMATING
#AUTHOR: ASHOK SEKAR

#codes necessary 
require(reshape)
require(plyr)
require(dplyr)
require(matrixStats)
require(lubridate)
require(reshape2)



#developing a dataset that is similar to activityds.day no whereds.day is needed. 

activityds.day<-data.frame()

##dataset needed 
setwd("~/Dropbox/Ongoing_Work/Time_Use/Inputs")
atusact<-read.table("atusact_0314.dat",header=TRUE,sep=",")
atussum<-read.table("atussum_0314.dat",header=TRUE,sep=",")
atusresp<-read.table("atusresp_0314.dat",header=TRUE,sep=",")

#dataset preperation 
##find code errors and correct them 
##remove unnecessary columns
##converting time to workable format for further processing 
##convert the input into multiple dataset for easy processing 

##saving the original file in another file 
ds<-atusact

##converting time to workable format 
require(lubridate)
###adding a new variable to sort out the day issue with start and stop time
###1199 equals 20hours i.e. 4AM - 11:50PM
ds$dateformat2<-ifelse(ds$TUCUMDUR24>1199,60*60*24,0) #stopping minute
ds$dateformat1<-ifelse((ds$TUCUMDUR24-ds$TUACTDUR24)>1199,60*60*24,0) #starting minute 
###creating the new start and stop time
ds$starttime<-as.POSIXct(ds$TUSTARTTIM,format="%H:%M:%S")+ds$dateformat1
ds$stoptime<-ds$starttime+(ds$TUACTDUR24*60)
###saving as intervals
ds$act_interval<-as.interval(ds$starttime,ds$stoptime)


##creating time intervals for creating the hour dataset 
###custom time intervals, adjust values here for various time-intervals 
###Note: this piece of code always has to run on the same day as the code
###where we created new start and stoptime 
z<-c("04:00:00")
j<-0
time_interval<-NULL 
for (i in 1:25) #24 denotes interval of each hour plus one for 4:00 AM again 
{
  time_interval[i]<-as.POSIXct(z,format="%H:%M:%S")+j
  j<-j+60*60 # denotes number of seconds in each hour 
}
time_interval<-as.POSIXct(time_interval,origin = "1970-01-01") 


##find the codes and correcting them 
#list of codes to change from activity file 
codestochange<-read.csv('codestochange.csv')
codestochange$replace<-rowSums(codestochange[,3:4],na.rm = TRUE,dims = 1)
#changing the error codes in the atusact file 
#the codes in the atussumm file is not touched right now. 
for (i in 1:length(codestochange$X))
{
  id<-which(ds$TRCODEP==codestochange$codes_NA[i])
  #codestochange$numbers[i]<-length(id)
  if (codestochange$replace[i]!=0)
  {
    ds$TRCODEP[id]<-codestochange$replace[i]
  }
}


##removing unnecessary columns 
necessary.colnames<-c("TUCASEID","TUACTIVITY_N","TRCODEP","TUACTDUR24",
                      "TUCUMDUR24","TEWHERE","starttime","stoptime",
                      "act_interval")
ds<-ds[,c(necessary.colnames)]
ds$year<-as.numeric(substr(ds$TUCASEID,1,4))
years<-unique(ds$year)

## code for creating the new category variables 
res.code<-c(-1,1,3) #blank, home and other's home
travel.code<-c(12,13)   #(12:21,99) # all travel 
commercial.code<-c(2,4,5,6,7,8,9,10,11,30,31,32) # Non-residential location
other<-c(-3,-2,89,99,14:21) #unspecified and not coded
#othertravel.code<-c(99,14:21) # other travel codes 
ds$newwhere<-NA 
ds$newwhere[ds$TEWHERE %in% res.code]<-"Residential"
ds$newwhere[ds$TEWHERE %in% travel.code]<-"Transportation"
ds$newwhere[ds$TEWHERE %in% commercial.code]<-"Non_Residential"
ds$newwhere[ds$TEWHERE %in% other.code]<-"Other"
#ds$newwhere[ds$TEWHERE %in% othertravel.code]<-"Other_Travel_Modes"




##code for activityds.day 
necessary.colnames<-c('TUCASEID','TUACTDUR24','TRCODEP','TEWHERE')

id<-cumsum(table(ds$year))
j<-1
chk<-list()
for (i in 1:10)
{
  chk[[i]]<-cast(ds[j:id[i],necessary.colnames],
                 TUCASEID~TRCODEP,value = "TUACTDUR24",sum)
  j<-id[i]+1
}
###add missing columns and create a new dataset and rbind it 
activityds.day<-data.frame()
allact<-sort(unique(ds$TRCODEP)) 
colnames.activityds.day<-c("TUCASEID",as.character(allact))
activityds.day<-data.frame(matrix(ncol = length(colnames.activityds.day)))
colnames(activityds.day)<-colnames.activityds.day
###CODE FOR ACTIVITIES
for (i in 1:10)
{
  missing.Cols<-setdiff(colnames.activityds.day,colnames(chk[[i]]))
  chk[[i]][,c(missing.Cols)]<-NA
  activityds.day<-rbind(activityds.day,chk[[i]])
}
activityds.day<-activityds.day[-1,]

# somemore formating before output 
#formating the new datasets
activityds.day[is.na(activityds.day)]<-0
require(reshape)
#formating day datasets do not rerun this without checkign the inputs and outputs 
##adding year and melting 
activityds.day$year<-NULL 
chk<-melt(activityds.day,id.vars = c("TUCASEID")) 
activityds.day<-chk
activityds.day$year<-as.numeric(substr(activityds.day$TUCASEID,1,4))
##merging to add weight files 
activityds.day$TUFNWGTP<-atussum$TUFNWGTP[match(activityds.day$TUCASEID,atussum$TUCASEID)]
##renaming 
activityds.day<-rename(activityds.day,c("TRCODEP"="variable",'TUFNWGTP'='weight'))


#filtering and generating the statistics file 
necessary.colnames<-c('TUCASEID','TUFNWGTP','TELFS','TULAY','TULK','TUABSOT','TURETOT','TEAGE')
filter.file<-atusresp[,necessary.colnames]
filter.file$emp<-ifelse(filter.file$TELFS<3,1,0)
filter.file$unemp<-ifelse((filter.file$TELFS<5&filter.file$TELFS>2),1,0)
filter.file$retired<-ifelse(filter.file$TELFS==5&filter.file$TURETOT==1,1,0)
filter.file$notinlaborforce<-ifelse(filter.file$TELFS==5&filter.file$TURETOT!=1,1,0)
filter.file$all<-rep(1,length(filter.file$TUCASEID))

filter.file<-merge(x=filter.file,y=atussum[,c("TUCASEID","TEAGE")],by.x = "TUCASEID",all.x = TRUE)
filter.file$age.lt18<-ifelse(filter.file$TEAGE<18,1,0)
filter.file$age.18to24<-ifelse(filter.file$TEAGE>=18&filter.file$TEAGE<25,1,0)
filter.file$age.25to34<-ifelse(filter.file$TEAGE>=25&filter.file$TEAGE<35,1,0)
filter.file$age.35to44<-ifelse(filter.file$TEAGE>=35&filter.file$TEAGE<45,1,0)
filter.file$age.45to54<-ifelse(filter.file$TEAGE>=45&filter.file$TEAGE<55,1,0)
filter.file$age.55to64<-ifelse(filter.file$TEAGE>=55&filter.file$TEAGE<65,1,0)
filter.file$age.ge65<-ifelse(filter.file$TEAGE>=65,1,0)

filter<-c('emp','unemp','retired','notinlaborforce','all',"age.lt18","age.18to24","age.25to34","age.35to44",
          "age.45to54","age.55to64","age.ge65")

ls<-list(activity = activityds.day) #the list provision is left for adding other files to do the similar analysis.

#generating the statistics 
desc.stat<-data.frame()
linearModel<-data.frame()
temp.desc.stat<-data.frame()


for (i in 1)
{
  ds1<-ls[[i]]
  
  for (j in filter)
  {
    
    #creating the new datast 
    id<-filter.file$TUCASEID[filter.file[,j]==1]
    ds<-ds1[ds1$TUCASEID %in% id,]
    
    
    ###code to calculate the yearly statistics 
    require(plyr)
    require(matrixStats)
    temp<-ddply(ds,.(year,variable),summarize,
                n.mean = round(weightedMean(value,weight),2),
                #p.mean = round(weightedMean(value[value!=0],weight[value!=0]),2),
                n.sd = round(weightedSd(value,weight),2),
                #p.sd = round(weightedSd(value[value!=0],weight[value!=0]),2),
                n.prate = round(sum(weight[value!=0])/sum(weight),2),
                p.min = ifelse(length(value[value!=0])==0,NA,
                               min(value[value!=0])),
                p.max = ifelse(length(value[value!=0])==0,NA,
                               max(value[value!=0])) 
    )
    
    require(reshape)
    temp.desc.stat<-melt(temp,id.vars=c("year","variable"))
    colnames(temp.desc.stat)<-c('year','variable','stat.type','value')
    temp.desc.stat$value<-round(temp.desc.stat$value,2)
    temp.desc.stat$data.type<-names(ls[i])
    temp.desc.stat$demog<-j
    
    #saving
    desc.stat<-rbind(desc.stat,temp.desc.stat)
    rm(temp.desc.stat)
  }
}

#adding all the names and variables 
#loading the files 
majact_desc<-read.csv("majactivitycodes_description.csv")
subact_desc<-read.csv("subactivitycodes_description.csv")
allact_desc<-read.csv("allactivitycodes_description.csv")

#merging them
stats<-desc.stat

stats<-merge
stats$majact_desc<-atussum$TUFNWGTP[match(activityds.day$TUCASEID,atussum$TUCASEID)]








