#Author: Ashok Sekar
#Date Created: Jan 05 2016

#Defining the outputs
##The output of the dataset preperation will be 6 files. 
##3 files for activities and 3 files for wherestats. later i need to create
## locations x 3 files for activitylocation combination stats. 
## Note that in each list there are going to 12 dataframes for each year

activityds.instance<-data.frame()
activityds.day<-data.frame()
activityds.hour<-data.frame()
whereds.instance<-data.frame()
whereds.day<-data.frame()
whereds.hour<-data.frame()


##dataset needed 
atusact<-read.table("atusact_0314.dat",header=TRUE,sep=",")


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


#Generating the dataset 
##converting the initial dataset into 12 different files for years 
##gives the activiyds.instance
ds$year<-as.numeric(substr(ds$TUCASEID,1,4))
years<-unique(ds$year)
activityds.instance<-ds


##code to generate the whereds.instance 
unique.id<-unique(ds$TUCASEID)
seq.length<-NULL
seq.id<-NULL
for (i in 1:length(unique.id))
{
  id<-which(ds$TUCASEID==unique.id[i]) 
  temp<-rle(ds[id,]$TEWHERE) 
  seq.length<-cbind(seq.length,t(temp$lengths))
  seq.id<-cbind(seq.id,t(temp$values)) 
  temp<-NULL 
}
##use the id values to make a small dataset
cumseq.length<-cumsum(seq.length[1,])
temp.whereds.instance<-data.frame(matrix(nrow = length(cumseq.length),ncol = 3))
colnames(temp.whereds.instance)<-c('TUCASEID','TUACTIVITY_N','TUACTDUR24')
j<-1

for (i in 413008:length(cumseq.length))
{
  temp.whereds.instance[i,]<-c(unique(ds$TUCASEID[j:cumseq.length[i]]),
                               min(ds$TUACTIVITY_N[j:cumseq.length[i]]),
                               sum(ds$TUACTDUR24[j:cumseq.length[i]]))
  
  j<-cumseq.length[i]+1
}
whereds.instance<-merge(x=temp.whereds.instance,y=ds,
                        by = c("TUCASEID","TUACTIVITY_N"),all.x = TRUE)

unique.length<-unique(seq.length[1,])
temp1<-data.frame()
temp2<-data.frame()
whereds1.instance<-data.frame()
for (i in unique.length)
{
  new.id<-cumseq.length[seq.length[1,]==i]
  if (i>1)
  {
    temp.new.id<-new.id
    zz<-NULL 
    for (k in 1:i)
    {
      z<-temp.new.id-k+1
      zz<-c(zz,z)
    }
    
    new.id<-zz
     #rm(zz)
     new.ds<-ds[new.id,c('TUCASEID','TUACTIVITY_N','TUACTDUR24','TEWHERE','starttime')]
     new.ds<-new.ds[order(new.ds$TUCASEID,new.ds$TUACTIVITY_N),]
     new.ds$TUCASEID<-format(new.ds$TUCASEID,digits = 10)
     new.ds$agg.id<-rep(c(1:i),length(new.ds$TUCASEID)/i)
     new.ds$NEWTUACT<-new.ds$TUACTIVITY_N-new.ds$agg.id
     
     temp1<-aggregate(new.ds$TUACTDUR24, 
                      by=list(TUCASEID=new.ds$TUCASEID,
                              NEWTUACT=new.ds$NEWTUACT), 
                      FUN=sum)
     temp2<-aggregate(new.ds$starttime, 
                      by=list(TUCASEID=new.ds$TUCASEID,
                              NEWTUACT=new.ds$NEWTUACT), 
                      FUN=min)
     temp3<-merge(x=temp1,y=temp2,by=c("TUCASEID","NEWTUACT"))
     temp3$NEWTUACT<-temp3$NEWTUACT+1
     temp3<-rename(temp3,c("NEWTUACT"="TUACTIVITY_N","x.x"="TUACTDUR24","x.y"="starttime"))
     temp3<-merge(x=temp3,y=ds[,c('TUCASEID','TUACTIVITY_N','TEWHERE')],all.x = TRUE)
     
     temp1.whereds.instance<-temp3
     
  }
  else
  {
    new.ds<-ds[new.id,c('TUCASEID','TUACTIVITY_N','TUACTDUR24','TEWHERE','starttime')]
    temp1.whereds.instance<-new.ds
  }
  
  whereds1.instance<-rbind(whereds1.instance,temp1.whereds.instance)
      
  #rm(new.id,temp1.whereds.instance,temp1,temp2,temp3)
}

whereds1.instance$stoptime<-whereds1.instance$starttime+whereds1.instance$TUACTDUR24*60
whereds1.instance$act_interval<-as.interval(whereds1.instance$starttime,
                                            whereds1.instance$stoptime)
whereds1.instance$year<-as.numeric(substr(whereds1.instance$TUCASEID,1,4))
whereds.instance<-whereds1.instance 


##code for activityds.day and locationds.day
require(reshape)
necessary.colnames<-c('TUCASEID','TUACTDUR24','TRCODEP','TEWHERE')

id<-cumsum(table(ds$year))
j<-1
for (i in 1:12)
{
  chk[[i]]<-cast(activityds.instance[j:id[i],necessary.colnames],
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
for (i in 1:12)
{
  missing.Cols<-setdiff(colnames.activityds.day,colnames(chk[[i]]))
  chk[[i]][,c(missing.Cols)]<-NA
  activityds.day<-rbind(activityds.day,chk[[i]])
}
activityds.day<-activityds.day[-1,]
###CODE FOR WHERE
whereds.day<-cast(activityds.instance[,necessary.colnames],
                  TUCASEID~TEWHERE,value = "TUACTDUR24",sum)
whereds.day$year<-as.numeric(substr(whereds.day$TUCASEID,1,4))

whereds.day<-whereds.day[,-1]
activityds.day<-activityds.day[,-1]
activityds.day[is.na(temp.activityds.day)]<-0
whereds.day[is.na(temp.whereds.day)]<-0




##code for activityds.hour and whereds.hour
activityds.hour<-data.frame()
temp.activityds.hour<-data.frame()
temp.actyearFile<-data.frame()

for (y in 1:length(years)) 
{
  temp.actyearFile<-subset(activityds.instance,year==years[y])#year filter 
  for (t in 1:(length(time_interval)-1))#interval filter 
  {
    ##list id of person whose activity matched within the time interval
    id.caseid<-int_overlaps(temp.actyearFile$act_interval,
                            as.interval(time_interval[t],
                                        time_interval[t+1]-1)) 
    ##duration of the activity within the time interval 
    temp.actyearFile$int.time<-as.difftime(intersect(as.interval(time_interval[t],
                                                                 time_interval[t+1]-1),
                                                     temp.actyearFile$act_interval))
    temp.actyearFile$int.time<-as.numeric(temp.actyearFile$int.time,units="mins")
    #making the overlap as zero from NA values 
    temp.actyearFile$int.time[is.na(temp.actyearFile$int.time)]<-0
    #merge the necessary file 
    temp.activityds.hour<-data.frame()
    temp.activityds.hour<-aggregate(temp.actyearFile$int.time, 
                                    by=list(TUCASEID=temp.actyearFile$TUCASEID,
                                            TRCODEP=temp.actyearFile$TRCODEP), 
                                    FUN=sum)
    
    temp.activityds.hour$hour<-t
    temp.activityds.hour$year<-years[y]
    activityds.hour<-rbind(activityds.hour,temp.activityds.hour)
    temp.activityds.hour<-NULL
  }
}


#same repeat code for travelds 
whereds.hour<-data.frame()
temp.whereds.hour<-data.frame()
temp.whereyearFile<-data.frame()
for (y in 1:length(years)) 
{
  temp.whereyearFile<-subset(whereds.instance,year==years[y])#year filter 
  for (t in 1:(length(time_interval)-1))#interval filter 
  {
    ##list id of person whose activity matched within the time interval
    id.caseid<-int_overlaps(temp.whereyearFile$act_interval,
                            as.interval(time_interval[t],
                                        time_interval[t+1]-1)) 
    ##duration of the activity within the time interval 
    temp.whereyearFile$int.time<-as.difftime(intersect(as.interval(time_interval[t],
                                                                 time_interval[t+1]-1),
                                                     temp.whereyearFile$act_interval))
    temp.whereyearFile$int.time<-as.numeric(temp.whereyearFile$int.time,units="mins")
    #making the overlap as zero from NA values 
    temp.whereyearFile$int.time[is.na(temp.whereyearFile$int.time)]<-0
    #merge the necessary file 
    temp.whereds.hour<-data.frame()
    temp.whereds.hour<-aggregate(temp.whereyearFile$int.time, 
                                    by=list(TUCASEID=temp.whereyearFile$TUCASEID,
                                            TEWHERE=temp.whereyearFile$TEWHERE), 
                                    FUN=sum)
    temp.whereds.hour$hour<-t
    temp.whereds.hour$year<-years[y]
    whereds.hour<-rbind(whereds.hour,temp.whereds.hour)
    temp.whereds.hour<-NULL
  }
}

save.image()






