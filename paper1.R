#R code of the ATUS paper 1 

#author: Ashok Sekar
#date: Jan 3 2016


#necessary packages 
require(lubridate)
require(censReg)
require(survival)
require(matrixStats)
require(reshape)
require(VGAM)

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
codestochange$replace<-rowSums(codestochange[,3:4],na.rm = TRUE,dims = 1)
#changing the error codes in the atusact file 
#the codes in the atussumm file is not touched right now. 
for (i in 1:length(codestochange$X))
{
  id<-which(atusact$TRCODEP==codestochange$codes_NA[i])
  #codestochange$numbers[i]<-length(id)
  if (codestochange$replace[i]!=0)
  {
    atusact$TRCODEP[id]<-codestochange$replace[i]
  }
}


 
#edits to input data 
#codes needed 
atusact$year<-as.numeric(substr(atusact$TUCASEID,1,4))
atusact<-merge(x=atusact,y=atussumm[,c("TUCASEID","TUFNWGTP")],by='TUCASEID',all.x = TRUE)
atusact$year_edit<-atusact$year-2002 #used to running the regression
atussumm$year_edit<-atussumm$TUYEAR-2002 #used for running regression 


#finding total weights for each year
totweight_year<-matrix()
years<-2003:2014
for (i in 1:12) 
{
  totweight_year[i]<-sum(atussumm$TUFNWGTP[atussumm$TUYEAR==years[i]])
}


#working with time data 
#adding a new variable to sort out the day issue with start and stop time
# 1199 equals 20hours i.e. 4AM - 11:50PM
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


######DESCRIPTIVE STATISTICS 

#####activity based assessment 

#finding time intervals within the dataset for activity based assessment 
#logic: find if there is intersection, then calculate the difference 
#value<-int_overlaps(act_interval,as.interval(time_interval[1],time_interval[2]-1))

#list of activities/inputs
allact<-unique(atusact$TRCODEP)
majact<-unique(atusact$TRTIER1P)
subjact<-unique(atusact$TRTIER2P)


#initializing inputs 

temp_mean_a<-data.frame()
temp_sd_a<-data.frame()
temp_mean_b<-data.frame()
temp_sd_b<-data.frame()
temp_prate_a<-data.frame()
temp_prate_b<-data.frame()
temp_count<-data.frame()
tot_prate_a<-data.frame()
tot_prate_b<-data.frame()
tot_count<-data.frame()

list_mean_a<-list()
list_mean_b<-list()
list_sd_a<-list()
list_sd_b<-list()
list_prate_a<-list()
list_prate_b<-list()
list_count<-list()


#the main loop 
#need to quantify sd_a it is currently incomplete. 

for (a in 1:length(allact)) #change the number of activities 
{
  mainFile<-subset(atusact,TRCODEP==allact[a])
  
  for (y in 1:length(years)) 
  {
    fileYear<-subset(mainFile,year==years[y])
    #temp_mean[y,1]<-weighted.mean(fileYear$TUACTDUR24,file$TUFNWGTP)
    tot_count[y,1]<-length(unique(fileYear$TUCASEID))
    tot_prate_a[y,1]<-sum(unique(fileYear$TUFNWGTP))/totweight_year[y]
    tot_prate_b[y,1]<-1

    for (t in 1:(length(time_interval)-1))
    {
      #mean and sd of duration of each activity
      ##list id of person whose activity matched within the time interval
      id_year<-int_overlaps(fileYear$act_interval,
                            as.interval(time_interval[t],
                                        time_interval[t+1]-1)) #id that provides when time interval intersects  
      ##duration of the activity within the time interval 
      temp1<-as.difftime(intersect(as.interval(time_interval[t],time_interval[t+1]-1),fileYear$act_interval))
      temp1<-as.numeric(temp1,units="mins")
      #making the overlap as zero from NA values 
      temp1[is.na(temp1)]<-0
      ##mean of the duration in each interval 
      temp_mean_a[y,t]<-sum(temp1*fileYear$TUFNWGTP[id_year])/totweight_year[y]
      ##temp_sd_a[y,t]<-
      
      temp_mean_b[y,t]<-weighted.mean(temp1[id_year],fileYear$TUFNWGTP[id_year])
      temp_sd_b[y,t]<-weightedSd(temp1[id_year],fileYear$TUFNWGTP[id_year])
      #calculating participation rate and count
      temp_count[y,t]<-length(fileYear$TUCASEID[id_year])
      temp_prate_a[y,t]<-sum(fileYear$TUFNWGTP[id_year])/totweight_year[y]
      temp_prate_b[y,t]<-sum(fileYear$TUFNWGTP[id_year])/sum(fileYear$TUFNWGTP)
    }
    
    #the following calculations are done for each year 
    temp_mean_a[is.na(temp_mean_a)]<-0
    temp_mean_b[is.na(temp_mean_b)]<-0
    temp_sd_b[is.na(temp_sd_b)]<-0
    
    
    temp_mean_a$tot[y]<-sum(temp_mean_a[y,])
    temp_mean_b$tot[y]<-sum(temp_mean_b[y,])
    #temp_sd_a$tot[y]<-sqrt(mean(temp_sd_a[y,]^2))
    temp_sd_b$tot[y]<-sqrt(mean(temp_sd_b[y,]^2))
  }
  
 
  temp_prate_a$tot<-tot_prate_a$V1
  temp_prate_b$tot<-tot_prate_b$V1
  temp_count$tot<-tot_count$V1
  
  
  #naming all the dataset before adding it to a list 
  
  
  list_count[[a]]<-temp_count
  list_mean_a[[a]]<-temp_mean_a
  list_mean_b[[a]]<-temp_mean_a
  #list_sd_a[[a]]<-temp_sd_a
  list_sd_b[[a]]<-temp_sd_b
  list_prate_a[[a]]<-temp_prate_a
  list_prate_b[[a]]<-temp_prate_b
  
}



names(list_count[[a]])<-allact
names(list_mean_a[[a]])<-allact
names(list_mean_b[[a]])<-allact
#list_sd_a[[a]]<-temp_sd_a
names(list_sd_b[[a]])<-allact
names(list_prate_a[[a]])<-allact
names(list_prate_b[[a]])<-allact



#saving files with appropriate names 
allactivities_count<-list_count
allactivities_prate<-list(list_prate_a,list_prate_b)
allactivities_mean<-list(list_mean_a,list_mean_b)
allactivities_sd<-list(list_sd_a,list_sd_b)


save.image(file = "~/Dropbox/Ongoing_Work/Time_Use/Paper1/paper1.Rdata")

#------------------------------------------------------------------------------------
#location analysis 
#homeFile<-atusact[atusact$TEWHERE %in% c(-1,1,3),] #selecting activities conducted at home
#home analysis outputs 4 files that containts the time spent at home locations 
# -1, 1, and 3 i.e., personal activities, at home and at neighbors home.
# the four files summarizes the mean, count, participation rate and SD 
# the results from this can be used to 


#initializing inputs 
temp_mean_a<-data.frame()
temp_sd_a<-data.frame()
temp_mean_b<-data.frame()
temp_sd_b<-data.frame()
temp_prate_a<-data.frame()
temp_prate_b<-data.frame()
temp_count<-data.frame()
tot_prate_a<-data.frame()
tot_prate_b<-data.frame()
tot_count<-data.frame()

list_mean_a<-list()
list_mean_b<-list()
list_sd_a<-list()
list_sd_b<-list()
list_prate_a<-list()
list_prate_b<-list()
list_count<-list()


locations<-unique(atusact$TEWHERE)
for (a in 1:length(locations)) #change the number of activities 
{
  locFile<-atusact[atusact$TEWHERE %in% locations[a],]
  for (y in 1:length(years)) 
  {
    fileYear<-subset(locFile,year==years[y])
    tot_count[y,1]<-length(unique(fileYear$TUCASEID))
    tot_prate_a[y,1]<-sum(unique(fileYear$TUFNWGTP))/totweight_year[y]
    tot_prate_b[y,1]<-1
    
    for (t in 1:(length(time_interval)-1))
    {
      #mean and sd of duration of each activity
      ##list id of person whose activity matched within the time interval
      id_year<-int_overlaps(fileYear$act_interval,
                            as.interval(time_interval[t],
                                        time_interval[t+1]-1)) #id that provides when time interval intersects  
      ##duration of the activity within the time interval 
      temp1<-as.difftime(intersect(as.interval(time_interval[t],
                                               time_interval[t+1]-1),fileYear$act_interval))
      temp1<-as.numeric(temp1,units="mins")
      #making the overlap as zero from NA values 
      temp1[is.na(temp1)]<-0
      ##mean of the duration in each interval 
      temp_mean_a[y,t]<-sum(temp1*fileYear$TUFNWGTP[id_year])/totweight_year[y]
      ##temp_sd_a[y,t]<-
      
      temp_mean_b[y,t]<-weighted.mean(temp1[id_year],fileYear$TUFNWGTP[id_year])
      temp_sd_b[y,t]<-weightedSd(temp1[id_year],fileYear$TUFNWGTP[id_year])
      #calculating participation rate and count
      temp_count[y,t]<-length(fileYear$TUCASEID[id_year])
      temp_prate_a[y,t]<-sum(fileYear$TUFNWGTP[id_year])/totweight_year[y]
      temp_prate_b[y,t]<-sum(fileYear$TUFNWGTP[id_year])/sum(fileYear$TUFNWGTP)
    }
    
    #the following calculations are done for each year 
    temp_mean_a[is.na(temp_mean_a)]<-0
    temp_mean_b[is.na(temp_mean_b)]<-0
    temp_sd_b[is.na(temp_sd_b)]<-0
    
    
    temp_mean_a$tot[y]<-sum(temp_mean_a[y,])
    temp_mean_b$tot[y]<-sum(temp_mean_b[y,])
    #temp_sd_a$tot[y]<-sqrt(mean(temp_sd_a[y,]^2))
    temp_sd_b$tot[y]<-sqrt(mean(temp_sd_b[y,]^2))
  }
  
  
  temp_prate_a$tot<-tot_prate_a$V1
  temp_prate_b$tot<-tot_prate_b$V1
  temp_count$tot<-tot_count$V1
  
  
  #naming all the dataset before adding it to a list 
  
  
  list_count[[a]]<-temp_count
  list_mean_a[[a]]<-temp_mean_a
  list_mean_b[[a]]<-temp_mean_a
  #list_sd_a[[a]]<-temp_sd_a
  list_sd_b[[a]]<-temp_sd_b
  list_prate_a[[a]]<-temp_prate_a
  list_prate_b[[a]]<-temp_prate_b
  
}


names(list_count[[a]])<-locations
names(list_mean_a[[a]])<-locations
names(list_mean_a[[a]])<-locations
#list_sd_a[[a]]<-locations
names(list_sd_b[[a]])<-locations
names(list_prate_a[[a]])<-locations
names(list_prate_b[[a]])<-locations




#saving files with appropriate names 
locactivities_count<-list_count
locactivities_prate<-list(list_prate_a,list_prate_b)
locactivities_mean<-list(list_mean_a,list_mean_b)
locactivities_sd<-list(list_sd_a,list_sd_b)


save.image(file = "~/Dropbox/Ongoing_Work/Time_Use/Paper1/paper1.Rdata")





#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------TOBIT MODEL-------------------------------------------------------

#currently tobit model will be run only for whole day
#FIRST FOR ALL ACTIVITIES 

activitytobit<-list()
file1<-atussumm[,c('TUCASEID','TUYEAR')]

for (a in 1)#:length(allact))
{
  file2<-atusact[,c('TUCASEID','TUACTDUR24',"TEWHERE")][atusact$TRCODEP %in% c(120303),]
  file3<-merge(x=file1,y=file2,by='TUCASEID',all.x = TRUE)
  file3$TUYEAR<-NULL 
  file3<-file3[order(file3$TUCASEID),]
  file3$TUACTDUR24[is.na(file3$TUACTDUR24)]<-0
  file4<-aggregate(file3,by=list(file3$TUCASEID),FUN = sum)
  file4$year<-as.numeric(substr(file4$Group.1,1,4))-2002
  activitytobit[[a]]<-summary(vglm(formula = TUACTDUR24 ~ year, family = tobit(Lower = 0), data = file4))
}

save.image(file = "~/Dropbox/Ongoing_Work/Time_Use/Paper1/paper1.Rdata")

#FOR ALL LOCATIONS and ACTIVITIES 

locationtobit<-list()
#cartobit<-list()
file1<-atussumm[,c('TUCASEID','TUYEAR')]

for (a in 1:length(locations))
{
  file2<-atusact[,c('TUCASEID','TUACTDUR24','TRCODEP')][atusact$TEWHERE %in% locations[a],]
  file3<-merge(x=file1,y=file2,by='TUCASEID',all.x = TRUE)
  file3$TUYEAR<-NULL 
  file3<-file3[order(file3$TUCASEID),]
  file3$TUACTDUR24[is.na(file3$TUACTDUR24)]<-0
  file4<-aggregate(file3,by=list(file3$TUCASEID),FUN = sum)
  file4$year<-as.numeric(substr(file4$Group.1,1,4))-2002
  locationtobit[[a]]<-summary(vglm(formula = TUACTDUR24 ~ year, family = tobit(Lower = 0), data = file4))
}


###for all locations for different demographic group 
#employment 
file1<-atussumm[,c('TUCASEID','TUYEAR')]
slope.emp<-data.frame(matrix(nrow = length(locations),ncol = 7))

for (a in 28:length(locations))
{
  file2<-atusact[,c('TUCASEID','TUACTDUR24','TRCODEP')][atusact$TEWHERE %in% locations[a],]
  file3<-merge(x=file1,y=file2,by='TUCASEID',all.x = TRUE)
  file3$TUYEAR<-NULL 
  file3<-file3[order(file3$TUCASEID),]
  file3$TUACTDUR24[is.na(file3$TUACTDUR24)]<-0
  file4<-aggregate(file3,by=list(file3$TUCASEID),FUN = sum)
  file4$year<-as.numeric(substr(file4$Group.1,1,4))-2002
  file4<-merge(x=file4,y=atussumm[,c("TUCASEID","TELFS")],by="TUCASEID",all.x = TRUE)
  emp<-list(a=c(1,2),b=c(3,4),c=5)
  temp<-list()
  k<-c(2,3)
  for (j in 1:3)
  {
    file5<-file4[file4$TELFS %in% emp[[j]],]
    if (sum(file5$TUACTDUR24>2000))
    {
      z<-summary(vglm(formula = TUACTDUR24 ~ year, family = tobit(Lower = 0), data = file5))
      z1<-c(z@coef3[3,1],round(z@coef3[3,4],5))
      slope.emp[a,c(k)]<-z1
    }
    k<-k+2
  }
  slope.emp[a,1]<-locations[a]
}

slope.emp[order(slope.emp$X1),]




#######FOR SELECTED LOCATIONS AND ACTIVITIES 

hometobit<-list()
#cartobit<-list()
file1<-atussumm[,c('TUCASEID','TUYEAR')]
locations_short<-c(-1,1)
#locations_short<-c(12) 


file2<-atusact[,c('TUCASEID','TUACTDUR24','TRCODEP')][atusact$TEWHERE %in% locations_short,]
file3<-merge(x=file1,y=file2,by='TUCASEID',all.x = TRUE)
file3$TUYEAR<-NULL 
file3<-file3[order(file3$TUCASEID),]
file3$TUACTDUR24[is.na(file3$TUACTDUR24)]<-0
file4<-aggregate(file3,by=list(file3$TUCASEID),FUN = sum)
file4$year<-as.numeric(substr(file4$Group.1,1,4))-2002
hometobit[[1]]<-summary(vglm(formula = TUACTDUR24 ~ year, family = tobit(Lower = 0), data = file4))
#cartobit[[1]]<-summary(vglm(formula = TUACTDUR24 ~ year, family = tobit(Lower = 0), data = file4))

home.acttobit<-list()
#car.acttobit<-list()

temp<-data.frame(matrix(nrow = length(allact),ncol = 2))
for (j in 1:length(allact))
{
  chk<-file2[file2$TRCODEP %in% allact[j],]
  temp[j,]<-c(allact[j],round(length(unique(chk$TUCASEID))/length(atussumm$TUCASEID),2))
}
temp<-temp[order(-temp$X2),]
allact_short<-temp$X1[temp$X2>.005]

for (b in 1:length(allact_short))
{
  file2a<-file2[,c('TUCASEID','TUACTDUR24')][file2$TRCODEP %in% allact_short[b],]
  file3<-merge(x=file1,y=file2a,by='TUCASEID',all.x = TRUE)
  file3$TUYEAR<-NULL 
  file3<-file3[order(file3$TUCASEID),]
  file3$TUACTDUR24[is.na(file3$TUACTDUR24)]<-0
  file4<-aggregate(file3,by=list(file3$TUCASEID),FUN = sum)
  file4$year<-as.numeric(substr(file4$Group.1,1,4))-2002
  if(sum(file4$TUACTDUR24>2000))
  {
    home.acttobit[[b]]<-summary(vglm(formula = TUACTDUR24 ~ year, family = tobit(Lower = 0), data = file4))
  }
  
  #car.acttobit[[b]]<-summary(vglm(formula = TUACTDUR24 ~ year, family = tobit(Lower = 0), data = file4))
}

save.image(file = "~/Dropbox/Ongoing_Work/Time_Use/Paper1/paper1.Rdata")


#converting from list to data.frame
slope<-data.frame(matrix(nrow = length(locations),ncol = 3))
for (i in 1:length(locations))
{
  slope[i,1:3]<-c(locations[i],locationtobit[[i]]@coef3[3,1],round(locationtobit[[i]]@coef3[3,4],5))
}
slope<-slope[order(slope$X1),]

avg<-NA
for (i in 1:length(locations))
{
  z<-mean(locactivities_prate[[1]][[i]]$tot)
  avg<-c(avg,z)
}
avg<-data.frame(avg[-1])
avg$locations<-locations
avg[order(avg$locations),]

slope.homeact<-data.frame(matrix(nrow = length(allact_short),ncol = 3))
for (i in 1:67)
{
  slope.homeact[i,1:3]<-c(allact_short[i],home.acttobit[[i]]@coef3[3,1],round(home.acttobit[[i]]@coef3[3,4],5))
}
slope.homeact<-slope.homeact[order(slope.homeact$X1),]
slope.homeact<-rename(slope.homeact,c('X1'='V1'))
slope.homeact<-merge(x=slope.homeact,y=allact_desc,by='V1',all.x = TRUE)
slope.homeact<-rename(slope.homeact,c('V1'='X1'))
slope.homeact<-merge(x=slope.homeact,y=temp,by='X1',all.x = TRUE)
slope.homeact<-rename(slope.homeact,c('X3'='pr(>|z|)','X2.y'='prate'))

avg.homeact<-NA
for (i in 1:length(allact_short))
{
  id_temp<-which(allact==allact_short[i])
  z<-mean(temp_mean_a[[2]][[id_temp]]$tot)
  avg.homeact<-c(avg.homeact,z)
}
avg.homeact<-data.frame(avg.homeact[-1])
avg.homeact$X1<-allact_short
#avg.homeact[order(avg.homeact$locations),]




# ##for now do a heat map for time spent at home 
# 
# chk1<-homeactivities_mean[[1]]+homeactivities_mean[[2]]+homeactivities_mean[[3]]
# rownames(chk1)<-c(2003:2014)
# colnames(chk1)<-c('4-5AM','5-6AM','6-7AM','7-8AM','8-9AM','9-10AM','10-11AM','11-12PM','12-1PM','1-2PM','2-3PM','3-4PM','4-5PM','5-6PM','6-7PM','7-8PM','8-9PM','9-10PM','10-11PM','11-12AM','12-1AM','1-2AM','2-3AM','3-4AM')
# chk1$Total<-rowSums(chk1)
# 
# chk1_matrix<-data.matrix(chk1)
# heatmap(chk1_matrix,Rowv=NA,Colv=NA,col=heat.colors(256),scal="column",xlab = 'time',ylab = 'year',main = 'Heat map of time spent at home')

# ##do a heat map for 
# 
# 
# for (a in 1:length(allact))
# {
#   
#   mainFile<-subset(atusact,TRCODEP==allact[a])
#   
#   for (t in 1:(length(time_interval)-1))
#   {
#     #calculating tobit
#     ##list of id of respondents whose activity matched within the time interval 
#     id<-int_overlaps(mainFile$act_interval,as.interval(time_interval[t],time_interval[t+1]-1)) #id that provides when time interval intersects  
#     ##duration of the activitiy within the time interval 
#     temp2<-as.difftime(intersect(as.interval(time_interval[t],time_interval[t+1]-1),mainFile$act_interval))
#     temp2<-as.numeric(temp2,units="hours")
#     
#     temp3<-survReg(Surv(temp2[id],temp2[id]>0,type = "left") ~ year_edit[id], 
#                    weights = TUFNWGTP[id], data = mainFile, dist = "gaussian")
#     list_tobit[[t]]<-summary(temp3)$table
#     
# #     ##calculate the tobit 
# #     if (sum(temp2[id]==0))
# #     {
# #       list_tobit[[t]]<-c('all values were zero')
# #     }
# #     else 
# #     {
# #       temp3<-survreg(Surv(temp2[id],temp2[id]>0,type = "left") ~ year_edit[id], 
# #                      weights = TUFNWGTP[id], data = mainFile, dist = "gaussian")
# #       list_tobit[[t]]<-summary(temp3)$table
# #     }
#   }
#   
#   #list_list_tobit[[a]]<-list_tobit
#   
#   rm(mainFile)
# }
# 
# 
# 
# #list of activities done at home
# #for each activity what is the % of instance done at each location? 
# phome<-data.frame()
# newFile<-atusact[c("TEWHERE") %in% c(-1,1),]
# for (a in 1:length(allact))
# {
#   phome[a]<-round(dim(newFile[c("TRCODEP") %in% allact[a],])[1]/dim(atusact[c("TEWHERE") %in% allact[a],])[1],2)
# }


#code to quantify important activities 
##what are important activities? their mean should be relatively big, participation rate also big, 
##slope is also big, activity should be conducted at home


#analysis to get presentable result 

