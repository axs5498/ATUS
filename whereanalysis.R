#CODE TO UNDERSTAND LOCATION OF ACTIVITIES 

#libraries
library(plyr)
library(reshape2)

#load atussumm and atusact file 
rm(list=ls())

#generating datasets from atusact
atusact<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusact_0314.dat",header=TRUE,sep=",")
atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")

file<-atusact[,c('TUCASEID','TRTIER1P','TUACTDUR24','TRTIER2P','TRCODEP','TEWHERE')]
#change the TUWHERE ID'S 
wherenames<-read.csv('~/Dropbox/Results/wherenames.csv')

#new way of wordings 
# newnames<-data.frame(matrix(nrow = 5,ncol = 2))
# colnames(newnames)<-c('variable','description')
# newnames$description<-c('home','work','travel_motorized','travel_nonmotorized','other')
# newnames$variable<-c(1,2,3,4,5)
# 
# 
# file$TEWHERE[file$TEWHERE %in% c(-1,1)]<-newnames$description[1] #if blank or home make home 
# file$TEWHERE[file$TEWHERE %in% c(2)]<-newnames$description[2] #if 2 equals 2 or work 
# file$TEWHERE[file$TEWHERE %in% c(12,13,15,16,18,19,20,21,99)]<-newnames$description[3] # nonmotorized
# file$TEWHERE[file$TEWHERE==14|file1$TEWHERE==17]<-newnames$description[4]
# file$TEWHERE[!file$TEWHERE %in% c('home','work','travel_motorized','travel_nonmotorized')]<-newnames$description[5]
# 

file_intvars<-atussumm[,c('TUCASEID','TUFNWGTP','TELFS','TUYEAR')] #add variables here if needed 

actfile<-merge(x=file,y=file_intvars,all.x = TRUE)


 
years<-2003:2014
 


#descrition of the where codes 

#average number of activities in each locatio for all population 
dataFile<-list()
dF_activitywhere<-list()

for (a in 1)#:6)
{
  
  if (a==1) 
  {
    filename<-c('all')
    
    file_freq<-dcast(file,TUCASEID~TEWHERE,value.var = "TEWHERE") #DEFAULTS TO LENGTH
    file_sum<-dcast(file,TUCASEID~TEWHERE,value.var = "TUACTDUR24",sum) #sum of DURATION for each respondent
    
    #melting the files 
    melt_freq<-melt(file_freq,id=c('TUCASEID'))
    melt_sum<-melt(file_sum,id=c('TUCASEID'))
    
    #merging the meltfiles 
    meltfile<-merge(x=melt_sum,y=melt_freq,by=c('TUCASEID','variable'),all.x = TRUE)
    #merging variable names 
    meltfile<-merge(x=meltfile,y=wherenames,by='variable',all.x = TRUE)
    #merging interesting variables
    meltfile<-merge(x=meltfile,y=file_intvars,by='TUCASEID',all.x = TRUE)
    
    #renaming colnames
    meltfile<-rename(meltfile,c('value.x'='duration','value.y'='freq'))
    
    #defining new names 
    newFile<-meltfile
    
    #activity file
    afile<-actfile
    
  } 
  
  else if (a==2)
  {
    filename<-c('emp_present')
    newFile<-meltfile[meltfile$TELFS==1,]
    
    afile<-actfile[actfile$TELFS==1,]
    
  }
  
  else if(a==3)
  {
    filename<-c('emp_absent')
    newFile<-meltfile[meltfile$TELFS==2,]
    
    afile<-actfile[actfile$TELFS==2,]
  }
  
  else if(a==4)
  {
    filename<-c('unemp_onlayoff')
    newFile<-meltfile[meltfile$TELFS==3,]
    
    afile<-actfile[actfile$TELFS==3,]
  }  
    
  else if(a==5)
  {
    filename<-c('unemp_looking')
    newFile<-meltfile[meltfile$TELFS==4,]
    afile<-actfile[actfile$TELFS==4,]
  }  
  
  else
  {
    filename<-c('notinlaborforce')
    newFile<-meltfile[meltfile$TELFS==5,]
    
    afile<-actfile[actfile$TELFS==5,]
  }  

  
  
  stats_allyear<-ddply(newFile, c("variable","TUYEAR"), summarise,
                         desc=unique(description),
                       year=unique(TUYEAR),
                         mean_duration=round(sum(duration*TUFNWGTP)/sum(TUFNWGTP),2),
                       mean_freq=round(sum(freq*TUFNWGTP)/sum(TUFNWGTP),2)
  )
  
  stats_peryear<-ddply(newFile,c("variable","TUYEAR"),summarise,
                         #desc=unique(description),
                         year=unique(TUYEAR),
                       mean_duration=round(sum(duration*TUFNWGTP)/sum(TUFNWGTP),2),
                       mean_freq=round(sum(freq*TUFNWGTP)/sum(TUFNWGTP),2)
  )
  
  temp_trend<-ddply(stats_peryear,c("variable"),summarize,
                    rsq_duration=round(summary(lm(mean_duration~year))$r.squared,2),
                    slope_duration=round(summary(lm(mean_duration~year))$coef[2,1],2),
                    intercept_duration=round(summary(lm(mean_duration~year))$coef[1,1],2),
                    
                    rsq_freq=round(summary(lm(mean_freq~year))$r.squared,2),
                    slope_freq=round(summary(lm(mean_freq~year))$coef[2,1],2),
                    intercept_freq=round(summary(lm(mean_freq~year))$coef[1,1],2)
  )
  
  stats_allyear<-merge(x=stats_allyear,y=temp_trend,by="variable",all.x = TRUE)
  

  dataFile[[filename]]<-stats_allyear
    
#   
#   activity_where_maj<-ddply(afile,c("TRTIER1P"),summarize,
#                             home=round(sum(ifelse(TEWHERE==newnames$description[1],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             work=round(sum(ifelse(TEWHERE==newnames$description[2],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             travel_motorized=round(sum(ifelse(TEWHERE==newnames$description[3],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             travel_nonmotorized=round(sum(ifelse(TEWHERE==newnames$description[4],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             other=round(sum(ifelse(TEWHERE==newnames$description[5],1,0)*TUFNWGTP)/sum(TUFNWGTP),2))
#   activity_where_maj<-rename(activity_where_maj,c("TRTIER1P"="code"))
#   
#   
#   activity_where_sub<-ddply(afile,c("TRTIER2P"),summarize,
#                             home=round(sum(ifelse(TEWHERE==newnames$description[1],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             work=round(sum(ifelse(TEWHERE==newnames$description[2],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             travel_motorized=round(sum(ifelse(TEWHERE==newnames$description[3],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             travel_nonmotorized=round(sum(ifelse(TEWHERE==newnames$description[4],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             other=round(sum(ifelse(TEWHERE==newnames$description[5],1,0)*TUFNWGTP)/sum(TUFNWGTP),2))
#   activity_where_sub<-rename(activity_where_sub,c("TRTIER2P"="code"))
#   
#   
#   activity_where_all<-ddply(afile,c("TRCODEP"),summarize,
#                             home=round(sum(ifelse(TEWHERE==newnames$description[1],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             work=round(sum(ifelse(TEWHERE==newnames$description[2],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             travel_motorized=round(sum(ifelse(TEWHERE==newnames$description[3],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             travel_nonmotorized=round(sum(ifelse(TEWHERE==newnames$description[4],1,0)*TUFNWGTP)/sum(TUFNWGTP),2),
#                             other=round(sum(ifelse(TEWHERE==newnames$description[5],1,0)*TUFNWGTP)/sum(TUFNWGTP),2))
#   activity_where_all<-rename(activity_where_all,c("TRCODEP"="code"))
#   
#   
#   activity_where<-rbind(activity_where_maj,activity_where_sub,activity_where_all)
#   activity_where$code<-factor(activity_where$code)
#   
#   dF_activitywhere[[filename]]<-activity_where
  
}

setwd("~/Dropbox/Results")  

library(xlsx)
wb <- createWorkbook()
saveWorkbook(wb, 'where_stats.xlsx')
lapply(names(dataFile), function(x) write.xlsx(dataFile[[x]], 'where_stats.xlsx', sheetName=x, append=TRUE))

wb1<- createWorkbook()
saveWorkbook(wb1,'activitywhere.xlsx')
lapply(names(dF_activitywhere), function(x) write.xlsx(dF_activitywhere[[x]], 'activitywhere.xlsx', sheetName=x, append=TRUE))









#generating trends for the location 
#generating the trends


atusact_time<-atusact[,c("TUCASEID","TUACTIVITY_N","TUACTDUR24","TUCUMDUR24","TEWHERE")]
atusact_time<-rename(atusact_time,c("TEWHERE"="variable")) #TO CHANGE#
atusact_time<-merge(x=atusact_time,y=wherenames,by="variable",all.x = TRUE)
atusact_time<-merge(x=atusact_time,y=file_weight,by="TUCASEID",all.x = TRUE)
#atusact_time<-atusact_time[atusact_time$TUCASEID %in% unique(melt_act$TUCASEID),] average
atusact_time$year<-as.numeric(substr(atusact_time$TUCASEID,1,4))

#generating start and stop id 
atusact_time$startid<-(atusact_time$TUCUMDUR24-atusact_time$TUACTDUR24)/10
atusact_time$stopid<-atusact_time$TUCUMDUR24/10

funStart<-function(x,no_of_participants) # function for start and stop time 
{
  dam<-(matrix(nrow = 1,ncol = 144))
  for (i in 1:144)
  {
    dam[1,i]<-sum(x>(i-1)&x<=i)/no_of_participants
  }
  return(dam)
}

funPattern<-function(x,y,w,tot_weight) # function for activity pattern 
{
  dam<-(matrix(nrow = 1,ncol = 144))
  for (i in 1:144)
  {
    dam[1,i]<-sum(w[i>=x&i<y])/tot_weight
  }
  return(dam)
}

count<-length(unique(atusact_time$TUCASEID)) #count the number of unique case id
tot_weight<-sum(unique(atusact_time$TUFNWGTP)) #total sum of the weights 
allstats_starttime<-ddply(atusact_time, c("description","year"), function(df) funStart(df$startid,count))   
#allstats_stoptime<-ddply(atusact_time, c("variable"), function(df) funStart(df$stopid,count))   
allstats_pattern<-ddply(atusact_time, c("description","year"), function(df) funPattern(df$startid,df$stopid,df$TUFNWGTP,tot_weight))   


#plotting surface plots 
install.packages("devtools")  # so we can install from github
library("devtools")
install_github("ropensci/plotly")  # plotly is part of ropensci
library(plotly)

plot_ly(data1,type="surface")














