#file to analyze travel from the melt file 


#remove all variables 
rm(list=ls())

#library needed for this code
library(reshape2)
library(plyr)
library(base)
library(matrixStats)
library(xlsx)

#generating datasets from atusact
atusact<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusact_0314.dat",header=TRUE,sep=",")
file<-atusact[,c('TUCASEID','TRTIER1P','TUACTDUR24','TRTIER2P','TRCODEP')]

#----TOCHANGE------#
file_subact<-dcast(file,TUCASEID~TRTIER1P,value.var = "TUACTDUR24",sum)

#gettin weight files and merging with dataset 
atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")
file_weight<-atussumm[,c('TUCASEID','TUFNWGTP')]
file_subact<-merge(x=file_subact,y=file_weight,by='TUCASEID',all.x = TRUE)

#annotate the file with the category names 
#load file containinng name of all the activity codes
sub_act_names<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/majactivitycodes_description.csv") # to change

#change the colname of the above files to help merge 
sub_act_names<-rename(sub_act_names,c('V1'='variable'))

#ADDING TELFS IN THE DATA SET 
file_subact_org<-merge(x=file_subact,y=atussumm[,c('TUCASEID','TELFS')],by='TUCASEID',all.x = TRUE)

for (a in 1:6)
{ 
  
  if (a==1) 
  {
    filename<-c("majact_trends_all.xlsx")
    file_subact<-file_subact_org
    
  } 
  
  else if (a==2)
  {
    filename<-c('majact_trends_emp_present.xlsx')
    file_subact<-file_subact_org[file_subact_org$TELFS==1,]
  } 
  
  else if(a==3)
  {
    filename<-c('majact_trends_emp_absent.xlsx')
    file_subact<-file_subact_org[file_subact_org$TELFS==2,]
  }
  
  else if(a==4)
  {
    filename<-c('majact_trends_unemp_onlayoff.xlsx')
    file_subact<-file_subact_org[file_subact_org$TELFS==3,]
  }  
  
  else if(a==5)
  {
    filename<-c('majact_trends_unemp_looking.xlsx')
    file_subact<-file_subact_org[file_subact_org$TELFS==4,]
    
  }  
  
  else
  {
    filename<-c('majact_trends_notinlaborforce.xlsx')
    file_subact<-file_subact_org[file_subact_org$TELFS==5,]
  }  
  
  
  
  #creating a melt file for travel activities 
  melt_subact<-melt(file_subact,id=c('TUCASEID','TUFNWGTP','TELFS'))
  
  #merge the description of the activity codes 
  melt_subact<-merge(x=melt_subact,y=sub_act_names,by='variable',all.x = TRUE)
  
  #creating a new column called year
  melt_subact$year<-as.numeric(substr(melt_subact$TUCASEID,1,4))
  
  
  #summarizing the results 
  
  #ddply for all year trends 
  substats_allyear<-ddply(melt_subact, c("variable"), summarise,
                          description=unique(description),
                          #rsq=round(summary(lm(value~year))$r.squared,2),
                          #slope=round(summary(lm(value~year))$coef[2,1],2),
                          #tvalue=round(summary(lm(value~year))$coef[2,3],2),
                          #intercept=round(summary(lm(value~year))$coef[1,1],2),
                          mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                          sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                          #mean_excl_zeros = round(weightedMean(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                          #sd_excl_zeros = round(weightedSd(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                          participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                          numb_na=sum(is.na(value))
  )
  
  #ddply for per year results 
  substats_peryear<-ddply(melt_subact, c("variable","year"), summarise,
                          description=unique(description),
                          mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                          sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                          participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                          numb_na=sum(is.na(value))
  )
  
  #results from per year results that can be combined with all year result 
  temp_stats<-ddply(substats_peryear,c("variable"),summarize,
                    rsq_mean=round(summary(lm(mean~year))$r.squared,2),
                    slope_mean=round(summary(lm(mean~year))$coef[2,1],2),
                    intercept_mean=round(summary(lm(mean~year))$coef[1,1],2)
  )
  
  #combine 
  substats_allyear<-merge(x=substats_allyear,y=temp_stats,by="variable",all.x = TRUE)
  
  
  #generating the trends
  
  atusact_time<-atusact[,c("TUCASEID","TUACTIVITY_N","TUACTDUR24","TUCUMDUR24","TRTIER1P")] #TO CHANGE 
  atusact_time<-rename(atusact_time,c("TRTIER1P"="variable")) #TO CHANGE#
  atusact_time<-merge(x=atusact_time,y=sub_act_names,by="variable",all.x = TRUE)
  atusact_time<-merge(x=atusact_time,y=file_weight,by="TUCASEID",all.x = TRUE)
  atusact_time<-atusact_time[atusact_time$TUCASEID %in% file_subact$TUCASEID,]
  
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
  
  count<-length(unique(file_subact$TUCASEID)) #count the number of unique case id
  tot_weight<-sum(file_subact$TUFNWGTP) #total sum of the weights 
  allstats_starttime<-ddply(atusact_time, c("variable"), function(df) funStart(df$startid,count))   
  #allstats_stoptime<-ddply(atusact_time, c("variable"), function(df) funStart(df$stopid,count))   
  allstats_pattern<-ddply(atusact_time, c("variable"), function(df) funPattern(df$startid,df$stopid,df$TUFNWGTP,tot_weight))   
  
  #saving the files 
  wb<-createWorkbook() #creating workbook
  
  #naming the sheets 
  allyear<-createSheet(wb,sheetName="allyear")
  peryear<-createSheet(wb,sheetName="peryear")
  pattern<-createSheet(wb,sheetName="pattern")
  starttime<-createSheet(wb,sheetName="starttime")
  
  addDataFrame(substats_allyear, allyear)
  addDataFrame(substats_peryear, peryear)
  addDataFrame(allstats_pattern, pattern)
  addDataFrame(allstats_starttime, starttime)
  
  saveWorkbook(wb,filename)
  
}

##################################################
filename<-c('majact_trends_notinlaborforce.xlsx',
            'subact_trends_notinlaborforce.xlsx',
            'allact_trends_notinlaborforce.xlsx')

dataFile<-data.frame()
for (a in 1:3)
{ 
  dataFile<-rbind(dataFile,read.xlsx(filename[a],1))
} 
  
write.xlsx(dataFile,file = 'notinlaborforce.xlsx','Sheet1')

