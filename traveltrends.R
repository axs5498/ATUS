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


file_allact<-dcast(file,TUCASEID~TRCODEP,value.var = "TUACTDUR24",sum)

#gettin weight files and merging with dataset 
atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")
file_weight<-atussumm[,c('TUCASEID','TUFNWGTP')]
file_allact<-merge(x=file_allact,y=file_weight,by='TUCASEID',all.x = TRUE)

#annotate the file with the category names 
#load file containinng name of all the activity codes
all_act_names<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/allactivitycodes_description.csv")

#change the colname of the above files to help merge 
all_act_names<-rename(all_act_names,c('V1'='variable'))

#renaming the rownames of the codes
allcodes<-matrix(colnames(file_allact))
replace<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/codestochange.csv")
replace$change<-rowSums(replace[,c("match.with.0","match.with.9")],na.rm = TRUE)
replace_final<-subset(replace, change>0)
for (i in 1:length(replace_final$codes_NA))
{
  j<-as.character(replace_final$codes_NA[i])
  allcodes[allcodes==j]<-replace_final$change[i]
}
colnames(file_allact)<-allcodes

#selecting only travel activities
travelactlist<-allcodes[allcodes>=180000&allcodes<190000]
file_travelact<-file_allact[,c('TUCASEID','TUFNWGTP',travelactlist)]
file_travelact$Freq<-rowSums(file_allact[,travelactlist]>0)
file_travelact<-subset(file_travelact,Freq>0) #choosing only people who travelled 
#ADDING TELFS IN THE DATA SET 
file_travelact<-merge(x=file_travelact,y=atussumm[,c('TUCASEID','TELFS')],by='TUCASEID',all.x = TRUE)

#filename<-c("resp_who_travelled.xlsx")

#---------------------------------------spl cases 
#creating the new melt_travelact BASED ON EMP CATEGORYS  
#file_travelact<-file_travelact[file_travelact$TELFS==1,]
#filename<-c("resp_who_travelled_employed_present.xlsx")

#file_travelact<-file_travelact[file_travelact$TELFS==2,]
#filename<-c("resp_who_travelled_employed_absent.xlsx")

#file_travelact<-file_travelact[file_travelact$TELFS==3,]
#filename<-c("resp_who_travelled_unemp_onlayoff.xlsx")

#file_travelact<-file_travelact[file_travelact$TELFS==4,]
#filename<-c("resp_who_travelled_unemp_looking.xlsx")

file_travelact<-file_travelact[file_travelact$TELFS==5,]
filename<-c("resp_who_travelled_notinlaborforce.xlsx")


#creating a melt file for travel activities 
melt_travelact<-melt(file_travelact,id=c('TUCASEID','TUFNWGTP','Freq','TELFS'))

#merge the description of the activity codes 
all_act_names<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/allactivitycodes_description.csv")
all_act_names<-rename(all_act_names,c('V1'='variable')) # renaming colname
melt_travelact<-merge(x=melt_travelact,y=all_act_names,by='variable',all.x = TRUE)
#creating a new column called year
melt_travelact$year<-as.numeric(substr(melt_travelact$TUCASEID,1,4))


#summarizing the results 

#general result for all people travelling 

#ddply for all year trends 
travelstats_allyear<-ddply(melt_travelact, c("variable"), summarise,
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
travelstats_peryear<-ddply(melt_travelact, c("variable","year"), summarise,
                        description=unique(description),
                        mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                        sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                        #mean_excl_zeros = round(weightedMean(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        #sd_excl_zeros = round(weightedSd(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                        numb_na=sum(is.na(value))
)

#results from per year results that can be combined with all year result 
temp_stats<-ddply(travelstats_peryear,c("variable"),summarize,
                  rsq_mean=round(summary(lm(mean~year))$r.squared,2),
                  slope_mean=round(summary(lm(mean~year))$coef[2,1],2),
                  intercept_mean=round(summary(lm(mean~year))$coef[1,1],2)
)

#combine 
travelstats_allyear<-merge(x=travelstats_allyear,y=temp_stats,by="variable",all.x = TRUE)


#generating the trends

atusact_time<-atusact[,c("TUCASEID","TUACTIVITY_N","TUACTDUR24","TUCUMDUR24","TRCODEP")]
atusact_time<-rename(atusact_time,c("TRCODEP"="variable"))
atusact_time<-merge(x=atusact_time,y=all_act_names,by="variable",all.x = TRUE)
atusact_time<-merge(x=atusact_time,y=file_weight,by="TUCASEID",all.x = TRUE)
atusact_time<-atusact_time[atusact_time$TUCASEID %in% file_travelact$TUCASEID,]

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

count<-length(unique(file_travelact$TUCASEID)) #count the number of unique case id
tot_weight<-sum(file_travelact$TUFNWGTP) #total sum of the weights 
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

addDataFrame(travelstats_allyear, allyear)
addDataFrame(travelstats_peryear, peryear)
addDataFrame(allstats_pattern, pattern)
addDataFrame(allstats_starttime, starttime)

saveWorkbook(wb,filename)


