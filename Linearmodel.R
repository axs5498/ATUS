
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
#file_majact<-dcast(file,TUCASEID~TRTIER1P,value.var = "TUACTDUR24",sum)
#file_subact<-dcast(file,TUCASEID~TRTIER2P,value.var = "TUACTDUR24",sum)
file_allact<-dcast(file,TUCASEID~TRCODEP,value.var = "TUACTDUR24",sum)
file_allact<-file_allact[file_allact$TUCASEID %in% ytravel_subset$TUCASEID,] #code that removes people who did not travel 

atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")
file_weight<-atussumm[,c('TUCASEID','TUFNWGTP')]

#file_majact<-merge(x=file_majact,y=file_weight,by='TUCASEID',all.x = TRUE)
#file_subact<-merge(x=file_subact,y=file_weight,by='TUCASEID',all.x = TRUE)
file_allact<-merge(x=file_allact,y=file_weight,by='TUCASEID',all.x = TRUE)


#annotate the file with the category names 
#load file containinng name of all the activity codes
#maj_act_names<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/majactivitycodes_description.csv")
#sub_act_names<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/subactivitycodes_description.csv")
all_act_names<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/allactivitycodes_description.csv")

#change the colname of the above files to help merge 
#maj_act_names<-rename(maj_act_names,c('V1'='variable'))
#sub_act_names<-rename(sub_act_names,c('V1'='variable'))
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


#filtering the dataset only for travel





#melt the file into final form
#melt_majact<-melt(file_majact,id=c('TUCASEID','TUFNWGTP'))
#melt_subact<-melt(file_subact,id=c('TUCASEID','TUFNWGTP'))
melt_allact<-melt(file_allact,id=c('TUCASEID','TUFNWGTP'))

#merge the description of the activity codes 
#melt_majact<-merge(x=melt_majact,y=maj_act_names,by='variable',all.x = TRUE)
#melt_subact<-merge(x=melt_subact,y=sub_act_names,by='variable',all.x = TRUE)
melt_allact<-merge(x=melt_allact,y=all_act_names,by='variable',all.x = TRUE)


#create the year data
#melt_majact$year<-as.numeric(substr(melt_majact$TUCASEID,1,4))
#melt_subact$year<-as.numeric(substr(melt_subact$TUCASEID,1,4))
melt_allact$year<-as.numeric(substr(melt_allact$TUCASEID,1,4))



# #piece of code to identify the codes - currently not necessary 
# #identifying the codes that are renamed and changing them manually
# 
# codes_in_dataset<-ddply(melt_allact, c("variable"), summarise,
#                         description=unique(description)) #summarizing all the codes 
# 
# codes_NA<-as.numeric(as.character(codes_in_dataset$variable[is.na(codes_in_dataset$description)])) #list codes with NA
#                         
# melt_allact$variable<-as.numeric(as.character(melt_allact$variable))
# 
# codes_change<-data.frame(matrix(nrow = length(codes_NA),ncol = 3))
# colnames(codes_change)<-c("codes_NA","match with 0","match with 9")
# codes_change[,1]<-codes_NA
# 
# for (k in 1:length(codes_NA))
# { 
#   #find the closest number
#   #find numbers in sub category level 
#   i<-codes_NA[k]
#   
#   close<-all_act_names$variable[floor(all_act_names$variable/100)==floor(i/100)]
#   
#   j<-as.character(i)
#   
#   if (nchar(j)==5) close_1sub<-substr(j,1,3) else close_1sub<-substr(j,1,4)
#   if (nchar(j)==5) close_2sub<-substr(j,4,5) else close_2sub<-substr(j,5,6)
#   close_2sub1<- sub("8","0",close_2sub)
#   close_sub<-paste0(close_1sub,close_2sub1)
#   if(length(which(close==close_sub)>0)) 
#     {
#     codes_change[k,2]<-close_sub
#     #melt_allact$variable[melt_allact$variable]<-close_sub
#     } 
#     else 
#       {
#         codes_change[k,2]<-NA 
#       }
#   
#   close_2sub2<- sub("8","9",close_2sub)
#   close_sub<-paste0(close_1sub,close_2sub2)
#   if(length(which(close==close_sub)>0)) 
#   {
#     codes_change[k,3]<-close_sub 
#     #melt_allact$variable[melt_allact$variable]<-close_sub
#     
#   }
#     else 
#     {
#       codes_change[k,3]<-NA
#     }
# }  
#  



#all the codes necessary to get information about the activities
#first try is to use ddply to get mean, sd, t-value, participation rate

allstats_allyear<-ddply(melt_allact, c("variable"), summarise,
                description=unique(description),
                rsq=round(summary(lm(value~year))$r.squared,2),
                slope=round(summary(lm(value~year))$coef[2,1],2),
                tvalue=round(summary(lm(value~year))$coef[2,3],2),
                intercept=round(summary(lm(value~year))$coef[1,1],2),
                mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                mean_excl_zeros = round(weightedMean(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                sd_excl_zeros = round(weightedSd(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                numb_na=sum(is.na(value))
                )

temp_stats<-ddply(allstats_peryear,c("variable"),summarize,
                                rsq_mean=round(summary(lm(mean~year))$r.squared,2),
                                slope_mean=round(summary(lm(mean~year))$coef[2,1],2),
                                intercept_mean=round(summary(lm(mean~year))$coef[1,1],2)
                                )

allstats_allyear<-merge(x=allstats_allyear,y=temp_stats,by="variable",all.x = TRUE)
#------------------------ 


#STATISTICS PER YEAR 
allstats_peryear<-ddply(melt_allact, c("variable","year"), summarise,
                        description=unique(description),
                        mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                        sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                        mean_excl_zeros = round(weightedMean(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        sd_excl_zeros = round(weightedSd(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                        numb_na=sum(is.na(value))
)


###TREND FOR EACH ALL ACTIVITIES 

atusact_time<-atusact[,c("TUCASEID","TUACTIVITY_N","TUACTDUR24","TUCUMDUR24","TRCODEP")]
atusact_time<-rename(atusact_time,c("TRCODEP"="variable"))
atusact_time<-merge(x=atusact_time,y=all_act_names,by="variable",all.x = TRUE)
atusact_time<-merge(x=atusact_time,y=file_weight,by="TUCASEID",all.x = TRUE)

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

count<-length(unique(atusact$TUCASEID)) #count the number of unique case id
tot_weight<-sum(file_weight$TUFNWGTP) #total sum of the weights 
allstats_starttime<-ddply(atusact_time, c("variable"), function(df) funStart(df$startid,count))   
allstats_stoptime<-ddply(atusact_time, c("variable"), function(df) funStart(df$stopid,count))   
allstats_pattern<-ddply(atusact_time, c("variable"), function(df) funPattern(df$startid,df$stopid,df$TUFNWGTP,tot_weight))   
#-----------------------------------------------------------------------------------


#EMPLOYMENT BASED RESULTS 

emp<-atussumm[,c('TUCASEID','TELFS','TUFNWGTP','TEAGE')]

emp_present<-emp$TUCASEID[emp$TELFS==1]
emp_absent<-emp$TUCASEID[emp$TELFS==2]
unemp_layoff<-emp$TUCASEID[emp$TELFS==3]
unemp_looking<-emp$TUCASEID[emp$TELFS==4]
notinlaborforce<-emp$TUCASEID[emp$TELFS==5]

save.image()

melt_allact_emp_present<-melt_allact[melt_allact$TUCASEID %in% emp_present,]
melt_allact_emp_absent<-melt_allact[melt_allact$TUCASEID %in% emp_absent ,]
melt_allact_unemp_layoff<-melt_allact[melt_allact$TUCASEID %in% unemp_layoff,]
melt_allact_unemp_looking<-melt_allact[melt_allact$TUCASEID %in% unemp_looking,]
melt_allact_notinlaborforce<-melt_allact[melt_allact$TUCASEID %in% notinlaborforce,]

save.image()


#TREND FOR EACH EMPLOYMENT CATEGORY 
empstats_allyear<-ddply(melt_allact_emp_present, c("variable"), summarise,
                        description=unique(description),
                        rsq=round(summary(lm(value~year))$r.squared,2),
                        slope=round(summary(lm(value~year))$coef[2,1],2),
                        tvalue=round(summary(lm(value~year))$coef[2,3],2),
                        intercept=round(summary(lm(value~year))$coef[1,1],2),
                        mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                        sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                        mean_excl_zeros = round(weightedMean(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        sd_excl_zeros = round(weightedSd(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                        numb_na=sum(is.na(value))
)

temp_stats<-ddply(empstats_peryear,c("variable"),summarize,
                  rsq_mean=round(summary(lm(mean~year))$r.squared,2),
                  slope_mean=round(summary(lm(mean~year))$coef[2,1],2),
                  intercept_mean=round(summary(lm(mean~year))$coef[1,1],2)
)

empstats_allyear<-merge(x=empstats_allyear,y=temp_stats,by="variable",all.x = TRUE)

#---------------------
#notinlaborforce

empstats_allyear<-ddply(melt_allact_emp_present, c("variable"), summarise,
                        description=unique(description),
                        rsq=round(summary(lm(value~year))$r.squared,2),
                        slope=round(summary(lm(value~year))$coef[2,1],2),
                        tvalue=round(summary(lm(value~year))$coef[2,3],2),
                        intercept=round(summary(lm(value~year))$coef[1,1],2),
                        mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                        sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                        mean_excl_zeros = round(weightedMean(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        sd_excl_zeros = round(weightedSd(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                        numb_na=sum(is.na(value))
)

temp_stats<-ddply(empstats_peryear,c("variable"),summarize,
                  rsq_mean=round(summary(lm(mean~year))$r.squared,2),
                  slope_mean=round(summary(lm(mean~year))$coef[2,1],2),
                  intercept_mean=round(summary(lm(mean~year))$coef[1,1],2)
)

empstats_allyear<-merge(x=empstats_allyear,y=temp_stats,by="variable",all.x = TRUE)

#------------------------ 


#STATISTICS PER YEAR 
empstats_peryear<-ddply(melt_allact_emp_present, c("variable","year"), summarise,
                        description=unique(description),
                        mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                        sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                        mean_excl_zeros = round(weightedMean(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        sd_excl_zeros = round(weightedSd(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                        participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                        numb_na=sum(is.na(value))
)


###TREND FOR EACH ALL ACTIVITIES 

empact_time<-atusact[,c("TUCASEID","TUACTIVITY_N","TUACTDUR24","TUCUMDUR24","TRCODEP")]
empact_time<-rename(empact_time,c("TRCODEP"="variable"))
empact_time<-merge(x=empact_time,y=all_act_names,by="variable",all.x = TRUE)
empact_time<-merge(x=empact_time,y=emp,by="TUCASEID",all.x = TRUE)
empact_time<-subset(empact_time,TELFS==1)

#generating start and stop id 
empact_time$startid<-(empact_time$TUCUMDUR24-empact_time$TUACTDUR24)/10
empact_time$stopid<-empact_time$TUCUMDUR24/10


count<-length(unique(emp_present)) #the number of unique employed_work
tot_weight<-sum(emp_present) #the sum of all employed_workers weight 
empstats_starttime<-ddply(empact_time, c("variable"), function(df) funStart(df$startid,count))   
empstats_stoptime<-ddply(empact_time, c("variable"), function(df) funStart(df$stopid,count))   
empstats_pattern<-ddply(empact_time, c("variable"), function(df) funPattern(df$startid,df$stopid,df$TUFNWGTP,tot_weight))   




#output results 
#find all activities where atleast 2% participated 
#order/sorting to generate results 
actstats_allyear[order(-actstats_allyear$participation),]


#participation rate 
#list all activities where more than 5% participated 
p_rate<-subset(actstats_allyear[,c("description","participation")],participation>.05)
p_rate<-p_rate[order(-p_rate$participation),]
f<-ggplot(data = p_rate, aes(y=reorder(description,-participation), x=participation))+geom_point()+
  theme(text = element_text(size=8),
        axis.text.x = element_text(colour="grey20",size=7,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=7,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=7,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=7,angle=90,hjust=.5,vjust=.5,face="plain"))+
  scale_x_continuous(breaks=seq(0,1,by=.2),limits=c(0.05,1))+
  xlab("paricipation ratio") +
  ylab("activities") +
  ggtitle("Participation rate of activities \n (only activities greater than 5% participation sampled)")

f

ggsave(file="participationrate.pdf",width = 6.5)


#mean and percentage of the day of activities averaging atleast or greater than 5% of the day 
p_mean<-subset(actstats_allyear[,c("description","mean")],mean>.005*1440)
p_mean$perc<-p_mean$mean/1440

f<-ggplot(data = p_mean, aes(x=reorder(description,-mean), y=mean))+geom_bar(stat = "identity")+
  theme(text = element_text(size=8),
        axis.text.x = element_text(colour="grey20",size=7,angle=45,hjust=1,vjust=1,face="plain"),
        axis.text.y = element_text(colour="grey20",size=7,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=7,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=7,angle=90,hjust=.5,vjust=.5,face="plain"))+
  #scale_x_continuous(breaks=seq(0,1,by=.2),limits=c(0.05,1))+
  xlab("mean in minutes") +
  ylab("activities") +
  ggtitle("Mean activity duration \n (only activities greater than 0.5% of a day sampled)")

f
ggsave(file="activityduration.pdf",width = 6.5)





stats_maj_perday_peryear <- ddply(melt_majact, c("description","year"), summarise, 
                                 mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                                 sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                                 mean_excl_zeros = round(weightedMean(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                                 sd_excl_zeros = round(weightedSd(value[value!=0],TUFNWGTP[value!=0],na.rm=TRUE),2),
                                 participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                                 numb_na=sum(is.na(value))
                                 )



#statistics per instance 
mean_maj_perinstance <- ddply(atusact, "TRTIER1P", summarise, 
                             mean = round(mean(TUACTDUR24,na.rm=TRUE),2), 
                             mean_excludingzeros = mean(TUACTDUR24[TUACTDUR24!=0],na.rm=TRUE),
                             perc=length(TUACTDUR24),perc_zeros=length(TUACTDUR24[TUACTDUR24!=0]),
                             numb_na=sum(is.na(TUACTDUR24))) 

 










#------code for identifying the name of the variables----------------
#find and change the name of variables 
file_allnames<-unique(melt_allact$variable)
file_subnames<-unique(melt_subact$variable)
file_majnames<-unique(melt_majact$variable)

#using sets to identify the matches
alter_these_allact<-data.frame(setdiff(file_allnames,all_act_names$variable)) #codes that needs altering
allcodes_not_represented<-data.frame(setdiff(all_act_names$variable,file_allnames)) #all codes currently not represented 
colnames(alter_these_allact)<-'variable'
colnames(allcodes_not_represented)<-'variable'
allcodes_not_represented<-merge(x=allcodes_not_represented,y=all_act_names,by='variable',all.x = TRUE)
#-----------------------------------------------------------------------


