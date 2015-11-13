#this script aims to list codes for quantifying the descriptive statistics of the datasets generated. 

#there are two different datasets 1) activity duration perday, 2) per instance 
#therefore for each statistics different code will be employed 
#the result/output from this code will not be the final results 
#instead the results will be imported to excel from where figures, tables will be created

#the list of descriptive statistics include: 
#min, median, max, median, sd, histograms, 
#number of people doing the activity each year, freq of activity per person each year


#First - descriptive stats for activityduration per day 

rm(list=ls())
#set working directory
setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/")

#loadfiles
atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")
dataFiles <- lapply(Sys.glob("activityduration_perday*.csv"), read.csv)

#globalvariables
years<-2003:2014
act_codes<-read.table("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activitycodes.txt")  
maj_act_codes<-unique(floor(act_codes/10000)) # list of major categories
sub_act_codes<-unique(floor(act_codes/100)) # list of sub categories 

#loading library 
library(matrixStats)
library(xlsx)


#filters for employment status



filename<-NA

for (i in 2)
{
  file<-data.frame(dataFiles[i]) #saving the file in a variable
  rownames(file)<-atussumm$TUCASEID #giving rownames 
  file<-file[,-1] 
  file[file==0]<-NA
  file_zeros<-file
  file_zeros[is.na(file_zeros)]<-0
  
  
  #for (a in 1:5)
  #{
    filename<-paste0('telfs',a,'.xlsx')
    file_zeros_subset<-subset.data.frame(file_zeros,atussumm$TELFS==a)
    file_subset<-subset.data.frame(file,atussumm$TELFS==a)
    atussumm_subset<-subset.data.frame(atussumm,atussumm$TELFS==a)
    
    #defining matrix for all year results
    descstat_perday_allyear<-data.frame(matrix(nrow = length(file[1,]),ncol = 5))
    colnames(descstat_perday_allyear)<-c('min','median','max','mean','sd')
    #defining matrix for per year results 
    descstat_perday_peryear_min<-data.frame(matrix(nrow = length(file[1,]),ncol = length(years)))
    descstat_perday_peryear_median<-data.frame(matrix(nrow = length(file[1,]),ncol = length(years)))
    descstat_perday_peryear_mean<-data.frame(matrix(nrow = length(file[1,]),ncol = length(years)))
    descstat_perday_peryear_max<-data.frame(matrix(nrow = length(file[1,]),ncol = length(years)))
    descstat_perday_peryear_sd<-data.frame(matrix(nrow = length(file[1,]),ncol = length(years)))
    
    
#     
#     
#     if (length(file[1,])==length(act_codes$V1))
#     {
#       #filename<-'descstat_allyears_allact.xlsx'
#       rownames(descstat_perday_allyear)<-act_codes$V1
#       chk<-which(sapply(file,mode)=="logical")
#       file[,chk]<-as.numeric(as.character(file[,chk]))
#     }
#     else if (length(file[1,])==length(sub_act_codes$V1))
#     {
#       #filename<-'descstat_allyears_subact.xlsx'
#       rownames(descstat_perday_allyear)<-sub_act_codes$V1
#       chk<-which(sapply(file,mode)=="logical")
#       file[,chk]<-as.numeric(as.character(file[,chk]))
#     }
#     else
#     {
#       #filename<-'descstat_allyears_majact.xlsx'
#       rownames(descstat_perday_allyear)<-maj_act_codes$V1
#       chk<-which(sapply(file,mode)=="logical")
#       file[,chk]<-as.numeric(as.character(file[,chk]))
#     }
#     
    
    for (k in 1:length(file_zeros[1,]))#for each activity code
    {
      id<-1:2
      
      descstat_perday_allyear$min[k]<-min(file_subset[,k],na.rm = TRUE) #min
      descstat_perday_allyear$max[k]<-max(file_subset[,k],na.rm = TRUE) #max
      descstat_perday_allyear$median[k]<-weightedMedian(file_subset[,k],atussumm_subset$TUFNWGTP,na.rm = TRUE) #median
      descstat_perday_allyear$mean[k]<-weightedMean(file_zeros_subset[,k],atussumm_subset$TUFNWGTP) #mean
      descstat_perday_allyear$sd[k]<-weightedSd(file_zeros_subset[,k],atussumm_subset$TUFNWGTP)
      
      for (j in 1:length(years))# for each year
      {
        id[2]<-max(which(atussumm_subset$TUYEAR==years[j]))
        descstat_perday_peryear_mean[k,j]<-weightedMean(file_zeros_subset[id[1]:id[2],k],atussumm_subset$TUFNWGTP[id[1]:id[2]]) #min
        descstat_perday_peryear_sd[k,j]<-weightedSd(file_zeros_subset[id[1]:id[2],k],atussumm_subset$TUFNWGTP[id[1]:id[2]]) #min
        
        descstat_perday_peryear_min[k,j]<-min(file_subset[id[1]:id[2],k],na.rm = TRUE) #min
        descstat_perday_peryear_max[k,j]<-max(file_subset[id[1]:id[2],k],na.rm = TRUE) #max
        descstat_perday_peryear_median[k,j]<-weightedMedian(file_subset[id[1]:id[2],i],atussumm_subset$TUFNWGTP[id[1]:id[2]],na.rm = TRUE) #median 
        
        id[1]<-id[2]+1
      } 
    }
    
    #save the outputs 
    wb<-createWorkbook()
    desstat_allyears<-createSheet(wb,sheetName="desstat_allyears")
    mean_peryear<-createSheet(wb,sheetName="mean_peryear")
    median_peryear<-createSheet(wb,sheetName="median_peryear")
    sd_peryear<-createSheet(wb,sheetName="sd_peryear")
    min_peryear<-createSheet(wb,sheetName="min_peryear")
    max_peryear<-createSheet(wb,sheetName="max_peryear")
    
    addDataFrame(descstat_perday_peryear_min, min_peryear)
    addDataFrame(descstat_perday_peryear_median, median_peryear)
    addDataFrame(descstat_perday_peryear_mean, mean_peryear)
    addDataFrame(descstat_perday_peryear_sd, sd_peryear)
    addDataFrame(descstat_perday_peryear_max, max_peryear)
    addDataFrame(descstat_perday_allyear, desstat_allyears)
    
    saveWorkbook(wb,filename)
    rm(wb, desstat_allyears, mean_peryear,median_peryear,sd_peryear, min_peryear, max_peryear)
    
  #} 
  
}

  
    
