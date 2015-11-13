###tofind linear fit of the dataset


#clear all variables in workspace
rm(list = ls())

#set input direcotry
setwd("~/Dropbox/Ongoing_Work/Time_Use/Inputs")


####load file

atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")
atussumm[atussumm==0]<-NA
#set result directory
setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/")

#defining global variables
years<-2003:2014

#selecting the subset of data for processing
tottime_peractivity<-(atussumm[,25:455])


##converting the subset of data into major activity categories and subcategories 

#first get all the list of activities 
act_codes<-read.table("activitycodes.txt")
maj_act_codes<-unique(floor(act_codes/10000))
sub_act_codes<-unique(floor(act_codes/100))


#get the index of the activity codes 

ind_maj_act_codes<-0
ind_sub_act_codes<-0

for (i in 1:length(maj_act_codes$V1))
{
  ind_maj_act_codes[i]<-max(which(floor(act_codes/10000)==maj_act_codes$V1[i]))
  for (j in 1:length(sub_act_codes$V1))
  {
    ind_sub_act_codes[j]<-max(which(floor(act_codes/100)==sub_act_codes$V1[j]))
  }
}


####the dataset of ATUSSUMM for major and sub categories. 

#initializing data frame 
tottime_per_maj_activity<-data.frame(matrix(nrow=length(atussumm$TUCASEID), ncol=length(maj_act_codes$V1)))
rownames(tottime_per_maj_activity)<-atussumm$TUCASEID
colnames(tottime_per_maj_activity)<-maj_act_codes$V1

tottime_per_sub_activity<-data.frame(matrix(nrow=length(atussumm$TUCASEID), ncol=length(sub_act_codes$V1)))
rownames(tottime_per_sub_activity)<-atussumm$TUCASEID
colnames(tottime_per_sub_activity)<-sub_act_codes$V1

idx<-1:2
for (i in 1:length(maj_act_codes$V1))
{
  idx[2]<-ind_maj_act_codes[i]
  tottime_per_maj_activity[,i]<-rowSums(tottime_peractivity[,idx[1]:idx[2]],na.rm=TRUE)
  idx[1]<-idx[2]+1
}
#write data
write.table(tottime_per_maj_activity,file="total time per major activity",sep = ",")

idz<-1:2
for (j in 1:length(sub_act_codes$V1))
  {
  idz[2]<-ind_sub_act_codes[j]
  if (idz[1]!=idz[2])
  {
    tottime_per_sub_activity[,j]<-rowSums(tottime_peractivity[,idz[1]:idz[2]],na.rm=TRUE)
    idz[1]<-idz[2]+1
  }
  else
  {
    tottime_per_sub_activity[,j]<-(tottime_peractivity[,idz[1]])
    idz[1]<-idz[2]+1
  }
}
#write data
write.table(tottime_per_sub_activity,file="total time per sub activity category",sep = ",")




############# quantify linear fit of the data. time ~ years 
allactivity_lm<-data.frame(matrix(ncol=4 ,nrow=431))
rownames(allactivity_lm)<-colnames(tottime_peractivity)
colnames(allactivity_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

###find the fit for all activities,
for (i in 1:length(colnames(tottime_peractivity)))
{
  #to identify the number of "NA" in the dataset  
  chk<-is.na(tottime_peractivity[,i])
  value=length(chk[chk==FALSE])
  
  if(value>100) #random statement we can change this to be more complex. 
  {
    fit<-lm(tottime_peractivity[,i]~atussumm$TUYEAR) # linear fit 
    
    #collecting the fitted values
    allactivity_lm[i,1]<-summary(fit)$coef[2,1]
    allactivity_lm[i,2]<-summary(fit)$coef[2,2]
    allactivity_lm[i,3]<-summary(fit)$coef[2,3]
    allactivity_lm[i,4]<-summary(fit)$adj.r.squared
    
  }
  else
  {
    fit<-0
  }
}




###find the fit for all subcategory activities,

#initialize data frame
sub_activity_lm<-data.frame(matrix(ncol=4 ,nrow=length(sub_act_codes$V1)))
rownames(sub_activity_lm)<-colnames(tottime_per_sub_activity)
colnames(sub_activity_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

for (i in 1:length(colnames(tottime_per_sub_activity)))
{
  #to identify the number of "NA" in the dataset  
  chk<-is.na(tottime_per_sub_activity[,i])
  value=length(chk[chk==FALSE])
  
  if(value>0) #random statement we can change this to be more complex. 
  {
    fit<-lm(tottime_per_sub_activity[,i]~atussumm$TUYEAR) # linear fit 
    
    #collecting the fitted values
    sub_activity_lm[i,1]<-summary(fit)$coef[2,1]
    sub_activity_lm[i,2]<-summary(fit)$coef[2,2]
    sub_activity_lm[i,3]<-summary(fit)$coef[2,3]
    sub_activity_lm[i,4]<-summary(fit)$adj.r.squared
    
  }
  else
  {
    fit<-0
  }
}


###find the fit for all major activities,
maj_activity_lm<-data.frame(matrix(ncol=4 ,nrow=length(maj_act_codes$V1)))
rownames(maj_activity_lm)<-colnames(tottime_per_maj_activity)
colnames(maj_activity_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

for (i in 1:length(colnames(tottime_per_maj_activity)))
{
  #to identify the number of "NA" in the dataset  
  chk<-is.na(tottime_per_maj_activity[,i])
  value=length(chk[chk==FALSE])
  
  if(value>1) #random statement we can change this to be more complex. 
  {
    fit<-lm(tottime_per_maj_activity[,i]~atussumm$TUYEAR) # linear fit 
    
    #collecting the fitted values
    maj_activity_lm[i,1]<-summary(fit)$coef[2,1]
    maj_activity_lm[i,2]<-summary(fit)$coef[2,2]
    maj_activity_lm[i,3]<-summary(fit)$coef[2,3]
    maj_activity_lm[i,4]<-summary(fit)$adj.r.squared
    
  }
  else
  {
    fit<-0
  }
}


library("xlsx")
wb<-createWorkbook()
allactivities<-createSheet(wb,sheetName="allactivities")
subactivities<-createSheet(wb,sheetName="subactivities")
majactivities<-createSheet(wb,sheetName="majactivities")
addDataFrame(allactivity_lm, allactivities)
addDataFrame(sub_activity_lm,subactivities)
addDataFrame(maj_activity_lm,majactivities)
saveWorkbook(wb,"linearmodel.xlsx")










