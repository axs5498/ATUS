#script to find activity statistics 
#number of person who performed an activity each year 
#number of times each activity was conducted per person each year 


#load files
atusact<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusact_0314.dat",header=TRUE,sep=",")
dataFiles<-lapply(Sys.glob("activityduration_perinstance*.csv"), read.csv)

file<-data.frame(dataFiles) #saving the file in a variable
file<-file[,-1] 



#global variables
years<-2003:2014
act_codes<-read.table("activitycodes")
maj_act_codes<-unique(floor(act_codes/10000))
sub_act_codes<-unique(floor(act_codes/100))

file$year<-substr(file$TUCASEID,1,4)

#number of person performing each activity per year 

ds_all<-data.frame(matrix(nrow = length(act_codes$V1),ncol = length(years)))
ds_maj<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = length(years)))
ds_sub<-data.frame(matrix(nrow = length(sub_act_codes$V1),ncol = length(years)))
colnames(ds_sub)<-years
colnames(ds_maj)<-years
colnames(ds_all)<-years
rownames(ds_sub)<-sub_act_codes$V1
rownames(ds_maj)<-maj_act_codes$V1
rownames(ds_all)<-act_codes$V1


for (j in 1:length(years))
{
  file_subset<-subset.data.frame(file,year==years[j])
  
  for (i in 4:6)
  {
    filename<-colnames(file[i])
    
    #a<-data.frame(table(file_subset[,i]))
    
    if (filename=='TRCODEP')
    {
      for (k in 1:length(act_codes$V1))
      {
        a<-sum(file_subset[,i]==act_codes$V1[k])
        ds_all[k,j]<-a
      }
    }
    
    else if(filename=='TRTIER1P')
    {
      for (k in 1:length(maj_act_codes$V1))
      {
        a<-sum(file_subset[,i]==maj_act_codes$V1[k])
        ds_maj[k,j]<-a
      }
    }
    
    else
    {
      for (k in 1:length(sub_act_codes$V1))
      {
        a<-sum(file_subset[,i]==sub_act_codes$V1[k])
        ds_sub[k,j]<-a
      }
    }
    
  }
}


library("xlsx")
wb<-createWorkbook()
allactivities<-createSheet(wb,sheetName="allactivities")
subactivities<-createSheet(wb,sheetName="subactivities")
majactivities<-createSheet(wb,sheetName="majactivities")
addDataFrame(ds_all, allactivities)
addDataFrame(ds_sub,subactivities)
addDataFrame(ds_maj,majactivities)
saveWorkbook(wb,"activityfreq.xlsx")




