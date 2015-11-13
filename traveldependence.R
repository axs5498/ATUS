###clear all variables in workspace
rm(list = ls())

#set input direcotry
setwd("~/Dropbox/Ongoing_Work/Time_Use/Inputs")

####load file

atusact<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusact_0314.dat",header=TRUE,sep=",")
actsumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")

#set result directory
setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/")
act_codes<-read.table("activitycodes.txt")


#defining global variables
years<-2003:2014

#subset dataset with only the following variables: 
# CASEID, majactivity, subactivity, allactivities, activitynumber 

file<-atusact[c(1:3,11:13)]



##activities before and after travel 

travel_id<-which(atusact$TRTIER1P==18)

twoabove<-(travel_id-2)
oneabove<-(travel_id-1)
onebelow<-(travel_id+1)
twobelow<-(travel_id+2)

act_twoabove<-data.frame(matrix(nrow = length(travel_id),ncol=5))
act_oneabove<-data.frame(matrix(nrow = length(travel_id),ncol=5))
act_onebelow<-data.frame(matrix(nrow = length(travel_id),ncol=5))
act_twobelow<-data.frame(matrix(nrow = length(travel_id),ncol=5))


act_twoabove$X1<-atusact$TRTIER1P[twoabove]
act_twoabove$X2<-atusact$TRTIER2P[twoabove]
act_twoabove$X3<-atusact$TRCODEP[twoabove]
act_twoabove$X4<-atusact$TUACTDUR24[twoabove]
act_twoabove$X5<-atusact$TUSTARTTIM[twoabove]

act_oneabove$X1<-atusact$TRTIER1P[oneabove]
act_oneabove$X2<-atusact$TRTIER2P[oneabove]
act_oneabove$X3<-atusact$TRCODEP[oneabove]
act_oneabove$X4<-atusact$TUACTDUR24[oneabove]
act_oneabove$X5<-atusact$TUSTARTTIM[oneabove]

act_onebelow$X1<-atusact$TRTIER1P[onebelow]
act_onebelow$X2<-atusact$TRTIER2P[onebelow]
act_onebelow$X3<-atusact$TRCODEP[onebelow]
act_onebelow$X4<-atusact$TUACTDUR24[onebelow]
act_onebelow$X5<-atusact$TUSTARTTIM[onebelow]

act_twobelow$X1<-atusact$TRTIER1P[twobelow]
act_twobelow$X2<-atusact$TRTIER2P[twobelow]
act_twobelow$X3<-atusact$TRCODEP[twobelow]
act_twobelow$X4<-atusact$TUACTDUR24[twobelow]
act_twobelow$X5<-atusact$TUSTARTTIM[twobelow]

colnames(act_twobelow)<-c('maj_category','sub_category','act_code','duration','starttime')
colnames(act_onebelow)<-c('maj_category','sub_category','act_code','duration','starttime')
colnames(act_oneabove)<-c('maj_category','sub_category','act_code','duration','starttime')
colnames(act_twoabove)<-c('maj_category','sub_category','act_code','duration','starttime')



library("xlsx")
wb<-createWorkbook()
twoabove<-createSheet(wb,sheetName="twoabove")
oneabove<-createSheet(wb,sheetName="oneabove")
onebelow<-createSheet(wb,sheetName="onebelow")
twobelow<-createSheet(wb,sheetName="twobelow")


addDataFrame(act_twobelow, twobelow)
addDataFrame(act_twoabove, twoabove)
addDataFrame(act_onebelow, onebelow)
addDataFrame(act_oneabove, oneabove)


saveWorkbook(wb,"traveldependence.xlsx")






sort(table(act_oneabove$maj_category))/length(travel_id)


