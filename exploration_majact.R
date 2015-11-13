#exploratory analysis of ATUS major categories for each year


#Author: Ashok Sekar
#data created: Aug 21 2015

#load all input file atussumm, atusresp 
atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")
atusresp<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusresp_0314.dat",header=TRUE,sep=",")
#dataFiles_summ<-lapply(Sys.glob("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activityduration_perday_majactivity.csv"), read.csv) #input atussumm for each maj category

atusact<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusact_0314.dat",header=TRUE,sep=",")


atussumm_majact<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activityduration_perday_majactivity.csv")
#atussumm_subact<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activityduration_perday_subactivity.csv")




#define global variables
years<-2003:2014
act_codes<-read.table("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activitycodes.txt")
maj_act_codes<-unique(floor(act_codes/10000))
sub_act_codes<-unique(floor(act_codes/100))

#formatting input data
rownames(atussumm_majact)<-atussumm_majact[,1]
atussumm_majact<-atussumm_majact[-1]
colnames(atussumm_majact)<-maj_act_codes$V1

atussumm_majact_NA<-atussumm_majact
atussumm_majact_NA[atussumm_majact_NA==0]<-NA



#loading library 
library(matrixStats)
library(xlsx)


#define data.frame to store desc stats 
descstat_majact<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = 5))
rownames(descstat_majact)<-maj_act_codes$V1
colnames(descstat_majact)<-c('mean','sd','mean_withoutzeros','sd_withoutzeros','numberofzeros')


descstat_majact_peryear_mean<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = length(years)))
descstat_majact_peryear_sd<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = length(years)))
descstat_majact_peryear_meanwithoutzeros<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = length(years)))
descstat_majact_peryear_sdwithoutzeros<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = length(years)))
descstat_majact_peryear_zerofreq<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = length(years)))

rownames(descstat_majact_peryear_zerofreq)<-maj_act_codes$V1
rownames(descstat_majact_peryear_sdwithoutzeros)<-maj_act_codes$V1
rownames(descstat_majact_peryear_meanwithoutzeros)<-maj_act_codes$V1
rownames(descstat_majact_peryear_sd)<-maj_act_codes$V1
rownames(descstat_majact_peryear_mean)<-maj_act_codes$V1


colnames(descstat_majact_peryear_zerofreq)<-years
colnames(descstat_majact_peryear_sdwithoutzeros)<-years
colnames(descstat_majact_peryear_meanwithoutzeros)<-years
colnames(descstat_majact_peryear_sd)<-years
colnames(descstat_majact_peryear_mean)<-years



#generate histograms with mean and SD values 
for (i in 1:length(maj_act_codes$V1))
{
  descstat_majact$mean[i]<-weightedMean(atussumm_majact[,i],atussumm$TUFNWGTP)
  descstat_majact$sd[i]<-weightedSd(atussumm_majact[,i],atussumm$TUFNWGTP)
  descstat_majact$mean_withoutzeros[i]<-weightedMean(atussumm_majact_NA[,i],atussumm$TUFNWGTP,na.rm = TRUE)
  descstat_majact$sd_withoutzeros[i]<-weightedSd(atussumm_majact_NA[,i],atussumm$TUFNWGTP,na.rm=TRUE)
  descstat_majact$numberofzeros[i]<-sum(atussumm_majact[,i]==0)/length(atussumm_majact[,1])
  
  id<-1:2
  
  for (j in 1:length(years))
  {
    id[2]<-max(which(atussumm$TUYEAR==years[j]))
      
    descstat_majact_peryear_mean[i,j]<-weightedMean(atussumm_majact[id[1]:id[2],i],atussumm$TUFNWGTP[id[1]:id[2]])
    descstat_majact_peryear_sd[i,j]<-weightedSd(atussumm_majact[id[1]:id[2],i],atussumm$TUFNWGTP[id[1]:id[2]])
    descstat_majact_peryear_meanwithoutzeros[i,j]<-weightedMean(atussumm_majact_NA[id[1]:id[2],i],atussumm$TUFNWGTP[id[1]:id[2]],na.rm = TRUE)
    descstat_majact_peryear_sdwithoutzeros[i,j]<-weightedSd(atussumm_majact_NA[id[1]:id[2],i],atussumm$TUFNWGTP[id[1]:id[2]],na.rm=TRUE)
    descstat_majact_peryear_zerofreq[i,j]<-sum(atussumm_majact[id[1]:id[2],i]==0)/length(atussumm_majact[id[1]:id[2],1])
      
    id[1]<-id[2]+1
  
  }
} 



#set results working directory
setwd("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results")


#saving the results as workbook
wb<-createWorkbook()

allyear<-createSheet(wb,sheetName = "allyear")
peryear_mean<-createSheet(wb,sheetName="peryear_mean")
peryear_sd<-createSheet(wb,sheetName="peryear_sd")
peryear_meanwithoutzeros<-createSheet(wb,sheetName="peryear_meanwithoutzeros")
peryear_sdwithoutzeros<-createSheet(wb,sheetName="peryear_sdwithoutzeros")
peryear_freqzeros<-createSheet(wb,sheetName="peryear_freqzeros")


addDataFrame(descstat_majact, allyear)
addDataFrame(descstat_majact_peryear_mean, peryear_mean)
addDataFrame(descstat_majact_peryear_sd, peryear_sd)
addDataFrame(descstat_majact_peryear_meanwithoutzeros, peryear_meanwithoutzeros)
addDataFrame(descstat_majact_peryear_sdwithoutzeros, peryear_sdwithoutzeros)
addDataFrame(descstat_majact_peryear_zerofreq, peryear_freqzeros)

saveWorkbook(wb,"descstats_majact.xlsx")




#exploratory analysis

rm(list = ls())

atussumm_majact<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activityduration_perday_majactivity.csv")
atusresp<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusresp_0314.dat",header=TRUE,sep=",")

library(plyr)
atussumm_majact<-rename(atussumm_majact,c("X"="TUCASEID"))

weights<-atusresp[,c("TUCASEID","TUFNWGTP")]


library(reshape2)
file<-melt(atussumm_majact,id = "TUCASEID")
file$year<-substr(file$X,1,4)
file$variable<-substring(file$variable,2)

maj_act_names<-read.csv('~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/maj_activity_names.csv',header =FALSE)
act_codes<-read.table("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activitycodes.txt")
maj_act_codes<-unique(floor(act_codes/10000))
sub_act_codes<-unique(floor(act_codes/100))

maj_act_names$variable<-maj_act_codes$V1
file1<-merge(x=file,y=maj_act_names,by='variable',all.x = TRUE)
file2<-merge(x=file1,y=weights,by='TUCASEID',all.x = TRUE)
file1<-file2
rm(file,file2)

library(ggplot2)

file1$variable<-factor(file1$variable,levels=maj_act_codes$V1,labels = maj_act_codes$V1)



#plot as histogram 
dev.off()
plots<-list()
for (i in 1:18)
{
  a<-maj_act_names$variable[i]
  #filename<-paste0('hist_duration_of_maj_activity_',maj_act_names$variable[i],'.pdf')
  
  f<-ggplot(subset(file1,variable==a&value!=0),aes(value))
  fig<-f+geom_histogram(aes(y=..count../sum(..count..),weight=TUFNWGTP),binwidth=60)+
    ylab('density')+
    xlab('activity duration in mins')+
    scale_x_continuous(breaks=seq(0,1440,by=180))+
    scale_y_continuous(breaks=seq(0,1,by=0.1))+
    ggtitle(maj_act_names$V1[i])+
    theme(text = element_text(size=7),
          axis.text.x = element_text(colour="grey20",size=5,angle=0,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=7,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=7,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=7,angle=90,hjust=.5,vjust=.5,face="plain"))
  
  plots[[i]]<-fig
  #ggsave(file=filename)
}

dev.off()
pdf(file = "myplot.pdf",width = 13.33,height = 7.5)
library(Rmisc)
multiplot(plotlist=plots,cols=6)
dev.off()

#labels=c('4AM','6AM','8AM','10AM','12PM','2PM','4PM','6PM','8PM','10PM','12AM','2AM','4AM')






f<-ggplot(subset(file1,value=1),)
f



##plot all year  histogram
###allyear - hist including zeros
f<-qplot(value,data=subset(file1,V1=='personal care activities'),geom = "histogram",binwidth=30,facets = V1~.)
f

f+scale_x_continuous(breaks=seq(0,1440, by=60),lim=c(0,1440))
  
  
  scale_y_continuous(breaks=seq(0,130, by=65)



f<-ggplot(subset(file1,value=1))
f+geom_density()


f+geom_histogram(aes(y=..count..),binwidth=10)+facet_grid(variable~.)+
  xlab('time of day (in mins)')+scale_x_continuous(breaks=limits,lim=c(0,1440))+
  
#scale_y_continuous(breaks=seq(0,0.03,0.015),lim=c(0,0.03))

###allyear - density - excluding zeros
f<-ggplot(subset(file1,value!=0),aes(value))
f+geom_histogram(aes(y=..count..),binwidth=10)+facet_grid(variable~.)+
  xlab('time of day (in mins)')+scale_x_continuous(breaks=limits,lim=c(0,1440))
  
  #scale_y_continuous(breaks=seq(0,0.03,0.015),lim=c(0,0.03))


##plot selected 


###allyear - density - including zeros
f<-ggplot(subset(file1,variable==1|variable==2|variable==11|variable==12|variable==18),
          aes(value))
f+geom_histogram(binwidth=10)+facet_grid(variable~.)+
  xlab('time of day (in mins)')+scale_x_continuous(breaks=limits,lim=c(0,1440))


#plot as density 
f<-ggplot(subset(file1,variable==1|variable==11|variable==12|variable==18),aes(value))
f+geom_freqpoly(aes(y=..count..,colour=V1),binwidth=30)+xlab('time of day (in mins)')+scale_x_continuous(breaks=limits,lim=c(0,1440))


###allyear - density - excluding zeros
f<-ggplot(subset(file1,value!=0&variable==1|variable==2|variable==11|variable==12|variable==18),
          aes(value))
f+geom_histogram(aes(y=..density..),binwidth=10)+facet_grid(variable~.)+
  xlab('time of day (in mins)')+scale_x_continuous(breaks=limits,lim=c(0,1440))+
  scale_y_continuous(breaks=seq(0,0.03,0.015),lim=c(0,0.03))


#plot as density 
f<-ggplot(subset(file1,value!=0&variable==1|variable==11|variable==12|variable==18),
          aes(value))
f+geom_freqpoly(aes(y=..count..,colour=V1),binwidth=10)+
  xlab('time of day (in mins)')+scale_x_continuous(breaks=limits,lim=c(0,1440))


#plot as filled 
f<-ggplot(subset(file1,value!=0&variable==1|variable==11|variable==12|variable==18),
          aes(value))+geom_histogram(aes(fill=variable),binwidth=10,position="jitter")



#plot linear model 

descstat_majact_peryear_meanwithoutzeros$v1<-rownames(descstat_majact_peryear_meanwithoutzeros)

file_lm<-melt(descstat_majact_peryear_meanwithoutzeros,id="v1")


file_lm$v1<-as.factor(file_lm$v1)
file_lm<-subset(file_lm,v1==1|v1==11|v1==12|v1==18)

require(plyr)

lm_eqn <- function(file_lm){ 
  m <- lm(value ~ variable, file_lm);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = (summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq)); 
}

#equation label for all groups
eq <- ddply(file_lm,.(v1),lm_eqn)

#plot
p <- ggplot(data = file_lm, aes(x = variable, y = value)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x,aes(group=1)) +
  geom_point() 
p1 = p + geom_text(data=eq,aes(x = 5, y = 400,label=V1), parse = TRUE, inherit.aes=FALSE) + facet_grid(v1~.)
p1  

freq<-1:12
sd<-1:12
for (i in 1:12)
  {
  a<-subset(count_activities,year==years[i])
  freq[i]<-weighted.mean(a$Freq,a$TUFNWGTP)
  sd[i]<-matrixStats::weightedSd(a$Freq,a$TUFNWGTP)
  }



#plot as histogram 
dev.off() 
plots<-list() 
for (i in 1:12)
{ 
  a<-years[i]
  #filename<-paste0('hist_duration_of_maj_activity_',maj_act_names$variable[i],'.pdf')
  
  f<-ggplot(subset(count_activities,year==a),aes(Freq))
  fig<-f+geom_histogram(aes(y=..count../sum(..count..),weight=TUFNWGTP),binwidth=1)+
    ylab('density')+
    xlab('frequency')+
    scale_x_continuous(breaks=seq(0,90,by=15),limits=c(0,90))+ 
    scale_y_continuous(breaks=seq(0,1,by=0.1))+
    ggtitle(a)
    theme(text = element_text(size=7),
          axis.text.x = element_text(colour="grey20",size=5,angle=0,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=5,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=5,angle=90,hjust=.5,vjust=.5,face="plain"))  
   
  plots[[i]]<-fig
  #ggsave(file=filename)
}  

dev.off()
pdf(file = "myplot.pdf",width = 13.33,height = 7.5)
library(Rmisc)
multiplot(plotlist=plots,cols=2)
dev.off()





f<-qplot(variable, value, data = file_lm, geom = c("point"),method = "lm")
f+facet_grid(v1~.)
