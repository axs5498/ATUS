#linear fit of all the means
rm(list=ls())
#load the xlsx files from the previous results 

#load activitiy file
atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")
file<-read.csv('~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activityduration_perday_majactivity.csv')#read time spent by each person doing maj activity
library(reshape2)
library(plyr)
file<-rename(file,c("X"="TUCASEID"))
file<-merge(x=file,y=atussumm[,c("TUCASEID","TUFNWGTP","TUYEAR")],by='TUCASEID',all.x = TRUE)


rownames(file)<-file[,1]
file<-file[,-1]


act_codes<-read.table("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activitycodes.txt")
maj_act_codes<-unique(floor(act_codes/10000))
sub_act_codes<-unique(floor(act_codes/100))

years<-2003:2014

mean_maj_perday<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = length(years)))
rownames(mean_maj_perday)<-maj_act_codes$V1
colnames(mean_maj_perday)<-years

mean_maj_perday_wzero<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = length(years)))
rownames(mean_maj_perday_wzero)<-maj_act_codes$V1
colnames(mean_maj_perday_wzero)<-years

population_peryear<-data.frame(matrix(nrow = length(maj_act_codes$V1),ncol = length(years)))
rownames(population_peryear)<-maj_act_codes$V1
colnames(population_peryear)<-years



#file[file==0]<-NA

library(matrixStats)

for (i in 1:12)
{
  file_subset<-subset(file,TUYEAR==years[i])
  for (j in 1:18)
  {
    population_peryear[j,i]<-sum(file_subset$TUFNWGTP[file_subset[,j]!=0])/sum(file_subset$TUFNWGTP)
    mean_maj_perday_wzero[j,i]<-weightedMean(file_subset[,j],file_subset$TUFNWGTP)
    file_subset[file_subset==0]<-NA 
    mean_maj_perday[j,i]<-weightedMean(file_subset[,j],file_subset$TUFNWGTP,na.rm = TRUE)
    file_subset[is.na(file_subset)]<-0
  } 
}
  
    


# 
# 
# library(xlsx)
# mean_maj_perday<-read.xlsx('descstat_allyears_majact.xlsx',2)
# mean_sub_perday<-read.xlsx('descstat_allyears_subact.xlsx',2)
# mean_all_perday<-read.xlsx('descstat_allyears_allact.xlsx',2)


# #formatting the files 
# mean_maj_perday<-mean_maj_perday[-1]
# mean_sub_perday<-mean_sub_perday[-1]
# mean_all_perday<-mean_all_perday[,3:14]

mean_maj_perday<-data.matrix(mean_maj_perday,rownames.force = NA)
mean_maj_perday_wzero<-data.matrix(mean_maj_perday_wzero,rownames.force = NA)


# 
# mean_sub_perday<-data.matrix(mean_sub_perday,rownames.force = NA)
# mean_all_perday<-data.matrix(mean_all_perday,rownames.force = NA)



#getting dataset for fit

#for maj categories

maj_activity_lm<-data.frame(matrix(ncol=13 ,nrow=length(maj_act_codes$V1)))
rownames(maj_activity_lm)<-maj_act_codes$V1
colnames(maj_activity_lm)<-c('slope','std.error','tvalue','R-Sq.','mean','sd',
                             'slope_wzero','std.error_wzeros','tvalue_wzeros','R-Sq._wzeros','mean_wzeros','sd_wzeros',
                             'population')

reg_outputs<-data.frame(matrix(ncol=4,nrow=18))
colnames(reg_outputs)<-c('x','y','label','V1')

reg_outputs$x<-2011
reg_outputs$y<-430
reg_outputs$V1<-maj_activity_names$V1


for (i in 1:length(maj_act_codes$V1))
{
  file[is.na(file)]<-0
  maj_activity_lm[i,11]<-weightedMean(file[,i],file$TUFNWGTP)
  maj_activity_lm[i,12]<-weightedSd(file[,i],file$TUFNWGTP)
  maj_activity_lm[i,13]<-sum(file$TUFNWGTP[file[,i]!=0])/sum(file$TUFNWGTP) 
  
  
  file[file==0]<-NA
  maj_activity_lm[i,5]<-weightedMean(file[,i],file$TUFNWGTP,na.rm = TRUE)
  maj_activity_lm[i,6]<-weightedSd(file[,i],file$TUFNWGTP,na.rm = TRUE)
  
  
  #to identify the number of "NA" in the dataset  
  chk<-is.na(mean_maj_perday[i,])
  value=length(chk[chk==FALSE])
  
  if(value>0) #random statement we can change this to be more complex. 
  {
    fit<-lm(mean_maj_perday[i,]~years) # linear fit 
    fit1<-lm(mean_maj_perday_wzero[i,]~years)
    
    #collecting the fitted values
    maj_activity_lm[i,1]<-summary(fit)$coef[2,1]
    maj_activity_lm[i,2]<-summary(fit)$coef[2,2]
    maj_activity_lm[i,3]<-summary(fit)$coef[2,3]
    maj_activity_lm[i,4]<-summary(fit)$r.squared
    
    #reg_outputs$label[i]<-paste('slope =',round(summary(fit)$coef[2,1],2),'     r-sq.=',round(summary(fit)$r.squared,2))
    
    
    #collecting the fitted values _wzeros
    maj_activity_lm[i,7]<-summary(fit1)$coef[2,1]
    maj_activity_lm[i,8]<-summary(fit1)$coef[2,2]
    maj_activity_lm[i,9]<-summary(fit1)$coef[2,3]
    maj_activity_lm[i,10]<-summary(fit1)$r.squared
    reg_outputs$label[i]<-paste('slope =',round(summary(fit1)$coef[2,1],2),'     r-sq.=',round(summary(fit)$r.squared,2))
    
    
  }
  else
  {
    fit<-0
  }
}

library(reshape2)
library(ggplot2)
melt_file<-melt(mean_maj_perday)
#melt_file$Var2<-factor(melt_file$Var2)
maj_activity_names<-read.csv('~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/maj_activity_names.csv',header = FALSE)

melt_file_zero<-melt(mean_maj_perday_wzero)# this for without mean


#plot as histogram 
dev.off() 
plots<-list() 
for (i in 1:18)
{ 
  a<-maj_act_codes$V1[i]
  #filename<-paste0('hist_duration_of_maj_activity_',maj_act_names$variable[i],'.pdf')
  
  fit<-lm(value~Var2,data=subset(melt_file,Var1==a)) # linear fit 
  #fit1<-lm(mean_maj_perday_wzero[i,]~years)
  
  b0 <- round(summary(fit)$coef[1,1], 2)
  b1 <- round(summary(fit)$coef[2,1],2)
  r2 <- round(summary(fit)$r.squared, 2)
  eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~r^2 == .(r2))
  eqn<-as.character(as.expression(eqn))
  
  f<-ggplot(subset(melt_file,Var1==a),aes(Var2,value))
  fig<-f+geom_point()
    fig<-fig+geom_abline(intercept=summary(fit)$coef[1,1],slope=summary(fit)$coef[2,1],colour="blue",size=0.5)
    #fig <- fig + geom_text(size=2,aes(x = 2006, y = max(fit$fitted.values), label = eqn), parse = TRUE)+
      fig <- fig + annotate("text",x = 2007, y = 650, label = paste('slope =',b1,'\n r sq.=',r2,'\n'),size=2)+
      ylab('duration (in mins)')+
    xlab('year')+
    scale_x_continuous(breaks=seq(2003,2014,by=2),limits=c(2003,2014))+ 
    scale_y_continuous(breaks=seq(0,720,by=120))+
    ggtitle(maj_activity_names$V1[i])+
  theme(text = element_text(size=7),
        axis.text.x = element_text(colour="grey20",size=7,angle=45,hjust=.5,vjust=.5,face="plain"),
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





#trying a facet plot 

maj_activity_names$Var1<-maj_act_codes$V1
melt_file1<-merge(x=melt_file,y=maj_activity_names,by='Var1',all.x = TRUE)

f<-ggplot(data=melt_file1,aes(x=Var2,y=value))+geom_point(size=2,alpha=0.3)+geom_smooth(method=lm,colour="black")+facet_wrap(~V1,nrow=6)
fig<-f+theme(text = element_text(size=10),
             axis.text.x = element_text(colour="grey20",size=10,angle=45,hjust=.5,vjust=.5,face="plain"),
             axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=1,vjust=0,face="plain"),  
             axis.title.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=0,face="plain"),
             axis.title.y = element_text(colour="grey20",size=10,angle=90,hjust=.5,vjust=.5,face="plain"),
             strip.text=element_text(size = 10))+
  ylab('duration (in mins)')+
  xlab('year')+
  scale_x_continuous(breaks=seq(2003,2014,by=2),limits=c(2003,2014))+ 
  scale_y_continuous(breaks=seq(0,600,by=240),limits=c(0,600))+
  geom_text(data=reg_outputs,aes(x,y,label=label),inherit.aes=FALSE,size=3)  

fig 

ggsave(file="linearfit.pdf",width = 13.33,height = 7.5)





#plot for with zeros

melt_file_zero<-melt(mean_maj_perday_wzero)# this for without mean


#plot as histogram 
dev.off() 
plots<-list() 
for (i in 1:18)
{ 
  a<-maj_act_codes$V1[i]
  #filename<-paste0('hist_duration_of_maj_activity_',maj_act_names$variable[i],'.pdf')
  
  fit<-lm(value~Var2,data=subset(melt_file_zero,Var1==a)) # linear fit 
  #fit1<-lm(mean_maj_perday_wzero[i,]~years)
  
  b0 <- round(summary(fit)$coef[1,1], 2)
  b1 <- round(summary(fit)$coef[2,1],2)
  r2 <- round(summary(fit)$r.squared, 2)
  eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~r^2 == .(r2))
  eqn<-as.character(as.expression(eqn))
  
  f<-ggplot(subset(melt_file_zero,Var1==a),aes(Var2,value))
  fig<-f+geom_point()
  fig<-fig+geom_abline(intercept=summary(fit)$coef[1,1],slope=summary(fit)$coef[2,1],colour="blue",size=0.5)
  #fig <- fig + geom_text(size=2,aes(x = 2006, y = max(fit$fitted.values), label = eqn), parse = TRUE)+
  fig <- fig + annotate("text",x = 2011, y = min(f$data$value), label = paste('slope =',b1,'\n r sq.=',r2,'\n'),size=2)+
    ylab('duration (in mins)')+
    xlab('year')+
    scale_x_continuous(breaks=seq(2003,2014,by=2),limits=c(2003,2014))+ 
    scale_y_continuous(breaks=seq(0,720,by=120))+
    ggtitle(maj_activity_names$V1[i])+
    theme(text = element_text(size=7),
          axis.text.x = element_text(colour="grey20",size=7,angle=45,hjust=.5,vjust=.5,face="plain"),
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


#trying a facet plot 

maj_activity_names$Var1<-maj_act_codes$V1
melt_file_zero1<-merge(x=melt_file_zero,y=maj_activity_names,by='Var1',all.x = TRUE)

f<-ggplot(data=melt_file_zero1,aes(x=Var2,y=value))+geom_point(size=2,alpha=0.3)+geom_smooth(method=lm,colour="black")+facet_wrap(~V1,nrow=6)
fig<-f+theme(text = element_text(size=10),
             axis.text.x = element_text(colour="grey20",size=10,angle=45,hjust=.5,vjust=.5,face="plain"),
             axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=1,vjust=0,face="plain"),  
             axis.title.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=0,face="plain"),
             axis.title.y = element_text(colour="grey20",size=10,angle=90,hjust=.5,vjust=.5,face="plain"),
             strip.text=element_text(size = 10))+
  ylab('duration (in mins)')+
  xlab('year')+
  scale_x_continuous(breaks=seq(2003,2014,by=2),limits=c(2003,2014))+ 
  scale_y_continuous(breaks=seq(0,600,by=240),limits=c(0,600))+
  geom_text(data=reg_outputs,aes(x,y,label=label),inherit.aes=FALSE,size=3)  

fig 

ggsave(file="linearfit.pdf",width = 13.33,height = 7.5)






library("xlsx")
wb<-createWorkbook()

mean_per_year<-createSheet(wb,sheetName="mean_per_year")
mean_wzeros_per_year<-createSheet(wb,sheetName="mean_wzeros_per_year")
population_per_year<-createSheet(wb,sheetName="population_per_year")
allyear<-createSheet(wb,sheetName="allyear")

addDataFrame(mean_maj_perday,mean_per_year)
addDataFrame(mean_maj_perday_wzero,mean_wzeros_per_year)
addDataFrame(population_peryear,population_per_year)
addDataFrame(maj_activity_lm,allyear)

saveWorkbook(wb,"linearmodel_majact.xlsx")














#for sub categories

file_sub<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activityduration_perday_subactivity.csv")
file_sub<-rename(file_sub,c("X"="TUCASEID"))
file_sub<-merge(x=file_sub,y=atussumm[,c("TUCASEID","TUFNWGTP","TUYEAR")],by='TUCASEID',all.x = TRUE)



sub_activity_lm<-data.frame(matrix(ncol=4 ,nrow=length(sub_act_codes$V1)))
rownames(sub_activity_lm)<-sub_act_codes$V1
colnames(sub_activity_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

for (i in 1:length(sub_act_codes$V1))
{
  #to identify the number of "NA" in the dataset  
  chk<-is.na(mean_sub_perday[i,])
  value=length(chk[chk==FALSE])
  
  if(value>0) #random statement we can change this to be more complex. 
  {
    fit<-lm(mean_sub_perday[i,]~years) # linear fit 
    
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


#for all categories
all_activity_lm<-data.frame(matrix(ncol=4 ,nrow=length(act_codes$V1)))
rownames(all_activity_lm)<-act_codes$V1
colnames(all_activity_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

for (i in 1:length(act_codes$V1))
{
  #to identify the number of "NA" in the dataset  
  chk<-is.na(mean_all_perday[i,])
  value=length(chk[chk==FALSE])
  
  if(value>0) #random statement we can change this to be more complex. 
  {
    fit<-lm(mean_all_perday[i,]~years) # linear fit 
    
    #collecting the fitted values
    all_activity_lm[i,1]<-summary(fit)$coef[2,1]
    all_activity_lm[i,2]<-summary(fit)$coef[2,2]
    all_activity_lm[i,3]<-summary(fit)$coef[2,3]
    all_activity_lm[i,4]<-summary(fit)$adj.r.squared
    
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
addDataFrame(all_activity_lm, allactivities)
addDataFrame(sub_activity_lm,subactivities)
addDataFrame(maj_activity_lm,majactivities)
saveWorkbook(wb,"linearmodel_withmeanonly.xlsx")

#the plot for major categories alone 

##the idea is ---- 3 subplots with 

#for each major activity 
# plot the major, sub and all category 
#save as figure 

maj_activity_lm$maj_act_codes<-maj_act_codes$V1
sub_activity_lm$sub_act_codes<-sub_act_codes$V1
all_activity_lm$act_codes<-act_codes$V1

rownames(maj_activity_lm) <- 1:nrow(maj_activity_lm)
rownames(sub_activity_lm) <- 1:nrow(sub_activity_lm)
rownames(all_activity_lm) <- 1:nrow(all_activity_lm)

#reading files that contain description of activities 
maj_codes_desc<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/majactivitycodes_description.csv")
sub_codes_desc<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/subactivitycodes_description.csv")
all_codes_desc<-read.csv("~/Dropbox/Ongoing_Work/Time_Use/Inputs/allactivitycodes_description.csv")

#merging the files 
all_activity_lm[,6:7]<-merge(act_codes,all_codes_desc,by.x = "V1",all.x=TRUE)
sub_activity_lm[,6:7]<-merge(sub_act_codes,sub_codes_desc,by.x = "V1",all.x=TRUE)
maj_activity_lm[,6:7]<-merge(maj_act_codes,maj_codes_desc,by.x = "V1",all.x=TRUE)


# Generate data
maj<-ggplot(maj_activity_lm[1,], aes(x=factor(description),y=estimate))+geom_bar(stat="identity",width=0.2)
sub<-ggplot(sub_activity_lm[1:6,], aes(x=factor(description),y=estimate))+geom_bar(stat="identity",width=0.5)
all<-ggplot(all_activity_lm[1:12,], aes(x=factor(description),y=estimate))+geom_bar(stat="identity",width=0.5)

