#linear fit of all the means
rm(list=ls())
#load the xlsx files from the previous results 
library(xlsx)
mean_telfs1_perday<-read.xlsx('telfs1.xlsx',2)
mean_telfs2_perday<-read.xlsx('telfs2.xlsx',2)
mean_telfs3_perday<-read.xlsx('telfs3.xlsx',2)
mean_telfs4_perday<-read.xlsx('telfs4.xlsx',2)
mean_telfs5_perday<-read.xlsx('telfs5.xlsx',2)



#activitiy codes 
act_codes<-read.table("~/Dropbox/Ongoing_Work/Time_Use/activitydemand/Results/activitycodes.txt")
maj_act_codes<-unique(floor(act_codes/10000))
sub_act_codes<-unique(floor(act_codes/100))

years<-2003:2014

#formatting the files 
mean_telfs1_perday<-mean_telfs1_perday[-1]
mean_telfs2_perday<-mean_telfs2_perday[-1]
mean_telfs3_perday<-mean_telfs3_perday[-1]
mean_telfs4_perday<-mean_telfs4_perday[-1]
mean_telfs5_perday<-mean_telfs5_perday[-1]


mean_telfs1_perday<-data.matrix(mean_telfs1_perday,rownames.force = NA)
mean_telfs2_perday<-data.matrix(mean_telfs2_perday,rownames.force = NA)
mean_telfs3_perday<-data.matrix(mean_telfs3_perday,rownames.force = NA)
mean_telfs4_perday<-data.matrix(mean_telfs4_perday,rownames.force = NA)
mean_telfs5_perday<-data.matrix(mean_telfs5_perday,rownames.force = NA)



#getting dataset for fit

#for maj categories

telfs1_lm<-data.frame(matrix(ncol=4 ,nrow=length(maj_act_codes$V1)))
rownames(telfs1_lm)<-maj_act_codes$V1
colnames(telfs1_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

telfs2_lm<-data.frame(matrix(ncol=4 ,nrow=length(maj_act_codes$V1)))
rownames(telfs2_lm)<-maj_act_codes$V1
colnames(telfs2_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

telfs3_lm<-data.frame(matrix(ncol=4 ,nrow=length(maj_act_codes$V1)))
rownames(telfs3_lm)<-maj_act_codes$V1
colnames(telfs3_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

telfs4_lm<-data.frame(matrix(ncol=4 ,nrow=length(maj_act_codes$V1)))
rownames(telfs4_lm)<-maj_act_codes$V1
colnames(telfs4_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')

telfs5_lm<-data.frame(matrix(ncol=4 ,nrow=length(maj_act_codes$V1)))
rownames(telfs5_lm)<-maj_act_codes$V1
colnames(telfs5_lm)<-c('estimate','std. error','t value','adjusted R-Sq.')




for (i in 1:length(maj_act_codes$V1))
{
  
  #to identify the number of "NA" in the dataset  
#   chk<-is.na(mean_telfs1_perday[i,])
#   value=length(chk[chk==FALSE])
  
#   if(value>0) #random statement we can change this to be more complex. 
#   {
    fit1<-lm(mean_telfs1_perday[i,]~years) # linear fit 
    fit2<-lm(mean_telfs2_perday[i,]~years) # linear fit 
    fit3<-lm(mean_telfs3_perday[i,]~years) # linear fit 
    fit4<-lm(mean_telfs4_perday[i,]~years) # linear fit 
    fit5<-lm(mean_telfs5_perday[i,]~years) # linear fit 
    
    
    #collecting the fitted values
    telfs1_lm[i,1]<-summary(fit1)$coef[2,1]
    telfs1_lm[i,2]<-summary(fit1)$coef[2,2]
    telfs1_lm[i,3]<-summary(fit1)$coef[2,3]
    telfs1_lm[i,4]<-summary(fit1)$adj.r.squared
    
    #collecting the fitted values
    telfs2_lm[i,1]<-summary(fit2)$coef[2,1]
    telfs2_lm[i,2]<-summary(fit2)$coef[2,2]
    telfs2_lm[i,3]<-summary(fit2)$coef[2,3]
    telfs2_lm[i,4]<-summary(fit2)$adj.r.squared
    
    #collecting the fitted values
    telfs3_lm[i,1]<-summary(fit3)$coef[2,1]
    telfs3_lm[i,2]<-summary(fit3)$coef[2,2]
    telfs3_lm[i,3]<-summary(fit3)$coef[2,3]
    telfs3_lm[i,4]<-summary(fit3)$adj.r.squared
    
    #collecting the fitted values
    telfs4_lm[i,1]<-summary(fit4)$coef[2,1]
    telfs4_lm[i,2]<-summary(fit4)$coef[2,2]
    telfs4_lm[i,3]<-summary(fit4)$coef[2,3]
    telfs4_lm[i,4]<-summary(fit4)$adj.r.squared
  
    #collecting the fitted values
    telfs5_lm[i,1]<-summary(fit5)$coef[2,1]
    telfs5_lm[i,2]<-summary(fit5)$coef[2,2]
    telfs5_lm[i,3]<-summary(fit5)$coef[2,3]
    telfs5_lm[i,4]<-summary(fit5)$adj.r.squared
    
#   }
#   else
#   {
#     fit<-0
#   }

}






library("xlsx")
wb<-createWorkbook()
telfs1<-createSheet(wb,sheetName="telfs1")
telfs2<-createSheet(wb,sheetName="telfs2")
telfs3<-createSheet(wb,sheetName="telfs3")
telfs4<-createSheet(wb,sheetName="telfs4")
telfs5<-createSheet(wb,sheetName="telfs5")


addDataFrame(telfs1_lm, telfs1)
addDataFrame(telfs2_lm, telfs2)
addDataFrame(telfs3_lm, telfs3)
addDataFrame(telfs4_lm, telfs4)
addDataFrame(telfs5_lm, telfs5)


saveWorkbook(wb,"linearmodel_telfs.xlsx")


