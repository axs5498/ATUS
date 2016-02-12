#dataset generation for each location 
##the objective of this code is to find activityds.day datasets for each location.
##in otherworkds this dataset will help in find what are the activities that are done at each 
##location and the dynamics of those 

#structure of code 
##The structure of the code follows the same as dataset gen to create the activityds.day except we
##will have 29 small datasets.
whereactivityds.day<-list()


#cast the dataset into list for each year 
require(reshape)
necessary.colnames<-c('TUCASEID','TUACTDUR24','TRCODEP','TEWHERE')
main.ds<-atusact
main.ds$year<-as.numeric(substr(main.ds$TUCASEID,1,4))

allwhere<-unique(main.ds$TEWHERE)



for (a in 24:length(allwhere))
{
  #create a location based dataset 
  ds<-main.ds[main.ds$TEWHERE %in% allwhere[a],]
  desc<-as.character(wherenames$description[wherenames$variable==allwhere[a]])
  
  ##initializing other variables 
  id<-cumsum(table(ds$year))
  j<-1
  chk<-list()
  for (i in 1:length(unique(ds$year))) 
  {
    chk[[i]]<-cast(ds[j:id[i],necessary.colnames],
                   TUCASEID~TRCODEP,value = "TUACTDUR24",sum)
    j<-id[i]+1
  }
  ###add missing columns and create a new dataset and rbind it 
  temp.activityds.day<-data.frame()
  allact<-sort(unique(main.ds$TRCODEP)) 
  colnames.activityds.day<-c("TUCASEID",as.character(allact))
  temp.activityds.day<-data.frame(matrix(ncol = length(colnames.activityds.day)))
  colnames(temp.activityds.day)<-colnames.activityds.day
  for (i in 1:length(unique(ds$year)))
  {
    #addmissingcols
    missing.Cols<-setdiff(colnames.activityds.day,colnames(chk[[i]]))
    caseid<-unique(main.ds$TUCASEID[main.ds$year==unique(ds$year)[i]])
    chk[[i]][,c(missing.Cols)]<-NA
    #addmissingrows
    missing.Rows<-setdiff(caseid,chk[[i]]$TUCASEID)
    l<-length(chk[[i]]$TUCASEID)+1
    n<-length(chk[[i]]$TUCASEID)+length(missing.Rows)
    chk[[i]][l:n,]<-NA
    chk[[i]]$TUCASEID[l:n]<-missing.Rows
    temp.activityds.day<-rbind(temp.activityds.day,chk[[i]])
  }
  temp.activityds.day<-temp.activityds.day[-1,]
  temp.activityds.day[is.na(temp.activityds.day)]<-0
  #formating 
  #temp<-melt(temp.activityds.day,id.vars = c("TUCASEID")) 
  #temp.activityds.day<-temp
  temp.activityds.day$year<-as.numeric(substr(temp.activityds.day$TUCASEID,1,4))
  #mergin
  temp.activityds.day$TUFNWGTP<-atussum$TUFNWGTP[match(temp.activityds.day$TUCASEID,
                                               atussum$TUCASEID)]
  #renaming
  temp.activityds.day<-rename(temp.activityds.day,c('TUFNWGTP'='weight'))
  #saving in the new list 
  whereactivityds.day[[desc]]<-temp.activityds.day
}
 
save.image()


#######################################
######################################
#doing the stats for the list 
desc.stat.allwhere<-data.frame()
temp.desc.stat.allwhere<-data.frame()



for (i in 1:length(allwhere))
{
  ds2<-whereactivityds.day[[i]]
  
  for (j in filter)
  {
    
    #creating the new datast 
    id<-filter.file$TUCASEID[filter.file[,j]==1]
    ds1<-ds2[ds2$TUCASEID %in% id,]
    ###code to calculate the yearly statistics 
    require(plyr)
    require(matrixStats)
    
    n.mean<-data.frame()
    p.mean<-data.frame()
    n.sd<-data.frame()
    p.sd<-data.frame()
    n.prate<-data.frame()
    p.min<-data.frame()
    p.max<-data.frame()
    zz<-NULL 
    y<-unique(ds1$year)
    for (m in 1:length(y))
    {
      ds<-ds1[ds1$year %in% y[m],]
      for (k in 1:length(allact))
      {
        id.s<-which(colnames(ds)==allact[k])
        
        
        n.mean[m,k]<-round(weightedMean(ds[,id.s],ds[,'weight']),2)
        p.mean[m,k] <- round(weightedMean(ds[,id.s][ds[,id.s]!=0],ds[,'weight'][ds[,id.s]!=0]),2)
        n.sd[m,k] <- round(weightedSd(ds[,id.s],ds[,'weight']),2)
        p.sd[m,k] <- round(weightedSd(ds[,id.s][ds[,id.s]!=0],ds[,'weight'][ds[,id.s]!=0]),2)
        n.prate[m,k] <- round(sum(ds[,'weight'][ds[,id.s]!=0])/sum(ds[,'weight']),2)
        p.min[m,k] <- ifelse(length(ds[,id.s][ds[,id.s]!=0])==0,NA,
                             min(ds[,id.s][ds[,id.s]!=0]))
        p.max[m,k] <- ifelse(length(ds[,id.s][ds[,id.s]!=0])==0,NA,
                             max(ds[,id.s][ds[,id.s]!=0]))
        
      }
      zz<-c(zz,rep(y[m],7))
    }
    temp<-rbind(n.mean,p.mean,n.sd,p.sd,n.prate,p.min,p.max)
    colnames(temp)<-allact
    temp$stat.type<-rep(c('n.mean','p.mean','n.sd','p.sd','n.prate','p.min','p.max'),length(y))
    temp$demog<-j
    temp$where<-names(whereactivityds.day[i])
    temp$year<-zz
    
    temp.desc.stat.allwhere<-rbind(temp.desc.stat.allwhere,temp)
    
    #saving
    
  }
}

desc.stat.allwhere<-temp.desc.stat.allwhere
save.image()

desc.stat.allwhere<-melt(desc.stat.allwhere,id.vars=c("year","where","stat.type","demog"))

#similar set of calculation to calculate mean using linear model 
temp.ds<-desc.stat.allwhere[desc.stat.allwhere$stat.type %in% c('p.mean','n.mean','n.prate'),]
temp.ds$value[is.na(temp.ds$value)]<-0
#temp.ds<-temp.ds[-which(is.na(temp.ds$value)),] # check this........ 

# linmod <- function(df) 
# {
#   summary(lm(value ~ year, data = mutate(df, year = year - min(year))))
# }

allwhere.summary.lm<-dlply(temp.ds,.(where,demog,stat.type,variable),linmod) 

allwhere.coef.lm<-ldply(allwhere.summary.lm,coef)

#finding id to match the stats 
a<-as.numeric(allwhere.coef.lm$variable)
seq<-rle(a)
cumseq.lengths<-cumsum(seq$lengths)
allwhere.coef.lm$coef<-0
allwhere.coef.lm$coef[cumseq.lengths[seq$lengths==1]]<-1
allwhere.coef.lm<-allwhere.coef.lm[allwhere.coef.lm$coef==0,]
allwhere.coef.lm$coef<-rep(c('Intercept','Year'),length(allwhere.coef.lm[,1])/2)

#wherenames<-read.csv('wherenames.csv')
#actnames<-read.csv('allactivitycodes_description.csv')
#allnames<-rbind(wherenames,actnames)

#allwhere.coef.lm<-merge(x=allwhere.coef.lm,y=allnames,by='variable',all.x = TRUE)
#desc.stat.allwhere<-merge(x=desc.stat.allwhere,y=allnames,by='variable',all.x = TRUE)

write.csv(desc.stat.allwhere,'desc.stat.csv')
write.csv(allwhere.coef.lm,'coef.all.csv')






