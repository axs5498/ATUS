##working with the activityds and travelds.day datasets. 

ls<-list(activity = activityds.day,
         where = whereds.day)

desc.stat<-data.frame()
linearModel<-data.frame()
temp.desc.stat<-data.frame()


filter<-c('emp','unemp','retired','notinlaborforce','all')

necessary.colnames<-c('TUCASEID','TUFNWGTP','TELFS','TULAY','TULK','TUABSOT','TURETOT')
filter.file<-atusresp[,necessary.colnames]
filter.file$emp<-ifelse(filter.file$TELFS<3,1,0)
filter.file$unemp<-ifelse((filter.file$TELFS<5&filter.file$TELFS>2),1,0)
filter.file$retired<-ifelse(filter.file$TELFS==5&filter.file$TURETOT==1,1,0)
filter.file$notinlaborforce<-ifelse(filter.file$TELFS==5&filter.file$TURETOT!=1,1,0)
filter.file$all<-rep(1,length(filter.file$TUCASEID))


for (i in 1:2)
{
  ds1<-ls[[i]]
  
  for (j in filter)
  {
  
    #creating the new datast 
    id<-filter.file$TUCASEID[filter.file[,j]==1]
    ds<-ds1[ds1$TUCASEID %in% id,]
    
    
    
    ###code to calculate the yearly statistics 
    require(plyr)
    require(matrixStats)
    temp<-ddply(ds,.(year,variable),summarize,
                n.mean = round(weightedMean(value,weight),2),
                p.mean = round(weightedMean(value[value!=0],weight[value!=0]),2),
                n.sd = round(weightedSd(value,weight),2),
                p.sd = round(weightedSd(value[value!=0],weight[value!=0]),2),
                n.prate = round(sum(weight[value!=0])/sum(weight),2),
                p.min = ifelse(length(value[value!=0])==0,NA,
                               min(value[value!=0])),
                p.max = ifelse(length(value[value!=0])==0,NA,
                               max(value[value!=0])) 
    )
    
    require(reshape)
    temp.desc.stat<-melt(temp,id.vars=c("year","variable"))
    colnames(temp.desc.stat)<-c('year','variable','stat.type','value')
    temp.desc.stat$value<-round(temp.desc.stat$value,2)
    temp.desc.stat$data.type<-names(ls[i])
    temp.desc.stat$demog<-j
    
    #saving
    desc.stat<-rbind(desc.stat,temp.desc.stat)
    rm(temp.desc.stat)
  }
}



#similar set of calculation to calculate mean using linear model 
temp.ds<-desc.stat[desc.stat$stat.type %in% c('p.mean','n.mean','n.prate'),]
temp.ds<-temp.ds[-which(is.na(temp.ds$value)),]

linmod <- function(df) 
{
  summary(lm(value ~ year, data = mutate(df, year = year - min(year))))
}

temp.summary.lm<-dlply(temp.ds,.(data.type,demog,stat.type,variable),linmod) 

temp.coef.lm<-ldply(temp.summary.lm,coef)

#finding id to match the stats 
a<-as.numeric(temp.coef.lm$variable)
seq<-rle(a)
cumseq.lengths<-cumsum(seq$lengths)
temp.coef.lm$coef<-0
temp.coef.lm$coef[cumseq.lengths[seq$lengths==1]]<-1
temp.coef.lm<-temp.coef.lm[temp.coef.lm$coef==0,]
temp.coef.lm$coef<-rep(c('Intercept','Year'),length(temp.coef.lm[,1])/2)

wherenames<-read.csv('wherenames.csv')
actnames<-read.csv('allactivitycodes_description.csv')

allnames<-rbind(wherenames,actnames)

temp.coef.lm<-merge(x=temp.coef.lm,y=allnames,by='variable',all.x = TRUE)
desc.stat<-merge(x=desc.stat,y=allnames,by='variable',all.x = TRUE)

write.csv(desc.stat,'desc.stat.csv')
write.csv(temp.coef.lm,'coef.all.csv')





