
#CODE FOR MAJACT

load("~/.RData")

for (a in 1:6)
{
  if (a==1) 
  {
    filename<-c("allact_trends_all.xlsx")
    melt_act<-melt_allact
    
  } 
  
  else if (a==2)
  {
    filename<-c("allact_trends_emp_present.xlsx")
    melt_act<-melt_allact_emp_present
    
  } 
  
  else if(a==3)
  {
    filename<-c('allact_trends_emp_absent.xlsx')
    melt_act<-melt_allact_emp_absent
  }
  
  else if(a==4)
  {
    filename<-c('allact_trends_unemp_onlayoff.xlsx')
    melt_act<-melt_allact_unemp_layoff
  }  
  
  else if(a==5)
  {
    filename<-c('allact_trends_unemp_looking.xlsx')
    melt_act<-melt_allact_unemp_looking
    
  }  
  
  else
  {
    filename<-c('allact_trends_notinlaborforce.xlsx')
    melt_act<-melt_allact_notinlaborforce
  }  



#summarizing the results 

#ddply for all year trends 
substats_allyear<-ddply(melt_act, c("variable"), summarise,
                        description=unique(description),
                        mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                        sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                        participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                        numb_na=sum(is.na(value))
)

#ddply for per year results 
substats_peryear<-ddply(melt_act, c("variable","year"), summarise,
                        description=unique(description),
                        mean = round(weightedMean(value,TUFNWGTP,na.rm=TRUE),2), 
                        sd = round(weightedSd(value,TUFNWGTP,na.rm=TRUE),2),
                        participation = round(sum(TUFNWGTP[value!=0])/sum(TUFNWGTP),2),
                        numb_na=sum(is.na(value))
)

#results from per year results that can be combined with all year result 
temp_stats<-ddply(substats_peryear,c("variable"),summarize,
                  rsq_mean=round(summary(lm(mean~year))$r.squared,2),
                  slope_mean=round(summary(lm(mean~year))$coef[2,1],2),
                  intercept_mean=round(summary(lm(mean~year))$coef[1,1],2)
)

#combine 
substats_allyear<-merge(x=substats_allyear,y=temp_stats,by="variable",all.x = TRUE)


#generating the trends

atusact_time<-atusact[,c("TUCASEID","TUACTIVITY_N","TUACTDUR24","TUCUMDUR24","TRCODEP")]
atusact_time<-rename(atusact_time,c("TRCODEP"="variable")) #TO CHANGE#
atusact_time<-merge(x=atusact_time,y=all_act_names,by="variable",all.x = TRUE)
atusact_time<-merge(x=atusact_time,y=file_weight,by="TUCASEID",all.x = TRUE)
atusact_time<-atusact_time[atusact_time$TUCASEID %in% unique(melt_act$TUCASEID),]

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

count<-length(unique(melt_act$TUCASEID)) #count the number of unique case id
tot_weight<-sum(unique(melt_act$TUFNWGTP)) #total sum of the weights 
allstats_starttime<-ddply(atusact_time, c("variable"), function(df) funStart(df$startid,count))   
#allstats_stoptime<-ddply(atusact_time, c("variable"), function(df) funStart(df$stopid,count))   
allstats_pattern<-ddply(atusact_time, c("variable"), function(df) funPattern(df$startid,df$stopid,df$TUFNWGTP,tot_weight))   

#saving the files 
wb<-createWorkbook() #creating workbook

#naming the sheets 
allyear<-createSheet(wb,sheetName="allyear")
peryear<-createSheet(wb,sheetName="peryear")
pattern<-createSheet(wb,sheetName="pattern")
starttime<-createSheet(wb,sheetName="starttime")

addDataFrame(substats_allyear, allyear)
addDataFrame(substats_peryear, peryear)
addDataFrame(allstats_pattern, pattern)
addDataFrame(allstats_starttime, starttime)

saveWorkbook(wb,filename)

}
