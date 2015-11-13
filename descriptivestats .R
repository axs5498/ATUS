#all the basic stats about ATUS 

#load atussumm
atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")
atusact<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusact_0314.dat",header=TRUE,sep=",")
atusresp<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusresp_0314.dat",header=TRUE,sep=",")
atuscps<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atuscps_0314.dat",header=TRUE,sep=",")

atuscps_1<-atuscps[atuscps$TUCASEID %in% atussumm$TUCASEID,]
atuscps_1<-subset(atuscps_1,TULINENO==1)
atuscps_1<-merge(x=atuscps_1,y=atussumm[,c('TUCASEID','TUFNWGTP')],by='TUCASEID',all.x = TRUE)

setwd("~/Dropbox/Results/allstats")
#table of employment stats 
empsumm<-data.frame(table(atussumm$TELFS))
empsumm$perc<-empsumm$Freq/sum(empsumm$Freq)
for (i in 1:length(empsumm$Var1))
{
  empsumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TELFS==empsumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}
empsumm$names<-c('Employed - at work','Employed - absent','Unemployed - on looking','Unemployed - looking','Not in labor force')

write.csv(empsumm,'empsumm.csv')



#table for number of household members 
respsumm<-data.frame(table(atusresp$TRNUMHOU))
respsumm$perc<-respsumm$Freq/sum(respsumm$Freq)
for (i in 1:length(respsumm$Var1))
{
  respsumm$percweight[i]<-sum(atusresp$TUFNWGTP[atusresp$TRNUMHOU==respsumm$Var1[i]])/sum(atusresp$TUFNWGTP)
}

#CONVERTING THE TABLE TO BIN 
respbin<-data.frame(matrix(nrow = 7, ncol = 4))
respbin[,1]<-c('1','2','3','4','5','6','7 and above')
colnames(respbin)<-c('bins','Freq','perc','percweight')

respsumm$Var1<-as.numeric(as.character(respsumm$Var1))

for (i in 2:4) 
{
  respbin[1,i]<-sum(respsumm[,i][respsumm[,1]==1]) 
  respbin[2,i]<-sum(respsumm[,i][respsumm[,1]==2]) 
  respbin[3,i]<-sum(respsumm[,i][respsumm[,1]==3])
  respbin[4,i]<-sum(respsumm[,i][respsumm[,1]==4])
  respbin[5,i]<-sum(respsumm[,i][respsumm[,1]==5])
  respbin[6,i]<-sum(respsumm[,i][respsumm[,1]==6])
  respbin[7,i]<-sum(respsumm[,i][respsumm[,1]>=7])
}

write.csv(respbin,'no_resp_hh.csv')

#TABLE FOR SEX
sexsumm<-data.frame(table(atussumm$TESEX))
sexsumm$name<-c("male","female")
sexsumm$perc<-sexsumm$Freq/sum(sexsumm$Freq)
for (i in 1:2)
{
  sexsumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TESEX==sexsumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}

write.csv(sexsumm,'sexsumm.csv')

#tabel for age 
##summarize the table 
agesumm<-data.frame(table(atussumm$TEAGE))
agesumm$perc<-agesumm$Freq/sum(agesumm$Freq)
for (i in 1:length(agesumm$Var1))
{
  agesumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TEAGE==agesumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}

##summarize the table with age bins 
agebin<-data.frame(matrix(nrow = 7, ncol = 4))
agebin[,1]<-c('15-17','18-24','25-34','35-44','45-54','55-66','65 and above')
colnames(agebin)<-c('bins','Freq','perc','percweight')

agesumm$Var1<-as.numeric(as.character(agesumm$Var1))

for (i in 2:4) 
{
  agebin[1,i]<-sum(agesumm[,i][agesumm[,1]<18]) 
  agebin[2,i]<-sum(agesumm[,i][agesumm[,1]>=18&agesumm[,1]<25]) 
  agebin[3,i]<-sum(agesumm[,i][agesumm[,1]>=25&agesumm[,1]<35])
  agebin[4,i]<-sum(agesumm[,i][agesumm[,1]>=35&agesumm[,1]<45])
  agebin[5,i]<-sum(agesumm[,i][agesumm[,1]>=45&agesumm[,1]<55])
  agebin[6,i]<-sum(agesumm[,i][agesumm[,1]>=55&agesumm[,1]<65])
  agebin[7,i]<-sum(agesumm[,i][agesumm[,1]>=65])
  }

write.csv(agebin,'agebin.csv')

#income of the respondent 
incsumm<-data.frame(table(atussumm$TRERNWA))
incsumm$perc<-incsumm$Freq/sum(incsumm$Freq)
for (i in 1:length(incsumm$Var1))
{
  incsumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TRERNWA==incsumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}

##summarize the table with age bins 
incbin<-data.frame(matrix(nrow = 10, ncol = 4))
incbin[,1]<-c('<15k','15k-25k','25k-35k','35k-50k','50k-75k','75k-100k','100k-150k','150k-200k','>=200k','NA')
colnames(incbin)<-c('bins','Freq','perc','percweight')

incsumm$Var1<-as.numeric(as.character(incsumm$Var1))
incsumm$Var1<-incsumm$Var1*52/100

for (i in 2:4) 
{
  incbin[1,i]<-sum(incsumm[,i][incsumm[,1]>=0&incsumm[,1]<15000]) 
  incbin[2,i]<-sum(incsumm[,i][incsumm[,1]>=15000&incsumm[,1]<25000]) 
  incbin[3,i]<-sum(incsumm[,i][incsumm[,1]>=25000&incsumm[,1]<35000])
  incbin[4,i]<-sum(incsumm[,i][incsumm[,1]>=35000&incsumm[,1]<50000])
  incbin[5,i]<-sum(incsumm[,i][incsumm[,1]>=50000&incsumm[,1]<75000])
  incbin[6,i]<-sum(incsumm[,i][incsumm[,1]>=75000&incsumm[,1]<100000])
  incbin[7,i]<-sum(incsumm[,i][incsumm[,1]>=100000&incsumm[,1]<150000])
  incbin[8,i]<-sum(incsumm[,i][incsumm[,1]>=150000&incsumm[,1]<200000])
  incbin[9,i]<-sum(incsumm[,i][incsumm[,1]>=200000])
  incbin[10,i]<-sum(incsumm[,i][incsumm[,1]<0])
}

write.csv(incbin,'resp_incbin.csv')

# #household income 
# #income of the respondent 
# hhincsumm<-data.frame(table(atuscps_1$HEFAMINC))
# hhincsumm$perc<-hhincsumm$Freq/sum(incsumm$Freq)
# for (i in 1:length(hhincsumm$Var1))
# {
#   hhincsumm$percweight[i]<-sum(atuscps_new$TUFNWGTP[atuscps_new$HUFAMINC==hhincsumm$Var1[i]])/sum(atuscps_new$TUFNWGTP)
# }
# 
# hhincsumm$names<-c('<5k','')



#table for region - CENSUS DIVISION 


#table for state 
fipssumm<-data.frame(table(atuscps_1$GESTFIPS))
fipssumm$perc<-fipssumm$Freq/sum(fipssumm$Freq)
for (i in 1:length(fipssumm$Var1))
{
  fipssumm$percweight[i]<-sum(atuscps_1$TUFNWGTP[atuscps_1$GESTFIPS==fipssumm$Var1[i]])/sum(atuscps_1$TUFNWGTP)
}

#fips code to census division and region 

geocodes<-read.csv('/Users/ashok/Dropbox/Results/stategeocodes.csv')
division<-read.csv('/Users/ashok/Dropbox/Results/divisionnames.csv')
geocodes<-merge(x=geocodes,y=division,by='Division',all.x = TRUE)
fipssumm<-merge(x=fipssumm,y=geocodes,by='Var1',all.x = TRUE)

write.csv(fipssumm,'fipssumm.csv')

#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
#travel based descriptive statistics 
setwd("~/Dropbox/Results/travelstats")

#frequency of travel and % of population 
travelactlist<-allcodes[allcodes>=18000&allcodes<=18999]
ytravel<-data.frame(matrix(nrow = length(file_allact$TUCASEID),ncol = 2))
colnames(ytravel)<-c('TUCASEID','Freq')
ytravel[,1]<-file_allact$TUCASEID
ytravel[,2]<-rowSums(file_allact[,travelactlist]>0)
ytravel_subset<-subset(ytravel,Freq!=0)

atussumm<-atussumm[atussumm$TUCASEID %in% ytravel_subset$TUCASEID,]
atusresp<-atusresp[atusresp$TUCASEID %in% ytravel_subset$TUCASEID,]
atusact<-atusact[atusact$TUCASEID %in% ytravel_subset$TUCASEID,]
atuscps_1<-atuscps_1[atuscps_1$TUCASEID %in% ytravel_subset$TUCASEID,]


#table of employment stats 
empsumm<-data.frame(table(atussumm$TELFS))
empsumm$perc<-empsumm$Freq/sum(empsumm$Freq)
for (i in 1:length(empsumm$Var1))
{
  empsumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TELFS==empsumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}
empsumm$names<-c('Employed - at work','Employed - absent','Unemployed - on looking','Unemployed - looking','Not in labor force')

write.csv(empsumm,'empsumm.csv')



#table for number of household members 
respsumm<-data.frame(table(atusresp$TRNUMHOU))
respsumm$perc<-respsumm$Freq/sum(respsumm$Freq)
for (i in 1:length(respsumm$Var1))
{
  respsumm$percweight[i]<-sum(atusresp$TUFNWGTP[atusresp$TRNUMHOU==respsumm$Var1[i]])/sum(atusresp$TUFNWGTP)
}

#CONVERTING THE TABLE TO BIN 
respbin<-data.frame(matrix(nrow = 7, ncol = 4))
respbin[,1]<-c('1','2','3','4','5','6','7 and above')
colnames(respbin)<-c('bins','Freq','perc','percweight')

respsumm$Var1<-as.numeric(as.character(respsumm$Var1))

for (i in 2:4) 
{
  respbin[1,i]<-sum(respsumm[,i][respsumm[,1]==1]) 
  respbin[2,i]<-sum(respsumm[,i][respsumm[,1]==2]) 
  respbin[3,i]<-sum(respsumm[,i][respsumm[,1]==3])
  respbin[4,i]<-sum(respsumm[,i][respsumm[,1]==4])
  respbin[5,i]<-sum(respsumm[,i][respsumm[,1]==5])
  respbin[6,i]<-sum(respsumm[,i][respsumm[,1]==6])
  respbin[7,i]<-sum(respsumm[,i][respsumm[,1]>=7])
}

write.csv(respbin,'no_resp_hh.csv')

#TABLE FOR SEX
sexsumm<-data.frame(table(atussumm$TESEX))
sexsumm$name<-c("male","female")
sexsumm$perc<-sexsumm$Freq/sum(sexsumm$Freq)
for (i in 1:2)
{
  sexsumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TESEX==sexsumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}

write.csv(sexsumm,'sexsumm.csv')

#tabel for age 
##summarize the table 
agesumm<-data.frame(table(atussumm$TEAGE))
agesumm$perc<-agesumm$Freq/sum(agesumm$Freq)
for (i in 1:length(agesumm$Var1))
{
  agesumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TEAGE==agesumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}

##summarize the table with age bins 
agebin<-data.frame(matrix(nrow = 7, ncol = 4))
agebin[,1]<-c('15-17','18-24','25-34','35-44','45-54','55-66','65 and above')
colnames(agebin)<-c('bins','Freq','perc','percweight')

agesumm$Var1<-as.numeric(as.character(agesumm$Var1))

for (i in 2:4) 
{
  agebin[1,i]<-sum(agesumm[,i][agesumm[,1]<18]) 
  agebin[2,i]<-sum(agesumm[,i][agesumm[,1]>=18&agesumm[,1]<25]) 
  agebin[3,i]<-sum(agesumm[,i][agesumm[,1]>=25&agesumm[,1]<35])
  agebin[4,i]<-sum(agesumm[,i][agesumm[,1]>=35&agesumm[,1]<45])
  agebin[5,i]<-sum(agesumm[,i][agesumm[,1]>=45&agesumm[,1]<55])
  agebin[6,i]<-sum(agesumm[,i][agesumm[,1]>=55&agesumm[,1]<65])
  agebin[7,i]<-sum(agesumm[,i][agesumm[,1]>=65])
}

write.csv(agebin,'agebin.csv')

#income of the respondent 
incsumm<-data.frame(table(atussumm$TRERNWA))
incsumm$perc<-incsumm$Freq/sum(incsumm$Freq)
for (i in 1:length(incsumm$Var1))
{
  incsumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TRERNWA==incsumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}

##summarize the table with age bins 
incbin<-data.frame(matrix(nrow = 10, ncol = 4))
incbin[,1]<-c('<15k','15k-25k','25k-35k','35k-50k','50k-75k','75k-100k','100k-150k','150k-200k','>=200k','NA')
colnames(incbin)<-c('bins','Freq','perc','percweight')

incsumm$Var1<-as.numeric(as.character(incsumm$Var1))
incsumm$Var1<-incsumm$Var1*52/100

for (i in 2:4) 
{
  incbin[1,i]<-sum(incsumm[,i][incsumm[,1]>=0&incsumm[,1]<15000]) 
  incbin[2,i]<-sum(incsumm[,i][incsumm[,1]>=15000&incsumm[,1]<25000]) 
  incbin[3,i]<-sum(incsumm[,i][incsumm[,1]>=25000&incsumm[,1]<35000])
  incbin[4,i]<-sum(incsumm[,i][incsumm[,1]>=35000&incsumm[,1]<50000])
  incbin[5,i]<-sum(incsumm[,i][incsumm[,1]>=50000&incsumm[,1]<75000])
  incbin[6,i]<-sum(incsumm[,i][incsumm[,1]>=75000&incsumm[,1]<100000])
  incbin[7,i]<-sum(incsumm[,i][incsumm[,1]>=100000&incsumm[,1]<150000])
  incbin[8,i]<-sum(incsumm[,i][incsumm[,1]>=150000&incsumm[,1]<200000])
  incbin[9,i]<-sum(incsumm[,i][incsumm[,1]>=200000])
  incbin[10,i]<-sum(incsumm[,i][incsumm[,1]<0])
}

write.csv(incbin,'resp_incbin.csv')

# #household income 
# #income of the respondent 
# hhincsumm<-data.frame(table(atuscps_1$HEFAMINC))
# hhincsumm$perc<-hhincsumm$Freq/sum(incsumm$Freq)
# for (i in 1:length(hhincsumm$Var1))
# {
#   hhincsumm$percweight[i]<-sum(atuscps_new$TUFNWGTP[atuscps_new$HUFAMINC==hhincsumm$Var1[i]])/sum(atuscps_new$TUFNWGTP)
# }
# 
# hhincsumm$names<-c('<5k','')



#table for region - CENSUS DIVISION 


#table for state 
fipssumm<-data.frame(table(atuscps_1$GESTFIPS))
fipssumm$perc<-fipssumm$Freq/sum(fipssumm$Freq)
for (i in 1:length(fipssumm$Var1))
{
  fipssumm$percweight[i]<-sum(atuscps_1$TUFNWGTP[atuscps_1$GESTFIPS==fipssumm$Var1[i]])/sum(atuscps_1$TUFNWGTP)
}

#fips code to census division and region 

geocodes<-read.csv('/Users/ashok/Dropbox/Results/stategeocodes.csv')
division<-read.csv('/Users/ashok/Dropbox/Results/divisionnames.csv')
geocodes<-merge(x=geocodes,y=division,by='Division',all.x = TRUE)
fipssumm<-merge(x=fipssumm,y=geocodes,by='Var1',all.x = TRUE)

write.csv(fipssumm,'fipssumm.csv')




#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
#non travel based descriptive statistics 
setwd("~/Dropbox/Results/nottravelstats")

#frequency of travel and % of population 
atussumm<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atussum_0314.dat",header=TRUE,sep=",")
atusact<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusact_0314.dat",header=TRUE,sep=",")
atusresp<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atusresp_0314.dat",header=TRUE,sep=",")
atuscps<-read.table("~/Dropbox/Ongoing_Work/Time_Use/Inputs/atuscps_0314.dat",header=TRUE,sep=",")

atuscps_1<-atuscps[atuscps$TUCASEID %in% atussumm$TUCASEID,]
atuscps_1<-subset(atuscps_1,TULINENO==1)
atuscps_1<-merge(x=atuscps_1,y=atussumm[,c('TUCASEID','TUFNWGTP')],by='TUCASEID',all.x = TRUE)

atussumm<-atussumm[!atussumm$TUCASEID %in% ytravel_subset$TUCASEID,]
atusresp<-atusresp[!atusresp$TUCASEID %in% ytravel_subset$TUCASEID,]
atusact<-atusact[!atusact$TUCASEID %in% ytravel_subset$TUCASEID,]
atuscps_1<-atuscps_1[!atuscps_1$TUCASEID %in% ytravel_subset$TUCASEID,]


#table of employment stats 
empsumm<-data.frame(table(atussumm$TELFS))
empsumm$perc<-empsumm$Freq/sum(empsumm$Freq)
for (i in 1:length(empsumm$Var1))
{
  empsumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TELFS==empsumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}
empsumm$names<-c('Employed - at work','Employed - absent','Unemployed - on looking','Unemployed - looking','Not in labor force')

write.csv(empsumm,'empsumm.csv')



#table for number of household members 
respsumm<-data.frame(table(atusresp$TRNUMHOU))
respsumm$perc<-respsumm$Freq/sum(respsumm$Freq)
for (i in 1:length(respsumm$Var1))
{
  respsumm$percweight[i]<-sum(atusresp$TUFNWGTP[atusresp$TRNUMHOU==respsumm$Var1[i]])/sum(atusresp$TUFNWGTP)
}

#CONVERTING THE TABLE TO BIN 
respbin<-data.frame(matrix(nrow = 7, ncol = 4))
respbin[,1]<-c('1','2','3','4','5','6','7 and above')
colnames(respbin)<-c('bins','Freq','perc','percweight')

respsumm$Var1<-as.numeric(as.character(respsumm$Var1))

for (i in 2:4) 
{
  respbin[1,i]<-sum(respsumm[,i][respsumm[,1]==1]) 
  respbin[2,i]<-sum(respsumm[,i][respsumm[,1]==2]) 
  respbin[3,i]<-sum(respsumm[,i][respsumm[,1]==3])
  respbin[4,i]<-sum(respsumm[,i][respsumm[,1]==4])
  respbin[5,i]<-sum(respsumm[,i][respsumm[,1]==5])
  respbin[6,i]<-sum(respsumm[,i][respsumm[,1]==6])
  respbin[7,i]<-sum(respsumm[,i][respsumm[,1]>=7])
}

write.csv(respbin,'no_resp_hh.csv')

#TABLE FOR SEX
sexsumm<-data.frame(table(atussumm$TESEX))
sexsumm$name<-c("male","female")
sexsumm$perc<-sexsumm$Freq/sum(sexsumm$Freq)
for (i in 1:2)
{
  sexsumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TESEX==sexsumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}

write.csv(sexsumm,'sexsumm.csv')

#tabel for age 
##summarize the table 
agesumm<-data.frame(table(atussumm$TEAGE))
agesumm$perc<-agesumm$Freq/sum(agesumm$Freq)
for (i in 1:length(agesumm$Var1))
{
  agesumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TEAGE==agesumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}

##summarize the table with age bins 
agebin<-data.frame(matrix(nrow = 7, ncol = 4))
agebin[,1]<-c('15-17','18-24','25-34','35-44','45-54','55-66','65 and above')
colnames(agebin)<-c('bins','Freq','perc','percweight')

agesumm$Var1<-as.numeric(as.character(agesumm$Var1))

for (i in 2:4) 
{
  agebin[1,i]<-sum(agesumm[,i][agesumm[,1]<18]) 
  agebin[2,i]<-sum(agesumm[,i][agesumm[,1]>=18&agesumm[,1]<25]) 
  agebin[3,i]<-sum(agesumm[,i][agesumm[,1]>=25&agesumm[,1]<35])
  agebin[4,i]<-sum(agesumm[,i][agesumm[,1]>=35&agesumm[,1]<45])
  agebin[5,i]<-sum(agesumm[,i][agesumm[,1]>=45&agesumm[,1]<55])
  agebin[6,i]<-sum(agesumm[,i][agesumm[,1]>=55&agesumm[,1]<65])
  agebin[7,i]<-sum(agesumm[,i][agesumm[,1]>=65])
}

write.csv(agebin,'agebin.csv')

#income of the respondent 
incsumm<-data.frame(table(atussumm$TRERNWA))
incsumm$perc<-incsumm$Freq/sum(incsumm$Freq)
for (i in 1:length(incsumm$Var1))
{
  incsumm$percweight[i]<-sum(atussumm$TUFNWGTP[atussumm$TRERNWA==incsumm$Var1[i]])/sum(atussumm$TUFNWGTP)
}

##summarize the table with age bins 
incbin<-data.frame(matrix(nrow = 10, ncol = 4))
incbin[,1]<-c('<15k','15k-25k','25k-35k','35k-50k','50k-75k','75k-100k','100k-150k','150k-200k','>=200k','NA')
colnames(incbin)<-c('bins','Freq','perc','percweight')

incsumm$Var1<-as.numeric(as.character(incsumm$Var1))
incsumm$Var1<-incsumm$Var1*52/100

for (i in 2:4) 
{
  incbin[1,i]<-sum(incsumm[,i][incsumm[,1]>=0&incsumm[,1]<15000]) 
  incbin[2,i]<-sum(incsumm[,i][incsumm[,1]>=15000&incsumm[,1]<25000]) 
  incbin[3,i]<-sum(incsumm[,i][incsumm[,1]>=25000&incsumm[,1]<35000])
  incbin[4,i]<-sum(incsumm[,i][incsumm[,1]>=35000&incsumm[,1]<50000])
  incbin[5,i]<-sum(incsumm[,i][incsumm[,1]>=50000&incsumm[,1]<75000])
  incbin[6,i]<-sum(incsumm[,i][incsumm[,1]>=75000&incsumm[,1]<100000])
  incbin[7,i]<-sum(incsumm[,i][incsumm[,1]>=100000&incsumm[,1]<150000])
  incbin[8,i]<-sum(incsumm[,i][incsumm[,1]>=150000&incsumm[,1]<200000])
  incbin[9,i]<-sum(incsumm[,i][incsumm[,1]>=200000])
  incbin[10,i]<-sum(incsumm[,i][incsumm[,1]<0])
}

write.csv(incbin,'resp_incbin.csv')

# #household income 
# #income of the respondent 
# hhincsumm<-data.frame(table(atuscps_1$HEFAMINC))
# hhincsumm$perc<-hhincsumm$Freq/sum(incsumm$Freq)
# for (i in 1:length(hhincsumm$Var1))
# {
#   hhincsumm$percweight[i]<-sum(atuscps_new$TUFNWGTP[atuscps_new$HUFAMINC==hhincsumm$Var1[i]])/sum(atuscps_new$TUFNWGTP)
# }
# 
# hhincsumm$names<-c('<5k','')



#table for region - CENSUS DIVISION 


#table for state 
fipssumm<-data.frame(table(atuscps_1$GESTFIPS))
fipssumm$perc<-fipssumm$Freq/sum(fipssumm$Freq)
for (i in 1:length(fipssumm$Var1))
{
  fipssumm$percweight[i]<-sum(atuscps_1$TUFNWGTP[atuscps_1$GESTFIPS==fipssumm$Var1[i]])/sum(atuscps_1$TUFNWGTP)
}

#fips code to census division and region 

geocodes<-read.csv('/Users/ashok/Dropbox/Results/stategeocodes.csv')
division<-read.csv('/Users/ashok/Dropbox/Results/divisionnames.csv')
geocodes<-merge(x=geocodes,y=division,by='Division',all.x = TRUE)
fipssumm<-merge(x=fipssumm,y=geocodes,by='Var1',all.x = TRUE)

write.csv(fipssumm,'fipssumm.csv')



