###code to generate the necessary dataset for the regression model 

#load the atusact file
atusact<-read.table("/Users/ashok/Documents/R Projects/Paper 1/atusact_0314.dat",
                     header=TRUE,sep=",")
atusresp<-read.table("/Users/ashok/Documents/R Projects/Paper 1/atusresp_0314.dat",
                    header=TRUE,sep=",")
atussum<-read.table("/Users/ashok/Documents/R Projects/Paper 1/atussum_0314.dat",
                     header=TRUE,sep=",")



# summarize the dataset with new labels based on location 

## code for creating the new category variables 
res.code<-c(-1,1,3) #blank, home and other's home
travel.code<-c(12,13)   #(12:21,99) # all travel 
commercial.code<-c(2,4,5,6,7,8,9,10,11,30,31,32) # Non-residential location
other.code<-c(-3,-2,89) #unspecified and not coded
othertravel.code<-c(99,14:21) # other travel codes 
ds<-atusact
ds$newwhere<-NA 
ds$newwhere[ds$TEWHERE %in% res.code]<-"Residential"
ds$newwhere[ds$TEWHERE %in% travel.code]<-"Cars"
ds$newwhere[ds$TEWHERE %in% commercial.code]<-"Non_Residential"
ds$newwhere[ds$TEWHERE %in% other.code]<-"Other"
ds$newwhere[ds$TEWHERE %in% othertravel.code]<-"Other_Travel_Modes"

##Aggregating the dataset based on the newwhere
newds<-aggregate(TUACTDUR24~TUCASEID+newwhere,
                    data=ds,FUN=sum)
newds<-newds[order(newds$TUCASEID),]
rownames(newds)<-NULL 

require(reshape2)
ds.cast<-dcast(newds,TUCASEID~newwhere,value.var = "TUACTDUR24")
ds.cast[is.na(ds.cast)]<-0


# Add necessary regression variables to the dataset 

merge.vars<-c("TUFNWGTP","TUYEAR","TELFS","TRDPFTPT","TEIO1COW",
              "TEIO1ICD","TEIO1OCD","TRERNHLY","TRERNWA","TRHHCHILD",
              "TRHOLIDAY","TRNUMHOU","TRSPPRES")

summ.vars<-c("TESEX","TEAGE")

finalds<-merge(x=ds.cast,y=atusresp[,c("TUCASEID",merge.vars)],
               by.x=c("TUCASEID"),all.x=TRUE)

finalds<-merge(x=finalds,y=atussum[,c("TUCASEID",summ.vars)],
               by.x = "TUCASEID",all.x = TRUE)
