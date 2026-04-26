a<-read.csv("invert.csv",skip=1)
str(a)
a$SampleID<-paste0(a$Site,a$Sample.Type,a$Date)
a<-a[,c(1:59,96)]
a<-a[,c(1,2,3,5:10,17,18,20,24:26,26,36:42,44:60)]

#mutate(a,species=old variable,
count=data)