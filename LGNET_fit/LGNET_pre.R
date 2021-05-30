
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory setwd('C:/...')
setwd("D:/LGNET/code/LGNET-master/LGNET_fit")

library("readxl")
data=data.frame(read_excel('TRD_Week.xlsx'))
date=data.frame(read_excel('date.xlsx'))

data2=matrix(NA,497,24)
data=data[,-3]
data2[,1]=date[,1]

# Get Symbol
name=c()
name[1]=data[2,1]
j=2
for(i in 2:nrow(data)){
  if(data[i,1]!=data[i-1,1]){
    name[j]=data[i,1]
    j=j+1
  }
}
name

# Data Cleaning
for (j in 1:length(name)) {

  for(i in 1:nrow(data2)){
    
    for(k in 1:nrow(data)){
      
      if(data[k,1]==name[j]&data[k,2]==data2[i,1]){
         data2[i,j+1]=data[k,3]
         break
    
    }
    
  }
}
}

name2=c("date",name)
colnames(data2)=name2
data2=apply(data2[,2:24], 2, as.numeric)
write.csv(data2,file = "stock_data.csv",row.names = F)

# Interpolation
#install.packages("DMwR", repos='https://mran.microsoft.com/snapshot/2019-02-01/')
library('DMwR')
data2=read.csv("stock_data2.csv")

data22=data2[,2:24]
data22=apply(data22, 2, as.numeric)

data33=knnImputation(data22)
data3=data.frame(data2[,1],data33)

write.csv(data3,file = "stock_data_Interpolation.csv",row.names = F)


