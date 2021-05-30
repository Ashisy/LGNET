
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory setwd('C:/...')
setwd("D:/LGNET/code/LGNET-master/LGNET_fit")

library("tseries")
library("moments")
library('FinTS')
library('readxl')
cn_log=data.frame(read_excel('cnstock_data_Interpolation.xlsx',sheet='lg_return(100)'))

summ=function(X){
  name=colnames(X)
  
  sumfun=function(x){
    matrix(c(mean(x),median(x),max(x),min(x),var(x)^0.5,skewness(x),
             kurtosis(x),(jarque.bera.test(x))$statistic,(ArchTest(x)$statistic),length(x)),10,1)}
  
sum1=apply(X, 2, sumfun)
dimnames(sum1)=list(c("均值","中位数","最大值","最小值","标准差","偏度","峰度","JB检验量",'Arch检验量',"观测值"),name)

return(round(sum1,3)) #round 3 digits
}


sumcor=function(X){
  name=c("pearson", "kendall", "spearman")
  
   sumcor=matrix(rbind(c(cor(X[,1],X[,2],method = name[1]),cor(X[,1],X[,2],method = name[2]),cor(X[,1],X[,2],method = name[3])),
                 c(cor(X[,1],X[,3],method = name[1]),cor(X[,1],X[,3],method = name[2]),cor(X[,1],X[,3],method = name[3])),
                 c(cor(X[,2],X[,3],method = name[1]),cor(X[,2],X[,3],method = name[2]),cor(X[,2],X[,3],method = name[3]))),3,3)
   dimnames(sumcor)=list(c("A-B","A-H",'B-H'),name)
  return(round(sumcor,3))
}


#-------------------------------------------------------
# Plotting the yield graph
name=data.frame(read_excel('name.xlsx'))
name=name[,2]

x_axis=seq(1,nrow(cn_log),50)
x_label=2012:2021

for (i in 1:23) {
  
  svg(paste0(i,name[i],'收益率.svg'),width=510,height=435,family="GB1")
  
  par(mai=c(0.6,0.6,0.2,0.1))
  plot(cn_log[,i+1],type = 'l',lwd=2,ylab =paste0(name[i],'收益率%'),xlab = '',
       tck=-0.02,mgp=c(1.8,0.4,0),las=1,xaxt='n')
  axis(side = 1,at=x_axis,labels = x_label)
  
  dev.off()
}



