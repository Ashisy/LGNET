
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory setwd('C:/...')
setwd("D:/LGNET/code/LGNET-master/LGNET_fit")

library('readxl')
library('localgauss')
library('fGarch')
library('tseries')

cn_stock=data.frame(read_excel('cnstock_data_Interpolation.xlsx',sheet='lg_return(100)'))

# Standardize data to remove heteroskedasticity
gafit=function(data){
  name=colnames(data)
  resi=c()
  for (i in 1:ncol(data)) {
    
    ga=garchFit(~garch(1,1),data = data[,i],cond.dist = "std",trace = F)
    ga_res=residuals(ga,standardize=TRUE)
    resi=cbind(resi,ga_res)
    
  }
  colnames(resi)=name
  return(resi)
}

ga_cnstock=gafit(cn_stock[,-1])


# Find the LG correlation at each time point
for(t in 1:nrow(ga_cnstock)){
  
  p_matrix=matrix(NA,23,23)
  name=colnames(ga_cnstock)

for (j in 1:ncol(ga_cnstock)) {
  b1=bandwidth.nrd(ga_cnstock[,j])
  for(i in 1:ncol(ga_cnstock)){
    # Use the rule of thumb to find the band width
    b2=bandwidth.nrd(ga_cnstock[,i])
    lg=localgauss(ga_cnstock[,j],ga_cnstock[,i],
                  b1,b2,xy.mat =c((ga_cnstock[,j])[t],(ga_cnstock[,i])[t]))
    p_matrix[j,i]=lg$par.est[5]
  }
}

  colnames(p_matrix)=name
  rownames(p_matrix)=name
write.csv(p_matrix,file=paste0('p_std',t,'.csv'))
  
}








