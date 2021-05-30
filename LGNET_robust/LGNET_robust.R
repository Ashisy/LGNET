# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory 
setwd("D:/LGNET/code/LGNET-master/LGNET_robust")

# install and load packages
libraries = c("KernSmooth", "SparseM", "MASS")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# generate the necessory csv files for LGNET Netwrk

# number of columns in connectedness tensor
cpd = 23
# number of rows in connectedness tensor
rpd = 482

library(miscTools)



# generate the connnectedness tensor
con_std = array(0, dim = c(cpd, cpd, rpd))
for (i in 1:rpd) {

  con_std[, , i] = as.matrix(read.csv(file = paste("p_std", i, ".csv", sep = ""))[,-1])
}
save(con_std,file = "co_std.RData")

# the total connectedness
total.c = matrix(0,cpd,cpd)
for (i in 1:cpd) 
{
  for (k in 1:cpd) 
  {
    total.c[i,k] = sum(con_std[i,k,]) 
  }
}
write.csv(total.c, file = "total.c.csv") 



# robust plot 

dt <- seq.Date(from = as.Date("2012-01-07",format = "%Y-%m-%d"), 
               by = "week", length.out = 482)
load("co.RData")

total.c = rep(0,482)
for (i in 1:482) {
  total.c[i] =sum(abs(con[,,i]))
}
total.c = total.c-0.95*floor(total.c)

total.b = rep(0,482)
for (i in 1:482) {
  total.b[i] =sum(abs(con_std[,,i]))
}
total.b = total.b-0.95*floor(total.b)

svg("conrobust.svg",width = 16 , height = 8 ,family="GB1")
plot(dt, total.c, ylab = "", xlab = "", pch = 16, 
     col = "white", cex.axis = 1.8, font.axis = 1.8,ylim = c(9,11))
lines(smooth.spline(dt, total.c, spar = 0.8), lwd = 4, col = "black")
lines(smooth.spline(dt, total.b, spar = 0.8),col = "blue", lty = 2, 
      lwd = 4)
legend("topright", inset=0, legend=c("原始数据","Robust"),
       col=c("black","blue"),lty=c(1,2),lwd=c(2,2),box.lty=1)
dev.off()
