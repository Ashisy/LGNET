
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
setwd("D:/LGNET/code/LGNET-master/LGNET_total_connectedness")

# read the file of total connectedness
# generate date
dt <- seq.Date(from = as.Date("2012-01-07",format = "%Y-%m-%d"), 
                   by = "week", length.out = 482)
load("co.RData")

# scale the total connectedness and SZ
total.c = rep(0,482)
for (i in 1:482) {
  total.c[i] =sum(abs(con[,,i]))
}
total.c = ((total.c - min(total.c))/(max(total.c) - min(total.c)))-0.25

SZ = as.numeric(read.csv("result3.csv", header = TRUE)[,1])
SZ = ((SZ - min(SZ))/(max(SZ) - min(SZ)))-0.65

# the total connectedness and SZ
svg("conn1.svg",width = 16 , height = 8 ,family="GB1")
plot(dt, total.c, ylab = "", xlab = "", pch = 16, 
     col = "white", cex.axis = 1.8, font.axis = 1.8,ylim = c(-0.1,0.1))
lines(smooth.spline(dt, total.c, spar = 0.8), lwd = 4, col = "blue")
lines(smooth.spline(dt, SZ, df = 5, spar = 0.8), col = "black", lty = 2, 
      lwd = 4)
legend("topright", inset=0, legend=c("系统总连通强度","上证指数"),
       col=c("blue","black"),lty=c(1,2),lwd=c(2,2),box.lty=1)
dev.off()