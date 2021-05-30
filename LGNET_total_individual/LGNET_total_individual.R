
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# sets the working directory setwd('C:/...')
setwd("D:/LGNET/code/LGNET-master/LGNET_total_individual")

# read the file of individual connectedness 
load("co.RData")
data       = read.csv("logreturn.csv", header = TRUE)
date_y <- seq.Date(from = as.Date("2012-01-07",format = "%Y-%m-%d"), 
                                    by = "week", length.out = 482)
ind = as.matrix(read.csv("ind.csv", header = TRUE)[,-1])
# plot log returns of i
i         = 8
dt   = data[,i+1]/10

svg("ind2.svg",width = 16 , height = 8 ,family="GB1")
plot(date_y,dt, type = "p", col = "grey2", lwd = 8, ylab = "", xlab = "", cex.axis = 2.5, 
     font.axis = 2.5,ylim = c(-2,2))
ind8 = as.vector(ind[, i])/mean(ind[, i])
ind8 <- ((ind8 - min(ind8))/(max(ind8) - min(ind8)))
lines(date_y,ind8 , lwd = 8, col = "blue")
con_ind = rep(0,482)
for (k in 1:482) {
 con_ind[k] =  sum(con[1,8,k])
}
lines(date_y, con_ind, lwd = 8, col = "red")
con_ind2 = rep(0,482)
for (k in 1:482) {
  con_ind2[k] =  sum(con[6,8,k])
}
lines(date_y, con_ind2, lwd = 8, col = "green4")
legend("topright", inset=0, legend=c("logreturn","XYZG","ZFSW","ind"),
       col=c("grey2","red", "green4","blue"),lty=c(NA,1,1,1),pch = c(19,NA,NA,NA),lwd=c(4,4,4,4),box.lty=1)

title(main="HR") 
dev.off()


