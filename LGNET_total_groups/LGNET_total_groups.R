
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
setwd("D:/LGNET/code/LGNET-master/LGNET_total_groups")

# read the file of group connectedness 
dt <- seq.Date(from = as.Date("2012-01-07",format = "%Y-%m-%d"), 
               by = "week", length.out = 482)
load("co.RData")

g1 = as.numeric(read.csv("group1.csv", header = TRUE)[,2])
g1 = ((g1 - min(g1))/(max(g1) - min(g1)))
g2 = as.numeric(read.csv("group2.csv", header = TRUE)[,3])
g2 = ((g2 - min(g2))/(max(g2) - min(g2)))
g3 = as.numeric(read.csv("group3.csv", header = TRUE)[,4])
g3 = ((g3 - min(g3))/(max(g3) - min(g3)))
g4 = as.numeric(read.csv("group4.csv", header = TRUE)[,5])
g4 = ((g4 - min(g4))/(max(g4) - min(g4)))
g5 = as.numeric(read.csv("group5.csv", header = TRUE)[,6])
g5 = ((g5 - min(g5))/(max(g5) - min(g5)))



# groupin
svg("groupin.svg",width = 16 , height = 8 ,family="GB1")
plot(dt, g1, type = "l", col = "white", lwd = 5, ylab = "", xlab = "", cex.axis = 1.8, 
  font.axis = 1.8, ylim = c(0.4,0.8))
lines(smooth.spline(dt, g1, spar = 0.5), lwd = 5, col = "red",lty = 5)

lines(smooth.spline(dt, g2, spar = 0.5), lty = 2, col = "blue", lwd = 5)

lines(smooth.spline(dt, g3, spar = 0.5), lty = 3, col = "green4", lwd = 5)

lines(smooth.spline(dt,g4, spar = 0.5), lty = 4, col = "purple", lwd = 5)
lines(smooth.spline(dt,g5, spar = 0.5), lty = 1, col = "black", lwd = 5)
legend("bottomright", inset=0, legend=c("金融","医药","消费","科技","基建"),
       col=c("red","blue","green4","purple","black"),lty=c(5,2,3,4,1),lwd=c(2,2,2,2,2),box.lty=1)
dev.off()

# group out 
svg("groupout.svg",width = 16 , height = 8 ,family="GB1")
plot(dt, g1, type = "l", col = "white", lwd = 5, ylab = "", xlab = "", cex.axis = 1.8, 
     font.axis = 1.8, ylim = c(0.2,0.8))
lines(smooth.spline(dt, g1, spar = 0.5), lwd = 5, col = "red",lty = 5)

lines(smooth.spline(dt, g3, spar = 0.5), lty = 3, col = "green4", lwd = 5)

lines(smooth.spline(dt,g4, spar = 0.5), lty = 4, col = "purple", lwd = 5)
lines(smooth.spline(dt,g5, spar = 0.5), lty = 1, col = "black", lwd = 5)
legend("bottomright", inset=0, legend=c("金融","消费","科技","基建"),
       col=c("red","green4","purple","black"),lty=c(5,3,4,1),lwd=c(2,2,2,2),box.lty=1)
dev.off()