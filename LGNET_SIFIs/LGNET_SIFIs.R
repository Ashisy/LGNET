
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory setwd('C:/...')
setwd("D:/LGNET/code/LGNET-master/LGNET_SIFIs")

# read the market caplitalization of 23 firms
mc = read.csv("mc.csv")[,4]

# read the total connecteness matrix 
tot.ct = as.matrix(read.csv("total.csv")[,-1])

# there are 482 windows
windows = 482

# ranks SI
sif_in = rep(0, 23)
for (i in 1:23) {
  in_firms  = tot.ct[i, ]
  sif_in[i] = mc[i] * (sum((in_firms)/windows * mc))
}
names(sif_in) = colnames(tot.ct)
sif_in = as.data.frame(sif_in)
write.csv(sif_in, file = "sif_in.csv")

