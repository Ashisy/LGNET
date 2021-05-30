# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory 
setwd("D:/LGNET/code/LGNET-master/LGNET_SIM")

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
con = array(0, dim = c(cpd, cpd, rpd))
for (i in 1:rpd) {

  con[, , i] = as.matrix(read.csv(file = paste("p", i, ".csv", sep = ""))[,-1])
}
save(con,file = "co.RData")

# the total connectedness
total.c = matrix(0,cpd,cpd)
for (i in 1:cpd) 
{
  for (k in 1:cpd) 
  {
    total.c[i,k] = sum(con[i,k,]) 
  }
}
write.csv(total.c, file = "total.c.csv") 

# the individual connectedness
ind.c = matrix(0,rpd,cpd)
for (k in 1:cpd) 
{
  for (i in 1:rpd) 
  {
    ind.c[i,k] = sum(con[,k,i])
  }
}
write.csv(ind.c, file = "ind.csv")

# the group connectedness
group1.c = matrix(0,rpd,5)
for (i in 1:rpd) 
{
  group1.c[i,1] = sum(con[1:5,1:5,i])/5
  group1.c[i,2] = sum(con[1:5,6:10,i])/5
  group1.c[i,3] = sum(con[1:5,11:15,i])/5
  group1.c[i,4] = sum(con[1:5,16:18,i])/5
  group1.c[i,5] = sum(con[1:5,19:23,i])/5
}
write.csv(group1.c, file = "group1.csv")

group2.c = matrix(0,rpd,5)
for (i in 1:rpd) 
{
  group2.c[i,1] = sum(con[6:10,1:5,i])/5
  group2.c[i,2] = sum(con[6:10,6:10,i])/5
  group2.c[i,3] = sum(con[6:10,11:15,i])/5
  group2.c[i,4] = sum(con[6:10,16:18,i])/5
  group2.c[i,5] = sum(con[6:10,19:23,i])/5
}
write.csv(group2.c, file = "group2.csv")

group3.c = matrix(0,rpd,5)
for (i in 1:rpd) 
{
  group3.c[i,1] = sum(con[11:15,1:5,i])/5
  group3.c[i,2] = sum(con[11:15,6:10,i])/5
  group3.c[i,3] = sum(con[11:15,11:15,i])/5
  group3.c[i,4] = sum(con[11:15,16:18,i])/5
  group3.c[i,5] = sum(con[11:15,19:23,i])/5
}
write.csv(group3.c, file = "group3.csv")

group4.c = matrix(0,rpd,5)
for (i in 1:rpd) 
{
  group4.c[i,1] = sum(con[16:18,1:5,i])/3
  group4.c[i,2] = sum(con[16:18,6:10,i])/3
  group4.c[i,3] = sum(con[16:18,11:15,i])/3
  group4.c[i,4] = sum(con[16:18,16:18,i])/3
  group4.c[i,5] = sum(con[16:18,19:23,i])/3
}
write.csv(group4.c, file = "group4.csv")
group5.c = matrix(0,rpd,5)
for (i in 1:rpd) 
{
  group5.c[i,1] = sum(con[19:23,1:5,i])/5
  group5.c[i,2] = sum(con[19:23,6:10,i])/5
  group5.c[i,3] = sum(con[19:23,11:15,i])/5
  group5.c[i,4] = sum(con[19:23,16:18,i])/5
  group5.c[i,5] = sum(con[19:23,19:23,i])/5
}
write.csv(group5.c, file = "group5.csv")