# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# set the working directory setwd('C:/...')
setwd("D:/LGNET/code/LGNET-master/LGNET_group_network")

# Individuals
# read firms' data
f = read.csv("name.csv",header = F)
# read the connectedness matrix 
# 2020-01-07 as an example
con      = as.matrix(read.csv("p425.csv")[, -1])

# read the firms' names
names.fi = f[,1]

groups   = list(1:23)

col      = c(rep("blue", 23))

# plot a network based on the adjacency matrix 'con' with threshold = mean,

svg("p425.svg",width = 7.8 , height = 5.3 ,family="GB1")
plot_g = qgraph(con, minimum = mean(con),groups = groups, layout = "groups",  
  shape = "circle", labels = names.fi,  
  maximum = max(con), color = "white",  label.color = "darkgray", 
  edge.color = "deepskyblue2",  border.color = "black")
text(x = -0.9, y = 1, labels = "2020-01-07", xpd = NA, cex = 1.5)
dev.off()




# Groups 

# divide the firms into five groups: 
groups   = list(1:5, 6:10, 11:15, 16:18,19:23)

col      = c(rep("red", 5), rep("blue", 5), rep("green4", 5), 
             rep("deepskyblue2", 3),rep("purple3", 5))

# plot a network based on the adjacency matrix 'con' with threshold = mean, 

svg("p425group.svg",width = 25 , height = 25 ,family="GB1")
plot_g = qgraph(con, minimum = 0, legend = F , groups = groups, layout = "groups", layoutScale = c(1.2, 1.2), 
                 shape = "circle", labels = names.fi, 
                maximum = max(con), color = rep("white", 5), label.color = col, 
                edge.color = col, curve = 1, border.width = 1.2, border.color = col, asize = 2.5)
text(x = -0.9, y = 1, labels = "2020-01-07", xpd = NA, cex = 5)
legend(1, 1,legend= c("金融","医药", "消费", "科技","基建"),box.lwd = 0.1, box.col = "white", bg = "white",
           text.col = c("red", "blue", "green4", "deepskyblue2","purple3"), cex = 3)
dev.off()



## heat map
library(corrplot)

svg("pc425.svg",width = 25 , height = 25 ,family="GB1")
cor= as.matrix(read.csv("pc425.csv")[,-1])
rownames(cor) <- names.fi
corrplot(cor, method = "shade", shade.col = NA, tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", cl.pos = "n")
dev.off()

svg("pc427.svg",width = 25 , height = 25 ,family="GB1")
cor= as.matrix(read.csv("pc427.csv")[,-1])
colnames(cor) <- names.fi
rownames(cor) <- names.fi
corrplot(cor, method = "shade", shade.col = NA, tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", cl.pos = "n")
dev.off()
