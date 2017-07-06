library(xlsx)
a <- read.csv("Wawrinka_Dolgopolov_RG2017.csv",header=F)
rownames(a) <- a[,1]

data <- list()
names <- c("Kovalik","Dolgopolov","Fognini","Monfils","Murray")
n.names <- length(names)

for (j in 1:n.names){
name <- names[j]
scrape <- read.csv(paste("Wawrinka_",name,"_RG2017.csv",sep=""),header=F)
data[[j]] <- cbind(scrape[,2],scrape[,4])
rownames(data[[j]]) <- scrape[,1]
}


####  do paired boxplots for each performance metric - during TB set, during NEXT set
####  metrics: Winner_Forced/NGames, BP total, Receive points, Winner_Forced/Unforced



metric.names <- c("Return points won (%)",
                  "BP Conversion (%)",
                  "Winners+Forced Errors per game",
                  "Net approaches per game")

n.metrics <- length(metric.names)
metrics <- array(0,c(n.names,2,n.metrics))

for (j in 1:n.names){
 # metrics[j,,1] <- data[[j]][2,] - data[[j]][10,]
#  metrics[j,,1] <- data[[j]][2,]/data[[j]][1,]
  metrics[j,,1] <- data[[j]][9,]
  metrics[j,,2] <- data[[j]][8,]/data[[j]][7,]
  metrics[j,,3] <- data[[j]][2,]/data[[j]][1,]
  metrics[j,,4] <- data[[j]][11,]/data[[j]][1,]
}

clrs <- c("red","blue","green","yellow")

pdf("Wawrinka_Stats.pdf")
par(mfrow=c(2,2),oma=c(0,0,2,0))
for(j in 1:n.metrics){
  boxplot(metrics[,2,j]-metrics[,1,j],
          main=metric.names[j],
          col=clrs[j])
  # boxplot(100*metrics[,2,j]/metrics[,1,j],
  #         main=metric.names[j],
  #         col=("red"))
}
title("Improvement in Wawrinka's performance after winning a tie-break",outer=TRUE)
dev.off()
