# Load Pacakges
library(DT)
library(ggplot2)
library(tidyverse)
library(grid)
library(knitr)
library(dplyr)
library(lubridate)
#require(devtools)
#install_github("Displayr/flipTime")
library(flipTime)
library(factoextra)
library(gridExtra)
library(fpc)
library(tidyr)
library(cluster)
library(clValid)
library(dendextend)
library(mclust)
library(ggpubr)
library(factoextra)

# Import Data
set.seed(2)
retail <-read.csv("/Users/lexie/Desktop/Y1\ S1/CSE\ 780\ Data\ Science/Assignment3/OnlineRetail.csv")
head(retail)
str(retail)
summary(retail)

# (0) Preparation
# Data Cleaning


retail <- mutate(retail, Quantity = replace(Quantity, Quantity <= 0, NA),
                 UnitPrice = replace(UnitPrice, UnitPrice <= 0, NA))

which(is.na(retail))
retail = na.omit(retail)

str(retail)

# Data Transformation
retail$InvoiceDate <- as.character(retail$InvoiceDate)

# separate date and time components of invoice date
retail$date <- sapply(retail$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
retail$time <- sapply(retail$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})
# create month, year and hour of day variables
retail$month <- sapply(retail$date, FUN = function(x) {strsplit(x, split = '[-]')[[1]][2]})
retail$year <- sapply(retail$date, FUN = function(x) {strsplit(x, split = '[-]')[[1]][3]})
retail$hourOfDay <- sapply(retail$time, FUN = function(x) {strsplit(x, split = '[:]')[[1]][1]})


# convert the variable data type

retail$InvoiceDate <- AsDateTime(retail$InvoiceDate)
retail = mutate(retail, TotalSales = Quantity*UnitPrice)
retail$dayOfWeek <- wday(retail$InvoiceDate,label = TRUE)

# convert into factors (numeric level) (suitable for categorical variables)
retail$Country <- as.factor(retail$Country)
retail$month <- as.factor(retail$month)
retail$year <- as.factor(retail$year)
levels(retail$year) <- c(2010,2011)
hourOfDay <- as.factor(retail$hourOfDay)
retail$dayOfWeek <- as.factor(retail$dayOfWeek)

### RFM analysis (Recency, Frequency, Monetary)
max_date <- max(retail$InvoiceDate, na.rm = TRUE)
retail = mutate(retail, Diff = difftime(max_date, InvoiceDate, units = "days"))
retail$Diff <- floor(retail$Diff)


RFM <- summarise(group_by(retail,CustomerID),Frequency = n(), Monetary = sum(TotalSales), Recency = min(Diff))
RFM$Recency <- as.numeric(RFM$Recency)
RFM$Monetary[is.na(RFM$Monetary)] <- 0
summary(RFM)
head(RFM)

# scaling 
RFM <- data.frame(RFM)
row.names(RFM) <- RFM$CustomerID
RFM <- RFM[,-1]
RFM_scaled <- scale(RFM) 
RFM_scaled <- data.frame(RFM_scaled)

#(1) Agglomerative or divisive hierarchical clustering
fviz_nbclust(RFM_scaled, FUN = hcut, method = "wss") + geom_vline(xintercept = 3, linetype = 2)

euclidian_dist <- dist(RFM_scaled, method = "euclidean")

# Hierarchical clustering using Complete Linkage (TRY ALL WAYS)
hc1 <- hclust(euclidian_dist, method = "single" )

hc2 <- hclust(euclidian_dist, method = "complete" )

hc3 <- hclust(euclidian_dist, method = "ward.D2" )

hc4 <- hclust(euclidian_dist, method = "average" )

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")


# function to compute coefficient
ac <- function(x) {
  agnes(RFM_scaled, method = x)$ac
}

map_dbl(m, ac)

# visualize dendrograms
hc2 <- as.dendrogram(hc2)
cd = color_branches(hc2,k = 3)
plot(cd)

hc3 <- as.dendrogram(hc3)
cd = color_branches(hc3,k = 3)
plot(cd)

# evaluate clustering

## RFM summary?



#(2) K-means clustering


# choose K
fviz_nbclust(RFM_scaled, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)
k3 <- kmeans(RFM_scaled, centers = 3, nstart = 25) 
fviz_cluster(k3, geom = "point", data = RFM_scaled, pointsize = 0.2) + ggtitle("k = 3")

# evaluate clustering

## RFM summary?

#(3) K-means or hierarchical clustering following principal analysis (PCA)


#compute principal component analysis (PCA) to reduce the data into small dimensions for visualization

# dimension reduction using PCA ??
pr.pca <- prcomp(RFM_scaled,scale = TRUE)
summary(pr.pca)

Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))]) }




CustomerID<-row.names(RFM_scaled)

plot(pr.pca$x[, 1:2], col = Cols(CustomerID), pch = 19, xlab = "Z1", ylab = "Z2")

plot(pr.pca$x[, c(1, 3)], col = Cols(CustomerID), pch = 19, xlab = "Z1", ylab = "Z3")

summary(pr.pca)
plot(pr.pca)

pve <- 100 * pr.pca$sdev^2 / sum(pr.pca$sdev^2)  
plot(pve, type = "o", ylab = "PVE",
     xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE",xlab = "Principal Component", col = "brown3")

hc.out <- hclust(dist(pr.out$x[, 1:3]))  

plot(hc.out, labels = nci.labs,main = "Hier. Clust. on First Three Score Vectors") 

hc.out <- as.dendrogram(hc.out)
cd = color_branches(hc.out,k = 3)
plot(cd)

# coordinates of individuals
# add clusters obtained using k-means
# add species from original dataset
# data inspection

#(4) Model-based clustering (Gaussian mixture models).

mod1 <- Mclust(RFM_scaled)
summary(mod1)
plot(mod1,  what = c("BIC"))

mod2 <- Mclust(RFM_scaled, 2)
summary(mod2)

plot(mod2,  what = c("classification"), main = "Mclust Classification with two components")

mod2a <- Mclust(RFM_scaled, 3)
summary(mod2a)
plot(mod2a,  what = c("classification"), main = "Mclust Classification with three components")
