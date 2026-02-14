# © Ervin Kurbegovic - Exam Modul3 - Trautmann, Kerschke, Seiler - 24.06.2023

# Loading libaries that may be used
library(mboost)
library(FNN)
library(robustbase)
library(randomForest)
library(h2o)
library(deepnet)
library(LiblineaR)
library(SwarmSVM)
library(adabag)
library(party)
library(bst)
library(ada)
library(mlr)
library(caret) # confusion matrix
library(text)
library(lsa) # cosine
library(tidyverse)
library(ggplot2)
library(HSAUR3)
library(GGally) #install.packages("GGally")
library(tidyr) # gather function
library(vegan) # install.packages('vegan')
library(fpc)
library(dbscan)
library(factoextra) #fviz DBSCAN plot
library(gridExtra) # ggplot side by side
library(mice) # Imputation
library(dplyr)
#library(MASS)

## Loading data
data <- read.table(file = "football.csv",header = TRUE, sep = ";", dec = ",")
data = read.csv(file = , header = , sep = , dec = )
#-----------------------------------------------------------------------------#

#### Exploratry data analysis
#-----------------------------------------------------------------------------#
summary(data) 
str(data)

# Using for loop to plot boxplot on all variables/columns
for (i in 1:length(data)) {
  boxplot(data[i],main=colnames(data[i]))
  
}

for (x in colnames(data)) {
  boxplot(data[x], main=x)
  
}

# put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# pairs with histogram
pairs(data,pch=19,cex.labels=2,cex.axis=2, diag.panel=panel.hist) 

# pairs with corelation 
pairs(data,pch=19,cex.labels=2,cex.axis=2, upper.panel=panel.cor) 

# pairs with both histogram and correlation
pairs(data,pch=19,cex.labels=2,cex.axis=2, diag.panel=panel.hist, upper.panel=panel.cor)

# ggpairs as alternative to pairs
GGally::ggpairs(data) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        axis.text.y = element_text(angle = 0, hjust = 1) )
#-----------------------------------------------------------------------------#

#### Normal Distribution
## QQ-plots can be used for checking the normality assumption:
## Compare empirical and (assumed) theoretical distribution functions.

# Calculation and plot
layout(matrix(1:2, ncol=2))
x = sort(c(-1.90, 1.05, -0.62, -1.85, -1.18, -0.97, -1.06, 1.01, -0.37, 0.17))
ranks = c(1:10)
j_star = (ranks -0.5) / length(ranks)
q_nor = round(qnorm(j_star),2)  # qnorm(seq(0.05,0.95,0.1)) = j_star

plot(x=q_nor, y=x, xlab = 'Theoretical quantiles', ylab='Sample Quantiles')
qqline(x, col='red')

# Without Calculation just provide Values to the functions

# Plotting quantile-quantile (Q-Q) plot for variable x
qqnorm(x)

# Adding a reference line to the Q-Q plot
qqline(x, col='red')

# Plotting quantile-quantile (Q-Q) plot with reference line for all Variables
par(mar=c(5, 3, 2, 1))
layout(matrix(1:8, ncol=4,nrow=2))
sapply(colnames(USairpollution), function(x) {
  qqnorm(USairpollution[,x], main = x,pch=19,cex.lab=2,cex.main=2,
         ylab="")
  qqline(USairpollution[,x],lwd=2,col="red")
})

# Plotting hostogramm for all Variables
sapply(colnames(USairpollution), function(x) {
  hist(USairpollution[[x]], main = x, pch=19, cex.lab=2, cex.main=2,
       ylab="", xlab="", col='cyan', ) #breaks=8
})

### Sharpio-Wilk-Test
# first visual
layout(matrix(1:8, ncol = 4))
sapply(colnames(USairpollution), function(x) {
  hist(as.numeric(USairpollution[[x]]),main=x,xlab=" ", col="cyan",cex.main=2)
})

## than statistical testing
sapply(colnames(USairpollution), function(x){
  shapiro.test(as.numeric(USairpollution[[x]]))
})

pvalue = 0.05
sapply(colnames(USairpollution), function(x) {
  value = shapiro.test(as.numeric(USairpollution[[x]]))[2]
  
  if (value > pvalue) {
    shapiro.test(as.numeric(USairpollution[[x]]))
  }
})


# Clustering

# Excercise 2d kmeans

x <- c(2.0, 2.5, 2.5, 3.0, 3.5, 3.5, 3.5, 4.0, 4.5, 4.5, 5.0)
y <- c(3.0, 2.5, 5.0, 2.0, 2.0, 3.5, 4.0, 2.0, 2.5, 5.0, 3.0)

# Transform to Dataframe
data = data_frame('x'=x, 'y'=y)

# Scale by column (by row 1)
data_s = apply(data,2,scale)

num_clusters = 5

# Following code calculates the within-cluster sum of squares (WSS) for different numbers of clusters using 
# the k-means clustering algorithm. It then plots the WSS values to help determine the optimal number of clusters 
# based on the "elbow" criterion, where a significant decrease in the WSS indicates a good number of clusters.
#n = nrow(data_s)

wss = rep(0, num_clusters)
for (i in 1:num_clusters) {
  wss[i] = sum(kmeans(data_s, centers = i)$withinss)
}
plot(wss, type = 'b', xlab="number of cluster", ylab="Total within Sum of Squares")
#abline(h = wss, col='red')
abline(v = 3, col='red')
abline(v = 4, col='darkorange')
text(y = 18 ,x = 3.5 ,labels = 'Optimal number\n of clusters', col='red')
text(y = 17 ,x = 4.5 ,labels = 'Alternative number\n of clusters', col='darkorange')

text(adj = 2 ,x = wss ,labels = round(c(0,diff(wss)), digits = 2), col='red')

k2 = kmeans(data_s, centers = 2)
k3 = kmeans(data_s, centers = 3)

data_d = dist(data, method = 'manhattan')

ward = hclust(data_d,method="ward.D2")
average =hclust(data_d,method="average") 
plot(ward, hang=-1)
plot(average, hang=-1)

data$ward = cutree(ward, k = 2)
data$average = cutree(average, k = 2)
data$kmeans2 = k2$cluster
data$kmeans3 = k3$cluster

pairs(data[,1:2], col=data$kmeans3)
g1 = ggplot(data, aes(x=x,y=y,color=as.factor(kmeans2))) + geom_point() +
  ggtitle('Excercise 2d) kmeans-Clustering 2 Clusters')
g2 = ggplot(data, aes(x=x,y=y,color=as.factor(kmeans3))) + geom_point() +
  ggtitle('Excercise 2d) kmeans-Clustering 3- Clusters')
g3 = ggplot(data, aes(x=x,y=y,color=as.factor(ward))) + geom_point() +
  ggtitle('Excercise 2d) hclust Ward.2d')
g4 = ggplot(data, aes(x=x,y=y,color=as.factor(average))) + geom_point() +
  ggtitle('Excercise 2d) hClust Average-Linkage')

grid.arrange(g1,g2,g3,g4, ncol=2)

?dbscan()
# Partition-based Clustering: k-Means
# Example: Iris – Generate initial ‘means’

# assigning values to variable(vector)
x <- c(2.0, 2.5, 2.5, 3.0, 3.5, 3.5, 3.5, 4.0, 4.5, 4.5, 5.0)
y <- c(3.0, 2.5, 5.0, 2.0, 2.0, 3.5, 4.0, 2.0, 2.5, 5.0, 3.0)

# generating dataframe out of the two vectors
data = data.frame('X'=x,'Y' = y)

# scaling the data
data_s = apply(data,2,scale)
# checking the variance of the columns, so if the data is scaled
apply(data_s,2, var)

# Compute Manhattan distance matrix
data_s_dist_m <- dist(data_s, method = "manhattan")

# Compute euclidean distance matrix
data_s_dist_e <- dist(data_s, method = "euclidean")

# setting the max number of clusters 
num_clusters = 5

# Following code calculates the within-cluster sum of squares (WSS) for different numbers of clusters using 
# the k-means clustering algorithm. It then plots the WSS values to help determine the optimal number of clusters 
# based on the "elbow" criterion, where a significant decrease in the WSS indicates a good number of clusters.
n = nrow(data_s)

wss = rep(0, num_clusters)
for (i in 1:num_clusters) {
  wss[i] = sum(kmeans(data_s_dist_e, centers = i)$withinss)
}
plot(wss, type = 'b', xlab="number of cluster", ylab="Total within Sum of Squares")
#abline(h = wss, col='red')
abline(v = 3, col='red')
abline(v = 4, col='darkorange')
text(y = 80 ,x = 3.5 ,labels = 'Optimal number\n of clusters', col='red')
text(y = 70 ,x = 4.5 ,labels = 'Alternative number\n of clusters', col='darkorange')

text(adj = 2 ,x = wss ,labels = round(c(0,diff(wss)), digits = 2), col='red')




#K-means clustering is typically performed on raw data without transforming it to a specific distance metric.
#The algorithm calculates distances based on the variables in the dataset.
#It aims to minimize the within-cluster sum of squares (WSS) using the default Euclidean distance.
#Data with different variable types can still be effectively clustered without explicit transformations.
#Scaling or normalization may be useful for variables with different scales or units.
#The decision to apply transformations depends on the data and analysis requirements

kmeans_2 =kmeans(data_s_dist_e, centers = 2, dist.method='manhattan')
kmeans_3 =kmeans(data_s_dist_e, centers = 3)
kmeans_4 =kmeans(data_s_dist_e, centers = 4)
kmeans_5 =kmeans(data_s_dist_e, centers = 5)
data$kmeans2 = kmeans_2$cluster
data$kmeans3 = kmeans_3$cluster
data$kmeans4 = kmeans_4$cluster
data$kmeans5 = kmeans_5$cluster

pairs(data[1:2], col=data$kmeans4, pch=19, main='Kmeans 4 Clusters ')
pairs(data[1:2], col=data$kmeans3, pch=".", cex=5, main='kmeans 3 Clusters')
legend("topleft", legend = 1:2, col = c('red', 'black'), pch = 19, title = "Clusters")

# Plot using ggplot2
g2 = ggplot(data[1:4], aes(x = X, y = Y, color = as.factor(data$kmeans2))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('red', 'cyan'), name = 'kmeans Clusters') + # labels optional
  ggtitle("Exam - Modul3", subtitle = "Excercise 2d") +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        axis.title = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12)) +
  #  labs(title = 'hans')
  #  theme(panel.border = element_blank(),
  #        panel.grid = element_blank(),
  #        axis.ticks = element_blank()) +
  theme(plot.background = element_rect(fill = "white"))


g3 = ggplot(data[1:4], aes(x = X, y = Y, color = as.factor(data$kmeans3))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('red', 'cyan', 'orange'), name = 'kmeans Clusters') + # labels optional
  ggtitle("Exam - Modul3", subtitle = "Excercise 2d") +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        axis.title = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12)) +
  #  labs(title = 'hans')
  #  theme(panel.border = element_blank(),
  #        panel.grid = element_blank(),
  #        axis.ticks = element_blank()) +
  theme(plot.background = element_rect(fill = "white"))

g4 = ggplot(data[1:4], aes(x = X, y = Y, color = as.factor(data$kmeans4))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('red', 'cyan', 'orange', 'purple'), name = 'kmeans Clusters') + # labels optional
  ggtitle("Exam - Modul3", subtitle = "Excercise 2d") +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        axis.title = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12)) +
  #  labs(title = 'hans')
#  theme(panel.border = element_blank(),
#        panel.grid = element_blank(),
#        axis.ticks = element_blank()) +
  theme(plot.background = element_rect(fill = "white"))

g5 = ggplot(data[1:4], aes(x = X, y = Y, color = as.factor(data$kmeans5))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('red', 'cyan', 'orange', 'purple', 'black'), name = 'kmeans Clusters') + # labels optional
  ggtitle("Exam - Modul3", subtitle = "Excercise 2d") +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        axis.title = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12)) +
  #  labs(title = 'hans')
  #  theme(panel.border = element_blank(),
  #        panel.grid = element_blank(),
  #        axis.ticks = element_blank()) +
  theme(plot.background = element_rect(fill = "white"))


# Arrange plots side by side
grid.arrange(g2, g3, g4, g4, nrow = 2)
grid.arrange(g3,g4, ncol = 2)

kmeans_5$withinss


# Example data
x <- seq(8,1,- 1)
y <- seq(16,1, -2)#c(5, 4.9, 4.5, 1, 0.9)
#x = c(x, 2)
#y = c(y, 7)
x
y
# Calculate correlation coefficients
spearman <- cor(x, y, method = "spearman")
pearson <- cor(x, y, method = "pearson")
kendall <- cor(x, y, method = "kendall")

# Scatter plot
plot(x, y, pch = 16, xlab = "x", ylab = "y", main = "Correlation Comparison")
abline(0, 1, col = "gray", lty = 2)  # Reference line

# Text annotations
text(4, 5.5, paste("Spearman:", round(spearman, 2)), adj = 0)
text(4, 5, paste("Pearson:", round(pearson, 2)), adj = 0)
text(4, 4.5, paste("Kendall:", round(kendall, 2)), adj = 0)

# Legend
legend("topright", legend = c("Spearman", "Pearson", "Kendall"), pch = 16, col = "black")


### Benchmarking
#Task
data = df.veh
data = iris

data
sample(x = data, size = (2,ncol(data)), replace = FALSE)
arrange(data[sample(nrow(data),50, replace = FALSE),])

task1 = makeClassifTask(data = data, target = "Species", id = "CV")
task2 = makeClassifTask(data = data, target = "Species", id = "Bootstrap")
# Learner
lrn.rpart = makeLearner("classif.rpart", id = 'Decision Tree')
lrn.rf = makeLearner("classif.randomForest", par.vals = list(ntree = 30L), id = 'RandomForest')
lrn.svm = makeLearner("classif.ksvm", id = 'Support Vector Machines', par.vals = list(sigma=0.08))

# Resampling
rinst.cv = makeResampleInstance("CV", iters = 10, task = task1)
rinst.boot = makeResampleInstance("Bootstrap", iters = 100L, task = task2)
rinst.cv
rinst.boot
lrn.svm
#Benchmark
bench = benchmark(
  learners = list(lrn.rpart, lrn.rf, lrn.svm),
  tasks = list(task1, task2),
  resamplings = list(rinst.cv, rinst.boot),
  measures = list(mmce, acc, rmse)
)
#Result
bench
measure = acc
measure = mmce
measure = c(mmce, acc)

plotBMRBoxplots(bench, style = 'violin', measure =acc)
plotBMRBoxplots(bench, style = 'box') #Mean misclassification error (MMCE) = (Number of Misclassified Instances / Total Number of Instances)
bench$results$CV$`Decision Tree`


# Exercise 3 
# a)
set.seed(123)
# generating random 10 values that are normaly distributed where the mean is 10 and standard deviation is 2.
# than scaling them and rounding them so that there are 2 decimal places

A = round(scale(rnorm(n = 10,mean = 5,sd = 2)),2)
B = round(scale(runif(n = 10, min = 2, max = 25)), 2)
# b)
layout(matrix(1:4, ncol = 2))
plot(A)
plot(B)
plot(A,B)


# c)
A = sort(A)
B = sort(B)
# Without Calculation just provide Values
qqnorm(A)
qqline(A, col='red')
qqnorm(B)
qqline(B, col='red')

# H0: data is normal distributed / Normal distribution is present.
shapiro.test(A)
# p-value = 0.3861, So p-value > 0.05 means H0 can not be rejected.
# Result Data of A is normal distributed
hist(A) # Visual check

# H0: data is normal distributed / Normal distribution is present.
shapiro.test(B)
# p-value = 0.5968, So p-value > 0.05 means H0 can not be rejected.
# Result Data of B is normal distributed
hist(B) # Visual check

layout(matrix(1:4, ncol = 2))
boxplot(c(A,0.75))
boxplot(c(B,-2.8))
plot(cbind(c(A,0.75),c(B, -2.8)))
points(y = -2.8, x = 0.75, col='red', cex=1.7)
text(0.75,-2.8, labels = "Multivariate Outlier",cex=1.25,col="red",pos=4,offset=1)

## The Mahalanobis distance is a measure of the distance between a point and a distribution. 
plot(data.frame('A'=c(A,1.75), 'B'=c(B, -3.8)))
xx =  data.frame('A'=c(A,1.75), 'B'=c(B, -3.8))# need to be a dataframe with at least two dim
cm <- colMeans(xx)
S <-  cov(xx)
d <- apply(xx,1,function(x) t(x-cm) %*% solve(S) %*% (x-cm))
n_out = which(d > qchisq(0.995,length(xx)-1))
xx[c(n_out),]   
which(d > qchisq(0.9,length(xx)))
d
qchisq(c(0.9,0.95, 0.99), length(xx)-1)
length(xx)


# Calculate mean and standard deviation
x = c(A,15.75)
mean_x <- mean(x)
sd_x <- sd(x)

# Transform to z-values 
# As a guideline: 3.5 can be considered critical for moderate sample sizes.

z <- (x - mean_x) / sd_x
which(abs(z)>3.5) # abs = absolut value so -3 is 3
z

set.seed(42)  # Set a seed for reproducibility

n <- 20  # Number of data points

# Generate x values from a random distribution
x <- rnorm(n)

# Generate y values with desired correlation
rxy <- -0.8  # Desired correlation coefficient
y <- rxy * x + sqrt(1 - rxy^2) * rnorm(n)

# Verify the correlation coefficient
correlation <- cor(x, y)
plot(x,y)
cor.test(c(x,-2.5),c(y,-2.5), method = 'pearson')
cor.test(c(x,-2.5),c(y,-2.5), method = 'spearman')


set.seed(42)  # Set a seed for reproducibility

n <- 100  # Number of data points

# Generate x values from a random distribution
x <- rnorm(n)

# Generate y values with a linear relationship to x
y <- 2 * x + rnorm(n)

# Calculate correlation coefficients
pearson_corr <- cor(x[-18], y[-18], method = "pearson")
spearman_corr <- cor(x[-18], y[-18], method = "spearman")
pearson_corr
spearman_corr
plot(x,y)
points(x[18], y[18], cex=2.5, col='red')
x[-18]

# Excercise 6
# a)
data()
?Orange
data = Orange
data$Tree = as.numeric(data$Tree)
apply(data, 2, function(x) {hist(x, xlab = colnames(x))})
sapply(Orange, function(x) {shapiro.test(as.numeric(x))})

for (i in 1:length(data)) {
  hist(as.numeric(data[,i]), main = colnames(data[i]))
  print('-----------------------------------------')
  print(paste('Variable:',colnames(data[i])))
  print('-----------------------------------------')
  print(shapiro.test(as.numeric(data[,i])))
  
}
str(data)
summary(data)

data2 = df.veh
# b)
#Task
task1 = makeRegrTask(data = data, target = "Tree", id = "CV")
task2 = makeRegrTask(data = data, target = "Tree", id = "Bootstrap")
task3 = makeClassifTask(data = data2, target = "Class", id = "CV")
task4 = makeClassifTask(data = data2, target = "Class", id = "Bootstrap")
# Learner Regression https://mlr.mlr-org.com/articles/tutorial/integrated_learners.html
lrn.rpart = makeLearner("regr.cforest", id = 'Decision Tree')
lrn.rf = makeLearner("regr.randomForest", par.vals = list(ntree = 30L), id = 'RandomForest')
lrn.svm = makeLearner("regr.ksvm", id = 'Support Vector Machines', par.vals = list('sigma'=0.08))
# Learner Classification predict.type = "prob" or "response" 
lrn.cl_rpart = makeLearner("classif.cforest", id = 'Decision Tree')
lrn.cl_rf = makeLearner("classif.randomForest", par.vals = list(ntree = 30L), id = 'RandomForest')
lrn.cl_svm = makeLearner("classif.ksvm", id = 'Support Vector Machines', par.vals = list('sigma'=0.08))


# Resampling
rinst.cv = makeResampleInstance("CV", iters = 10, task = task1)
rinst.boot = makeResampleInstance("Bootstrap", iters = 100L, task = task2)
rinst.cl_cv = makeResampleInstance("CV", iters = 10, task = task3)
rinst.cl_boot = makeResampleInstance("Bootstrap", iters = 100L, task = task4)

# Look at learner, Resampling
lrn.cl_rpart
lrn.cl_rf
lrn.cl_svm
rinst.cl_cv
rinst.cl_boot

lrn.svm
lrn.rf
lrn.rpart
rinst.cv
rinst.boot

#Benchmark Regression
bench = benchmark(
  learners = list(lrn.rpart, lrn.rf, lrn.svm),
  tasks = list(task1, task2),
  resamplings = list(rinst.cv, rinst.boot),
  measures = list(rmse, mae) # mmce, acc, f1, tpr, tnr 
  
)
#Benchmark Classification
bench_cl = benchmark(
  learners = list(lrn.cl_rpart, lrn.cl_rf, lrn.cl_svm),
  tasks = list(task3, task4),
  resamplings = list(rinst.cl_cv, rinst.cl_boot),
  measures = list(acc, mmce)
)


#Result
bench
bench_cl
measure = mae
measure = rmse
measures = rmse
plotBMRBoxplots(bench, style = 'violin', measure =measures)
plotBMRBoxplots(bench, style = 'box') #Mean misclassification error (MMCE) = (Number of Misclassified Instances / Total Number of Instances)

plotBMRBoxplots(bench_cl, style = 'violin', measure =acc)
plotBMRBoxplots(bench_cl, style = 'box', measure =mmce) #Mean misclassification error (MMCE) = (Number of Misclassified Instances / Total Number of Instances)


#--------------mlr excercise Kerschke----------------------#
## EXERCISE 2

## 1) Importing the data:
df.veh = read.table("vehicles.csv", header = TRUE, sep = ",")

## 2) Creating a classification task using the (training) data
task = makeClassifTask(data = df.veh, target = "Class")

## 3) Create a learner of your choice
# lrn = makeLearner("classif.ksvm")
# lrn = makeLearner("classif.rpart")
lrn = makeLearner("classif.ksvm")

## Train a classification model on the task
mod = train(learner = lrn, task = task)

## 4) Use the model to predict the car types
pred = predict(object = mod, newdata = df.veh)

## 5) Assess the model's quality based on "mmce" and "acc"
performance(pred = pred, measures = list(mmce, acc))

## 6*) Create a copy of the data, which consists of only 2 of the vehicle types

## here, we will use the dplyr-package (which is part of the 'tidyverse')
df2 = dplyr::filter(df.veh, Class %in% c("opel", "saab")) 
subset(df.veh, Class %in% c('bus'))
df.veh %>% filter(!is.na(Class))

## alternative variants for creating a 'reduced' data set:
# df2 = subset(df.veh, Class %in% c("opel", "saab")) 
# df2 = df.veh[df.veh$Class %in% c("opel", "saab"), ]


## 7*) Once again, create a task
task2 = makeClassifTask(data = df2, target = "Class")

## 8*) Create a learner, which predicts class probabilities
lrn.rpart = makeLearner("classif.rpart", predict.type = "prob")

## 9*) Train a model on the reduced task
mod2 = train(learner = lrn.rpart, task = task2)

## 10*) Assess the model quality using mmce, acc, F1 score, sensitivity (tpr) and specificity (tnr)
pred2 = predict(object = mod2, newdata = df2)
performance(pred = pred2, measures = list(mmce, acc, f1, tpr, tnr))

#################################################################

## EXERCISE 3

## 1) Train a linear model on the delivery data using mlr commands

library(robustbase)
data(delivery)

task = makeRegrTask(data = delivery, target = "delTime")
lrn = makeLearner("regr.lm")

mod = train(learner = lrn, task = task)


## 2) Access the summary statistics and diagnostic plots of the
## underlying lm-command

# one can access the trained model using $learner.model;
# the resulting object can subsequently be analyzed like the
# "regular" lm-object
summary(mod$learner.model)

plot(mod$learner.model)


## 3) Look at the diagnostic plots

plot(mod$learner.model, which = 1)
# Based on the residual plots it remains questionable, whether the data
# meets the underlying model assumptions. At least, the desired horizontal
# tube around the value "0" is not very obvious.

plot(mod$learner.model, which = 2)
# the QQ plot also contradicts the model assumptions
# (esp. the normal distribution assumptions)


#################################################################


## EXERCISE 4

## 1) Importing the data
df.car = read.table("Daten/car_price.csv", header = TRUE, sep = ",")

## 2) Creating a regression task for the price of the car
task = makeRegrTask(data = df.car, target = "price")

## 3) Creating three (regression) learners
lrn.lm = makeLearner("regr.lm")
lrn.rpart = makeLearner("regr.rpart")
lrn.ksvm = makeLearner("regr.ksvm")

## 4) Perform a benchmark comparison of the models using
## a 5-fold CV based on the performance measures "MSE", 
## "RMSE", and the coefficient of determination.
rinst = makeResampleInstance("CV", iters = 5L, task = task)

b = benchmark(
  learners = list(lrn.lm, lrn.rpart, lrn.ksvm),
  tasks = task,
  resamplings = rinst,
  measures = list(mse, rmse, rsq)
)

## 5) Visualize (and subsequently interpret) the results
plotBMRBoxplots(b, measure = mse)
plotBMRBoxplots(b, measure = rmse)
plotBMRBoxplots(b, measure = rsq)
b
# Attention: The interpretations can differ, as the distribution of the data across the
# blocks of the 5-fold CV is performed randomly. Likewise, the herein considered version
# of the support vector machine (ksvm) contains a stochastic component, making its results
# also random to a certain extent.

# In my case, the boxplots can be interpreted as follows:
# In terms of R^2, the ksvm achieves the highest scores (0.81 - 0.83) and thus is to be
# prefered over the linear model (0.77 - 0.79) and the decision tree (0.67 - 0.80);
# In terms of the RMSE, the results are comparable. The RMSE values of the ksvm range
# between 2,100 and 2,700 and thus are mostly below the values of the linear model
# (2,400 to 3,000) and the decision tree (2,600 to 3,100), respectively.
# Therefore, the ksvm is (based on the model quality) to be preferred over the two
# considered competitors (linear model and decision tree).

#-------------------------------------------------#
  
  
# Hierarchical Clustering  
# matrix input
distmat <- matrix(c(0,251,176,236,974,
                    251,0,397,580,986,
                    176,397,0,260,867,
                    236,580,260,0,872,
                    974,986,867,872,0),
                  ncol=5,byrow=T)

# transformation into distance matrix
distmat_input <- as.dist(distmat)

# complete linkage clustering  
# Methods: "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
slclust <- hclust(distmat_input,method="complete")

# dendrogram
plot(slclust,hang=-1,labels=c("w","u","z","a","t"))

# final clustering 2 clusters
cutree(slclust, k=2)

# final clustering 3 clusters
cutree(slclust, k=3)