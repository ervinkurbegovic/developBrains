#### Case Study Master Data Science (Module 3)
#Prof. Dr. Heike Trautmann, Prof. Dr. Pascal Kerschke
#Submitted by: Rohan Francis Pinto

# Load necessary libraries
library(dplyr)
library(lubridate)

# Set file paths for the two data sets
file1 <- "/Users/rohanpinto/Downloads/material/trips-2022_11.rds"
file2 <- "/Users/rohanpinto/Downloads/material/trips-2022_12.rds"

# Import the data sets
trips1 <- readRDS(file1)
trips2 <- readRDS(file2)

# Combine both data sets into a joint data set
jointData <- bind_rows(trips1, trips2)

# Augment the data set with the day of the week and month
jointData <- jointData %>%
  mutate(start_time = ymd_hms(time_start)) %>%
  mutate(day_of_week = wday(time_start, week_start = 1)) %>%
  mutate(month = month(time_start))

# View the resulting joint data set
View(jointData)

jointData = jointData[,-c(8)]

######## Data visualization

nrow(jointData)
## 1189 observations in total
colnames(jointData)
str(jointData)

summary(jointData)

# mean and standard deviation of the trip duration:
mean(jointData$duration)
sd(jointData$duration)

mean(jointData$air_distance)
sd(jointData$air_distance)

# visualize to find the corealtion and patterns among the variables
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(jointData,upper.panel = panel.cor, diag.panel = panel.hist)
library(GGally)
ggpairs(jointData)
?ggpairs
?pairs
# histograms or density plots for numerical variables
hist(jointData$duration, breaks = 30, xlab = "Duration (minutes)", main = "Distribution of Trip Durations")
hist(jointData$air_distance, breaks = 30, xlab = "Air Distance (meters)", main = "Distribution of Air Distances")
hist(jointData$time_start, breaks = 30, xlab = "Time_start (Day)", main = "Trip Start")


# bar plots
barplot(table(jointData$day_of_week), xlab = "Day of the Week", ylab = "Frequency", main = "Distribution of Trips by Day of the Week")
barplot(table(jointData$month), xlab = "Month", ylab = "Frequency", main = "Distribution of Trips by Month")

# line plots or bar plots to examine patterns over time
monthly_counts <- table(jointData$month)
plot(monthly_counts, type = "b", xlab = "Month", ylab = "Trip Count", main = "Monthly Trend of Trip Counts")

# scatter plots to explore correlations between variables
plot(jointData$duration, jointData$air_distance, xlab = "Duration (minutes)", ylab = "Air Distance (meters)", main = "Duration vs. Air Distance")
plot(jointData$duration, jointData$month, xlab = "Duration (minutes)", ylab = "Air Distance (meters)", main = "Duration vs. Air Distance")

#Identifying the missing values
which(is.na(jointData))


######### outliers detetection
##### univariate outliers
layout(matrix(1:12, ncol=6,nrow=2))
sapply(colnames(jointData), function(x){
  boxplot(as.numeric(jointData[[x]]),main=x,xlab=" ", col="cyan",cex.main=2)
})

boxplot(jointData$duration, main = "Boxplot of Duration")
u_out = which(jointData$duration > 400)
u_out
#Finding the outliers from dataset
View(jointData[c(u_out),])

#boxplot for air distance
boxplot(jointData$air_distance, main = "Boxplot of air distance")
u_out1 = which(jointData$air_distance > 10)
u_out1
View(jointData[c(u_out1),])

#####3multivariate
stiffness = jointData[,-c(2,3,8)]
str(stiffness)
cm <- colMeans(stiffness)
S <- cov(stiffness)
d <- apply(stiffness,1,function(x) t(x-cm) %*% solve(S) %*% (x-cm))

m_out = which(d > qchisq(0.999,length(stiffness)))
m_out

  # Adding a for loop for multiple points
  
colors <- c("green", "blue", "red", "orange", "purple")  

pairs(stiffness, pch = 19, panel = function(x, y, ...) {
  points(x, y, ...)
  for (i in m_out) {
    point_color <- colors[(i - 1) %% length(colors) + 1]  # Cycle through the colors
    points(x[i], y[i], cex = 3, col = point_color, lwd = 3)
    text(x[i], y[i], i, cex = 2, col = point_color, pos = 3, offset = 1)
  }
})

#.... From the above plot we can find that 53 and 908 are at the extreme points and hence can be removed from the data set2
m_outliers = jointData[c(52,908),]
m_outliers

jointData_c = jointData[-c(52,908, 491, 604),]
jointData_c
######### 4 Inyourmaterials,youwillfindanotherdataset(stations.rds)containingthe coordinates of the nextbike stations. Add the locations to the map of the region shown above.
#.......Hint: The map is a ggplot object, and consequently can be extended with ggplot commands.

library(ggplot2)
library(sf)
install.packages("sf")
?sf
## Import the geo-data (from the materials directory) that is needed to produce the map
load("map_files.RData")

## Produce a map of Potsdam (as ggplot object)
map1 = ggplot() +
  # roads and streets
  geom_sf(data = roads, color = "grey", alpha = 0.5) + 
  # railways (trains, trams, etc.)
  geom_sf(data = rail, color = "#A66A02", alpha = 0.3, linewidth = 1.0) +
  # water
  geom_sf(data = water, fill = "#009EE0", alpha = 0.6) + 
  # waterways
  geom_sf(data = waterways, color = "blue", size = 2.5, alpha = 0.2) +
  theme_bw() + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Nextbike-Stations in Potsdam",
    x = "Longitude", 
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(map1)

stations <- readRDS("stations.rds")


# Adding station locations to the map

map <- map1 +
  geom_point(data = stations, aes(x = lng, y = lat), color = "red", size = 3)

# Print the map
print(map)


######### 5. Find a meaningful grouping of the bike locations. For this purpose, try out different cluster algorithms and choose a suitable number of clusters.

library(ggplot2)
library(sf)
library(readr)
library(cluster)
library(factoextra)
install.packages("factoextra")

# Read the 'stations.rds' dataset containing the nextbike station coordinates
stations1 <- readRDS("stations.rds")

# ####### k-means clustering

#calculating the clusters using within sum squares and elbow method
fviz_nbclust(stations1[, c("lng", "lat")],kmeans,method = "wss") + 
  labs(subtitle = "Elbow method")

#comments
#In the elbow curve the sum of squared distance between each point and the centroid decreases after 3rd point.
#Most of the values can be found above point 3. so considering value of K = 3.

k <- 3  # Number of clusters to create
cluster_results <- kmeans(stations1[, c("lng", "lat")], centers = k, nstart = 25)

# Adding the cluster assignments to the stations data
stations1$cluster <- as.factor(cluster_results$cluster)

#visualizing the clustering results

fviz_cluster(cluster_results, data = stations[, c("lng", "lat")], ellipse = TRUE, geom = "point")
fviz_cluster(cluster_results, stations1[, c("lng", "lat")], ellipse = F, geom = "point")
# comments....... From the above plot we can see that the data points are assigned into three clusters. 
. 
#### trying with k = 4
k <- 4  # Number of clusters to create
cluster_results4 <- kmeans(stations1[, c("lng", "lat")], centers = k, nstart = 25)

# Adding the cluster assignments to the stations data
stations1$cluster4 <- as.factor(cluster_results4$cluster)

#visualizing the clustering results

fviz_cluster(cluster_results4, data = stations[, c("lng", "lat")], ellipse = TRUE, geom = "point")

# comments....... From the above plot we can see that the data points are assigned into four clusters. 
## However, I find 3 clusters are ideal.


############    Hierarchical clustering using complete linkage

stations_H <- readRDS("stations.rds")
stations_H = stations_H[, c("lng", "lat")]
# transformation into distance matrix
stations_H.dist = dist(stations_H)

stations_Hclust <- hclust(stations_H.dist,method="complete")
diff(stations_Hclust$height)

# Dendrogram
plot(stations_Hclust, hang=-1)
rect.hclust(stations_Hclust,k=3, border = 2:6)
# From the plot we can see a gap between two succeeding horizontal lines, which suggests 3 clusters.


stat_clusters = cutree(stations_Hclust, k=3)
pairs(stations_H, col = stat_clusters)
stations_H$cluster <- as.factor(stat_clusters)
# In the above plot the data points are divided into three clusters. 

## analysis with 4 clusters
stat_clusters4 = cutree(stations_Hclust, k=4)
stations_H$cluster4 <- as.factor(stat_clusters4)
pairs(stations_H, col = stat_clusters)
# From the plot we can see that the data points are split into 4 clusters. We can also find overlap within cluster points. 
# We can consider three clusters based on the above analysis.

##### DBscan 
## Choose MinPts=3, k = 2
library(fpc)
library(dbscan)
stations_DB <- readRDS("stations.rds")
stations_DB = stations_DB[, c("lng", "lat")]

dbscan::kNNdistplot(stations_DB, k = 2)
abline(h = 0.017, lty = 2,col="red")
# From the plot we can choose eps value as 0.024.

## apply DBSCAN algorithm

set.seed(123)
db <- fpc::dbscan(stations_DB, eps = 0.017, MinPts = 5)
db
# Plot DBSCAN results
plot(db, stations_DB, main = "DBSCAN", frame = TRUE)

fviz_cluster(db, stations_DB, stand = FALSE, ellipse = FALSE, geom = "point")
fviz_cluster(db, stations_DB, stand = FALSE, ellipse = TRUE, geom = "point")
# We can see the formation of 2 clusters. 

stations_DB$cluster = db$cluster
  
##### 6 Visualize your clusterresultsbycolor-codingthelocations(see4.)according to their cluster affiliation (see 5.). 
##...... Which cluster result seems most plausible to you?

### K means map
#with 3 clusters
map_kmeans3 <- map1 +
  geom_point(data = stations1, aes(x = lng, y = lat), color = stations1$cluster, size = 3)

# Printing the map
print(map_kmeans3)

#with 4 clusters
map_kmeans4 <- map1 +
  geom_point(data = stations1, aes(x = lng, y = lat), color = stations1$cluster4, size = 3)

# Printing the map
print(map_kmeans4)

#### Hierarchical cluster map

map_H <- map1 +
  geom_point(data = stations_H, aes(x = lng, y = lat), color = stations_H$cluster, size = 3)

# Printing the map
print(map_H)

### DBscan cluster map

map_DB <- map1 +
  geom_point(data = stations_DB, aes(x = lng, y = lat), color = stations_DB$cluster, size = 3)

# Printing the map
print(map_DB)         


#..... comments
#k means with 3 clustres has been considered

# 7. Let's assume that nextbike wants to set up a separate service station for each cluster.
#..Where should these service stations be placed in order to be accessible as well as 
#..possible from all stations of the respective cluster? Mark the recommended locations on your map as well!


# Assuming stations_DB is the data frame containing the station coordinates

# K-means clustering with 3 cluster to identify the centroids of the clusres
k <- 3  # Number of clusters to create
cluster_results <- kmeans(stations1[, c("lng", "lat")], centers = k, nstart = 25)

# Extracting the cluster centers
centroids <- cluster_results$centers

# centroid coordinates
centroids_df <- as.data.frame(centroids)
centroids_df

# Adding the centroids to the map, to identify the appropriate service centers. 
map_kmeans3c <- map_kmeans3 +
  geom_point(data = centroids_df, aes(x = lng, y = lat), color = "blue", size = 5)

# Printing the map with stations and centroids
print(map_kmeans3c)

############# 8 Intuitively, one would expect a relationship between trip duration and 
# the other variables (especially traveled distance). Examine this assumption by modeling 
# the relationship using various regression techniques. Which procedure is best suited for 
# this purpose? Interpret your result(s)!

colnames(jointData_c)

# Scatterplots of the independent variables vs. the dependent variable
plot(jointData_c$air_distance, jointData_c$duration, xlab = "Air Distance", ylab = "Duration")
plot(jointData_c$time_start, jointData_c$duration, xlab = "Start Time", ylab = "Duration")
plot(jointData_c$day_of_week, jointData_c$duration, xlab = "Day of Week", ylab = "Duration")
plot(jointData_c$month, jointData_c$duration, xlab = "Month", ylab = "Duration")

cor.test(jointData_c$duration,jointData_c$air_distance)
#since there is a certain degree of linear correlation between duration and air distance we can choose linear regression
# 0.25 is a weak positive correlation, hence its not ideal. 
f_data = jointData_c %>% filter(duration< 70)
cor.test(f_data$duration,f_data$air_distance)
# correlation of 0.547 is found
plot(f_data$air_distance, f_data$duration, xlab = "Air Distance", ylab = "Duration")

model <- lm(duration ~ air_distance, data = f_data)

# Print the model summary
summary(model)

# Assuming "joindata" is the name of your dataset and "model" is the fitted regression model

# Residual plot
plot(model, which = 1)

# Normal Q-Q plot of residuals
plot(model, which = 2)

##### Multiple regression 
# Creating a subset of variables for correlation analysis
variables <- c("duration", "air_distance", "day_of_week", "month")

c
# Calculate the correlation matrix
cor_matrix <- cor(f_data[, variables])

# Print the correlation matrix
print(cor_matrix)

model1 <- lm(duration ~ air_distance+day_of_week+month, data = f_data)

# Print the model summary
summary(model1)


###### 9. For urban planning, one of the areas of interest is the traffic between different areas of a city. 
#Consider the clusters identified in 5. as separate areas of the city and compare several classifiers that model the drop-off station 
# cluster as a function of the other metrics in a benchmark study. Interpret your result.


##... I have choosen Kmeans clustering with 3 clusters data

#... joining the trip data set with the cluster data set. 


cl_data <- jointData_c %>%
  inner_join(stations1, by = c("station_start" = "station_number")) %>%
  select(lng_start = lng, lat_start = lat, station_stop, station_start, duration, air_distance) %>%
  inner_join(stations1, by = c("station_stop" = "station_number")) %>%
  select(lng_start,lat_start, lng_stop = lng, lat_stop = lat, cluster,station_stop,station_start, duration, air_distance)


library(mlr)
library(ParamHelpers)
library(kernlab)
library(randomForest)
library(rpart)
library(kknn)
library(dplyr)
library(robustbase)
library(caTools)

# 1. Decision trees

set.seed(123)

# Calculating the number of rows for training set
train_size <- round(0.8 * nrow(cl_data))

# Randomly selecting indices for training set
train_indices <- sample(1:nrow(cl_data), train_size)

# Splitting the data
training_set <- cl_data[train_indices, ]
test_set <- cl_data[-train_indices, ]

# Convert tbl_df to data frame
training_set <- as.data.frame(training_set)
test_set <- as.data.frame(test_set)

View(training_set)
# Creating a classification task using the (training) data
cl.task = makeClassifTask(data = training_set, target = "cluster")

# creating a learners for decision tree
lrn.dc = makeLearner("classif.rpart")

#Information about the learner
lrn.dc
# learner's hyperparameters
getParamSet(lrn.dc) 

## train decision tree based on the given task
mod.dc = train(learner = lrn.dc, task = cl.task)

## access general information of the classifier
mod.dc
mod.dc$learner

# predicting the target values based on the test data set 
pred = predict(object = mod.dc, newdata = test_set)
str(pred)
# the confusion matrix is the frequency table of all pairs of
## true and predicted classes
table(pred$data)
# The above table shows the difference between predicted and actual values.
#There are no misclasification. 

## computing the misclassification error and accuracy
performance(pred = pred, measures = list(mmce, acc))

#   The mmce score is 0 and we got 100% accuracy.  

#####Random Forest

# building a model using random forests (with ntree = 100 trees)
# creating a learners for random forests (with ntree = 100 trees)
lrn.rf = makeLearner("classif.randomForest", par.vals = list(ntree = 100L))

# learner's hyperparameters
getParamSet(lrn.rf) 

# train random forests based on the given task
mod.rf = train(learner = lrn.rf, task = cl.task)

## access general information of the classifier
mod.rf
mod.rf$learner

# predicting the target values based on the test data set 
pred.rf = predict(object = mod.rf, newdata = test_set)
str(pred.rf)
# the confusion matrix is the frequency table of all pairs of
## true and predicted classes
table(pred.rf$data)
# The confusion matrix shows us that random forest give 100% accuracy.

## computing the misclassification error and accuracy
performance(pred = pred.rf, measures = list(mmce, acc))
#  The mmce score is 0 and we get 100% accuracy.  

###########   support vector machines #########

# creating a learners for random forests (with ntree = 100 trees)
lrn.svm1 = makeLearner("classif.ksvm") # requires R package "kernlab"
lrn.svm2 = makeLearner("classif.ksvm", par.vals = list(kernel = "polydot"))

# learner's hyperparameters
getParamSet(lrn.svm1) 

# train random forests based on the given task
mod.svm1 = train(learner = lrn.svm1, task = cl.task)

## access general information of the classifier
mod.svm1
mod.svm1$learner

# predicting the target values based on the test data set 
pred.svm1 = predict(object = mod.svm1, newdata = test_set)
str(pred.svm1)
# the confusion matrix is the frequency table of all pairs of
## true and predicted classes
table(pred.svm1$data)
# The model incorrectly predicts quality 4 as quality 3. 

## computing the misclassification error and accuracy
performance(pred = pred.svm1, measures = list(mmce, acc))
#  The mmce score is 0.008438819 and we get accuracy around 99.15%.

#Random forest gives the highest accuracy rate compared to other models. 

rinst = makeResampleInstance("CV", iters = 10L, task = cl.task)

bench = benchmark(
  learners = list(lrn.dc, lrn.rf, lrn.svm1),
  tasks = cl.task,
  resamplings = rinst,
  measures = list(mmce, acc)
)

# From the benchmark study we can conclude that, Decision tree classifier gives the highest accuracy.

#### 10.Select one of the classifiers (from 9.) and perform a feature selection to determine 
# which of the data set's features are most relevant.
###Hint: Study the code chunks and associated slides in the feature selection chapter of your course materials and modify the code to match this task!

## Feature Selection

## tuning 
df.station = cl_data
df.station$cluster = as.numeric(df.station$cluster)
task = makeRegrTask(data = df.station, target = "cluster")
lrn.rf = makeLearner("regr.rpart")

set.seed(123L)
rinst.cv = makeResampleInstance("CV", iters = 10, task = task)

## definition of a "greedy" sequential forward-backward search
ctrl.seq = makeFeatSelControlSequential(method = "sffs")

## execute the respective feature selection
res.seq = selectFeatures(learner = lrn.rf, task = task,
                         resampling = rinst.cv, control = ctrl.seq)

## Features to be chosen
res.seq$x

## Which performance is achieved by the "reduced" model?
res.seq$y

## show the first four tested feature combinations
head(getOptPathX(res.seq$opt.path), 4L)




