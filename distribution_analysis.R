setwd("/Users/lukebernier/Projects/fantasy football") # Replace with your directory

players <- read.csv('player_data_all.csv', header = TRUE) #read data from csv
true_points_dist <- players[, -c(1, 3, 4, 5)] #delete irrelevant columns - only col kept is points

summary(true_points_dist) # summary stats
hist(true_points_dist) #histogram of points

#install and load distribution analysis package
library(fitdistrplus)

descdist(data=true_points_dist, discrete=FALSE) #generate cullen and frey graph to analyze distribution
descdist(data=true_points_dist, discrete=FALSE, boot=1000)

plotdist(data=true_points_dist, histo=TRUE, demp=TRUE)

true_points_dist[true_points_dist==0] <- 10

#min-max normalization to fit beta distribution 
min_max_norm <- ((true_points_dist-min(true_points_dist))/(max(true_points_dist)-min(true_points_dist)))
summary(min_max_norm) # summary stats

min_max_norm[min_max_norm==0] <- 0.01
min_max_norm[min_max_norm==1] <- 0.99

beta_ <- fitdist(min_max_norm, "beta")
normal_ <- fitdist(true_points_dist, "norm")
gamma_ <- fitdist(true_points_dist, "gamma")

plot(beta_)
plot(normal_)
plot(gamma_)

summary(beta_)
summary(normal_)
summary(gamma_)

