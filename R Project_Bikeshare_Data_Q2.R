#Question 2
#What is the total travel time for users in different cities? 

#Getting familiar with the data sets 
head(ny,3)
head(wash,3)
head(chi,3)

#Finding the total trip durations
ny_sum <- sum(as.numeric(ny$Trip.Duration), na.rm = TRUE)
ny_sum
wash_sum <- sum(as.numeric(wash$Trip.Duration), na.rm = TRUE)
wash_sum
chi_sum <- sum(as.numeric(chi$Trip.Duration), na.rm = TRUE)
chi_sum

# Data Preparation for visualizing 
#1. Step Checking for NA values in the 'Day' column in order not to get a blank graph while visualizing

#First, "Wash"
library(tidyverse)
wash.new <- drop_na(wash,8)
#The same procedure for "Chi"
chi.new<- chi[-8630,-9]
chi.new2 <- drop_na(chi.new,6)
chi.new3<-drop_na(chi.new2,9)
chi.final<-drop_na(chi.new3,3)

#Visualization Step
## Providing violin graphs in order to illustrate how "time duration" data is distributed across the three cities.
## User.Type was also considered.
library(ggplot2)
function_graph=function(data){
  x <- ggplot(data,aes(x=Day,y=Trip.Duration,colour=Day))+
    geom_violin(size=I(1.2))+
    facet_grid(data$User.Type~.,scales="free", drop=TRUE) +
    xlab('Week of the Day') +
    ylab('Trip Duration') +
    coord_cartesian(ylim=c(0,3000)) +
    ggtitle('Trip Duration on a Daily Basis') +
    theme(axis.title.x=element_text(colour="Black", size=15),
          axis.text.x=element_text(size=10),
          plot.title= element_text(size=20,
                                   colour ="Blue",
                                   hjust = 0.5))
  return(x)
} 
# Graph for NY
function_graph(ny)
# Graph for Washington
function_graph(wash.new)
# Grap for Chicago
function_graph(chi.final)