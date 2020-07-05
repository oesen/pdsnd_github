#Question 3
#Can you investigate the most busy weekday (Wednesday from the Q1) in NY by considering 'generation' factor at the same time? 
#An appropriate visualization of the findings should be also provided.

# Data Preparation Phase
#Creating 'Day' column again
ny$Day <- weekdays(as.Date(ny$Start.Time))
#Excluding NA and the values elder than 1960 from the sample, since people who were born earlier than 1960s are not considered within the scope of this question.
ny.filtered= subset(ny, ny$Birth.Year>=1960)       
# Determining the generation based on the birth year
ny.filtered$Generation<- ifelse(ny.filtered$Birth.Year < 1981, "X",
                                ifelse(ny.filtered$Birth.Year %in% 1981:1996, "Y", "Z"))
# Quick Check if the sample still covers the undesired observations
# head(ny.filtered,100)
# Specifying the number of excluded rows
nrow(ny)
nrow(ny.filtered)

# Subsetting Data and checking if the desired subset was created
subset <- subset(ny.filtered, Day == 'Wednesday',select=c(Gender, Birth.Year, Day, Generation))
head(subset,3)

library(plyr)
counting_generation=count(subset, "Generation")
counting_generation
#Visualization of the findings
library(ggplot2)
g <- ggplot(data=subset, aes(x=Generation, y=Birth.Year,
                             colour=Generation))
g + geom_boxplot(size=1.2) + geom_jitter(alpha=0.1) +
  ylab('Birth of Year')+
  ggtitle('The Dist. of Birth of Year vs. Generation in the Selected Sample')+
  theme(axis.title.x=element_text(colour="Red", size=10),
        axis.title.y=element_text(colour="Blue", size=10),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        plot.title= element_text(size=15,
                                 colour ="Navy Blue",
                                 hjust = 0.5))
#Adding Summary Part
summary_for_GenX=summary(subset$Birth.Year[subset$Generation == 'X'], basic = T)
summary_for_GenY=summary(subset$Birth.Year[subset$Generation == 'Y'], basic = T)
summary_for_GenZ=summary(subset$Birth.Year[subset$Generation == 'Z'], basic = T)
summary_for_GenX
summary_for_GenY
summary_for_GenZ