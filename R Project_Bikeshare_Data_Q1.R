ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny,3)
tail(ny,3)
head(wash,3)
tail(wash,3)
head(chi,3)
tail(chi,3)

#Question 1
#What is the most common day of week for NY? 
#PS: Please provide also an illustration for the density of weekdays by taking 'gender' also into account.

# Specfying the weekdays
ny$Day <- weekdays(as.Date(ny$Start.Time))
wash$Day <- weekdays(as.Date(wash$Start.Time))
chi$Day <- weekdays(as.Date(chi$Start.Time))
# Check if the new column was created as desired
names(ny)
names(wash)
names(chi)
head(ny,3)
# Providing the solution  
by(ny$Day, ny$Gender, summary)
#Visualization by considering gender role
library(ggplot2)
ggplot(data=ny, aes(x=Day)) +
  geom_bar(colour='orange', fill='light blue') +
  facet_grid(ny$Gender~.,scales="free") +
  xlab('Week of the Day') +
  ylab('') +
  ggtitle('The Most Common Weekday (NY)') +
  theme(axis.title.x=element_text(colour="DarkGreen", size=15),
        axis.text.x=element_text(size=10),
        plot.title= element_text(size=20,
                                 colour ="Blue",
                                 hjust = 0.5))
#Adding Summary Part 
summary_female=summary(ny$Trip.Duration[ny$Gender == 'Female'], basic = T)
summary_male=summary(ny$Trip.Duration[ny$Gender == 'Male'], basic = T)
summary_female
summary_male

