#donna aziz

#clear environment
rm(list=ls())

#sets working directory
setwd("/Users/danna/Desktop/data visualization")
getwd()

#import data for project

visualdata<- read.csv("/Users/danna/Desktop/data visualization/project_data.csv")

#load tidyverse
library(tidyverse)
library(dash)
library(ggplot2)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

#regression btwn pop and freq. 

#runs reg analysis and plots reg line
fit<-lm(frequency~population,data=visualdata)
summary(fit)

#plot regression 
plot(fit)
abline(fit)

#scatterplot between population and crime freq.

scatter<-ggplot(visualdata, aes(x=population, y=frequency)) + geom_point() + geom_point(color="lightpink") + labs(title = "Scatterplot for Population vs. Frequency", x ="Population", y="Frequency") 

scatter2<- scatter+theme(plot.title = element_text(hjust = .5, color="steelblue", size=14, face="bold.italic"),axis.title.x = element_text(color="maroon3", size=14, face="bold"), axis.title.y = element_text(color="maroon3", size=14, face="bold"))

#render the above into interactive scatterplot
library(plotly)
ggplotly(scatter2)
scatter3<- ggplotly(scatter)

# save the widget at .html format
library(htmlwidgets)
saveWidget(scatter3, file="scatter2.html")

#dummy variable for collapsing into high and low pop. 
#any value that is at or above the 100k is a 1
visualdata$pop_above<-0
visualdata$pop_above[which(visualdata$population>=100000)] <- 1

#regression for popabove
#log reg bc binary dv. 
newfit<-glm(frequency~pop_above,data=visualdata)
summary(newfit)

plot(newfit)+labs(title = "Plot for Frequency ~ PopAbove")
abline(newfit) 

#logplot
above<- ggplot(data = visualdata, mapping = aes(x= pop_above, y= frequency, color ="redbrick"))+ geom_point(color="palevioletred1")+
  geom_smooth(method = "loess") + scale_x_log10() +
  labs(title = "Log Plot for Pop Above 100,000 ~ Frequency", x = "Population", y="Frequency")+
  theme(
    plot.title = element_text(hjust = .5, color="plum", size=14, face="bold.italic"),
    axis.title.x = element_text(color="plum", size=14, face="bold"),
    axis.title.y = element_text(color="plum", size=14, face="bold"))

#make it interactive #may not include this in website
ggplotly(above)

##interactive plot 
library(ggplot2)
library(plotly)
library(gapminder)

#anything below 100k is 0
visualdata$pop_below<-0
visualdata$pop_below[which(visualdata$population<=100000)]<-0

#plotting
#log plot
popplot<- ggplot(data = visualdata, mapping = aes(x= population, y= frequency))+
          geom_point(color="darkorchid3")+
          geom_smooth(method = "loess") + scale_x_log10() +
          labs(title = "Log Plot for Population ~ Frequency", x = "Population", y="Frequency")+
          theme(
          plot.title = element_text(hjust = .5, color="darkmagenta", size=14, face="bold.italic"),
          axis.title.x = element_text(color="plum", size=14, face="bold"),
          axis.title.y = element_text(color="plum", size=14, face="bold")
          )

#make it interactive
ggplotly(popplot)
plotwidget<-ggplotly(popplot)

# save the widget at .html format
library(htmlwidgets)
saveWidget(plotwidget, file="plot.html")

#freq of diff crimes in collin c. for 2018

#sum up crime

#data frame to aggregate the diff crimes for the plots
newdata<-data.frame(aggregate(visualdata$frequency, by=list(p_type=visualdata$p_type), FUN=sum))

#plot for crime freq by category
ggplot(newdata, aes(x=p_type, y=x)) + 
  geom_bar(stat = "identity", color="blue", fill=rgb(0.1,0.4,0.5,0.7, 0.8)) +geom_point(color="purple")+
  geom_smooth(method = "loess") +labs(title = "Crime in Collin County (2018)", x = "Crime Category", y="Frequency") +
  geom_text(vjust = -.5, aes(label = x ), position = position_dodge(0.9), color="black") + theme(
    plot.title = element_text(hjust = .5, color="purple", size=14, face="bold.italic"),
    axis.title.x = element_text(color="light blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

#plot for crime freq in each city

#agg by city
citydata<-data.frame(aggregate(visualdata$frequency, by=list(city_name=visualdata$city_name), FUN=sum))

#plot for crime freq by city
ggplot(citydata, aes(x=city_name, y=x)) + 
  geom_bar(stat = "identity", color="pink", fill=("firebrick3")) +geom_point(color="coral4")+
  geom_smooth(method = "loess") +labs(title = "Crime in Collin County (2018)", x = "City", y = "Frequency") +
  geom_text(vjust =-.5, aes(label = x), position = position_dodge(0.9), color="coral2") + theme(
  plot.title = element_text(hjust = .5, color="brown4", size=14, face="bold.italic"),
  axis.title.x = element_text(color="brown4", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold")
)

#agg by population for city
citypopulation<-data.frame(aggregate(visualdata$population, by=list(city_name=visualdata$city_name), FUN=sum))

#now plot it
ggplot(citypopulation, aes(x=city_name, y=x)) + 
  geom_bar(stat = "identity", color="pink", fill=("salmon")) +geom_point(color="coral4")+
  geom_smooth(method = "loess") +labs(title = "Populations of Cities in Collin County (2018)", x = "City", y = "Population") +
  geom_text(vjust =-.5, aes(label = x), position = position_dodge(0.9), color="coral2") + theme(
    plot.title = element_text(hjust = .5, color="skyblue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="skyblue", size=14, face="bold"),
    axis.title.y = element_text(color="skyblue", size=14, face="bold")
  )
##########

# data frames for the GIF
newdata <- data.frame(
  group=c("burglary ","larceny-theft","motor vehicle theft","arson") , 
 value=c(9065,25541,25541, 197) , 
 number_of_obs=c(1,26,0,0)
)

##animated bar plot
#dallas crime freq.
a <- data.frame(group=c("burglary","larceny-theft","motor vehicle theft", "arson"), 
                values=c(9065,25541,9660, 197), frame=rep('a',4))

b <- data.frame(group=c("burglary","larceny-theft","motor vehicle theft", "arson"), 
                values=c(1,26,0,0), frame=rep('b',4))
data <- rbind(a,b)  

#libraries for animated plot
install.packages("gifski")
library(gapminder)
library(gganimate)
library(gifski)
#basic barplot
ggplot(a, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity')

# Make a ggplot, but add frames 
myPlot<-ggplot(data, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity', color="pink", fill=("darkorchid1")) +labs(title = "Crime in Dallas and Lavon (2018)", x = "Crime Category", y="Frequency") +
  theme_bw() +  geom_text(vjust = -.5, aes(label = values ), position = position_dodge(0.9), color="mediumpurple") +
    theme(
    plot.title = element_text(hjust = .5, color="pink3", size=14, face="bold.italic"),
    axis.title.x = element_text(color="indianred2", size=14, face="bold"),
    axis.title.y = element_text(color="indianred2", size=14, face="bold"))+
  # gganimate specific bits:
  transition_states(
    frame,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out') 

##animate the barplot
animate(myPlot, duration = 5, fps = 20, width = 450, height = 450, renderer = gifski_renderer())

# Save it as a gif:
anim_save("dallas-lavon.gif")

####################

#calculate future positions
newdata$right <- cumsum(newdata$number_of_obs) + 30*c(0:(nrow(newdata)-1))
newdata$left <- newdata$right - newdata$number_of_obs 

#column chart

# Create the data for the chart
newdata<-data.frame (
cityofallen <- c(115,1095,67,0),
crimetype<- c("burglary","larceny-theft","motor vehicle theft","arson"))


##trying another bar plot

#aggregate by crime 

crime_category<-data.frame(aggregate(visualdata$frequency, by=list(p_type=visualdata$p_type), FUN=sum))

#plot freq of crime category in collin county

ggplot(crime_category, aes(x=p_type, y=x)) + 
  geom_bar(stat = "identity", color="pink", fill=rgb(0.9,0.3,0.4)) +geom_point(color="violetred4")+
  geom_smooth(method = "loess") +labs(title = "Crime in Collin County (2018)", x = "Crime Category", y="Frequency") +
 geom_text(vjust =-.5, aes(label = x), position = position_dodge(0.9), color="palevioletred3") + theme(
   plot.title = element_text(hjust = .5, color="rosybrown", size=14, face="bold.italic"),
   axis.title.x = element_text(color="plum3", size=14, face="bold"),
   axis.title.y = element_text(color="pale violet red3", size=14, face="bold")
 )

#crime freq. in Allen
ggplot(newdata, aes(x=crimetype, y=cityofallen)) + 
  geom_bar(stat = "identity", color="lightskyblue", fill="skyblue3") +geom_point(color="royalblue4") +
  geom_smooth(method = "loess") +labs(title = "Crime in Allen (2018)", x = "Crime Category", y="Frequency") +
  geom_text(vjust =-.5, aes(label = cityofallen), position = position_dodge(0.9), color="black") +
    theme(
    plot.title = element_text(hjust = .5, color="hotpink2", size=14, face="bold.italic"),
    axis.title.x = element_text(color="hotpink2", size=14, face="bold"),
    axis.title.y = element_text(color="hotpink2", size=14, face="bold")
    )

#flipit
ggplot(newdata, aes(x=crimetype, y=cityofallen)) + 
  geom_bar(stat = "identity", color="pink2", fill="rosybrown3") +geom_point(color="hotpink4")+
  geom_smooth(method = "loess") +labs(title = "Crime in Allen (2018)", x = "Crime Category", y="Frequency") +
  geom_text(vjust =-.5, aes(label = cityofallen), position = position_dodge(0.9), color="black") +
  theme(
    plot.title = element_text(hjust = .5, color="plum", size=14, face="bold.italic"),
    axis.title.x = element_text(color="plum", size=14, face="bold"),
    axis.title.y = element_text(color="plum", size=14, face="bold")
  ) +coord_flip()



