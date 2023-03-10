x <- quakes$mag 
h <- hist(x, breaks = 20, col = "red", xlab = "Earth Quake Depth")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit,mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2]*length(x))
lines(xfit, yfit, col = "blue", lwd = 2)

library(car)
attach(quakes)
plot(lat, long, main = "Location of Recorded Earthquakes",
     xlab = "Latitude", ylab = "Longitude ", pch = 10)

lines(lowess(lat,long), col="blue") # lowess line (x,y)

scatterplot(lat ~ long, data = quakes,
            xlab="Latitude", ylab="Longitude",
            main="Location of Recorded Earthquakes")

boxplot(quakes[,3:5], horizontal=TRUE, main="Earthquake information ")


library(ggplot2)
ggplot(quakes, aes(x = lat, y = long)) + 
  geom_point(aes(color = mag)) + 
  labs(title = "Earthquake Locations Colored by Magnitude") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 10)) 

ggplot(quakes, aes(x = lat, y = long)) + 
  geom_point(aes(color = depth)) + 
  labs(title = "Earthquake Locations Colored by Depth") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 10)) 
