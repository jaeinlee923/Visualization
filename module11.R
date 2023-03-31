# install.packages(c("CarletonStats", "epanetReader", "fmsb", "ggthemes","latticeExtra", "MASS", "PerformanceAnalytics", "psych", "plyr","prettyR", "plotrix","proto", "RCurl", "reshape", "reshape2"))

my_packages <- c("CarletonStats", "devtools", "epanetReader", "fmsb", "ggplot2", "ggthemes","latticeExtra", "MASS", "PerformanceAnalytics", "psych", "plyr", "prettyR", "plotrix","proto", "RCurl", "reshape", "reshape2")

lapply(my_packages, require, character.only = TRUE) 


library(readr)
hotdogs <- read_csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv")
colnames(hotdogs) <- c("Year", "Winner", "Dogs.eaten", "Country", "New.record")
x <- hotdogs$Year
y <- hotdogs$Dogs.eaten
png(width=1000, height=600)
plot(y ~ x, axes=F, xlab="", ylab="", pch=16, type="b")
# Add axis
axis(1, at=x, label=x, tick=F, family="serif")
axis(2, at=seq(0,75,5), label=sprintf("%s dogs",seq(0,75,5)), tick=F, las=2, family="serif")
# Add dashed horizontal lines
abline(h=70,lty=2)
abline(h=62,lty=2)
# Add the caption
text(max(x), min(y)*2.5,"Nathan's recordings of\ntotal hotdogs eaten by\nwinners each year.", adj=1, 
     family="serif")
text(2001, max(y)/1.03, labels="5%", family="serif")
dev.off()

library(highcharter)
library(dplyr)
d <- data.frame(x, y)
h <- highchart() %>%
  hc_chart(type = "scatter") %>% 
  hc_subtitle(text = "Nathan's recordings of total hotdogs eaten by winners each year.") %>%
  hc_yAxis(labels = list(format = "{value} dogs")) %>%
  hc_add_series(data = d) %>% 
  hc_add_theme(hc_theme_tufte())

# save the widget at .html format
library(htmlwidgets)
saveWidget(h, file="myFile.html")
# save the .html at png format
library(webshot)
webshot::install_phantomjs()
webshot("myFile.html" , "output.png", delay = 0.2)
dev.off()


library(devtools)
source_url("https://raw.githubusercontent.com/sjmurdoch/fancyaxis/master/fancyaxis.R")
# Replace NAs
air <- as.data.frame(airquality)
O_m <- median(air$Ozone, na.rm = TRUE)
S_m <- median(air$Solar.R, na.rm = TRUE)
# Use period to refer to all variables (columns)
air$Ozone[is.na(air$Ozone)]<-O_m
air$Solar.R[is.na(air$Solar.R)]<-S_m
x <- air$Solar.R
y <- air$Ozone
plot(x, y, axes = FALSE, pch=16, cex=0.8,
     xlab="Solar Radiation Levels", ylab="Ozone levels", 
     xlim= c(min(x)/1, max(x)+10), ylim=c(min(y)/1.5, max(y)+60))
title(main = "Relationship between Solar Radiation and Ozone Levels")
axis(1, tick = F)
axis(2, tick = F, las = 2)
axisstripchart(air$Solar.R, 1)
axisstripchart(air$Ozone, 2)

ggplot(air, aes(factor(Temp), Ozone)) + theme_tufte() +
  geom_tufteboxplot(outlier.colour="transparent") + theme(axis.title=element_blank()) +
  annotate("text", x = 23, y = 150, adj=1,  family="serif",
           label = c("Wind speed in miles per hour on average\nOzone levels in parts per Billion."))

