library(readr)
library(ggplot2)
library(gridExtra)
library(grid)
hotdogs <- read_csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv")
head(hotdogs)
colnames(hotdogs) <- c("Year", "Winner", "Dogs.eaten", "Country", "New.record")

hotdogs$new.record <- ifelse(hotdogs$New.record == 0, "No", "Yes")
ggplot(hotdogs, aes(x = Year, y = Dogs.eaten, color = new.record, shape = Country)) + geom_point() + labs(title = "Hot Dog Eating Contest Results", y = "Hot Dogs Eaten", subtitle = "Nathan's Contest Results From Year 1980 to 2010", caption = "Yau, N. (2011). Visualize this: The flowingdata guide to design, visualization, and statistics. Wiley Pub. ", color = "New Record") + scale_color_manual(values = c("turquoise", "red")) + scale_x_continuous(breaks = seq(1980, 2010, by = 5)) + theme_classic()

hotdogs$new.record <- ifelse(hotdogs$New.record == 0, "No", "Yes")
ggplot(hotdogs, aes(x = Year, y = Dogs.eaten, color = new.record, fill = Country)) + geom_bar(stat = "identity") + labs(title = "Hot Dog Eating Contest Results", y = "Hot Dogs Eaten", subtitle = "Nathan's Contest Results From Year 1980 to 2010", caption = "Yau, N. (2011). Visualize this: The flowingdata guide to design, visualization, and statistics. Wiley Pub. ", color = "New Record", fill = "Country") + scale_color_manual(values = c("black", "red")) + scale_fill_manual(values = c("lightgoldenrod", "turquoise", "palegreen", "red")) + scale_x_continuous(breaks = seq(1980, 2010, by=5))  + geom_text(aes(label=Dogs.eaten), size=2, position=position_dodge(width=0.9), vjust=-0.25) + theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=2,byrow=TRUE), color=guide_legend(nrow=2,byrow=TRUE))

hot_dog_places <-
  read.csv("http://datasets.flowingdata.com/hot-dog-places.csv",
            sep=",", header=TRUE)
names(hot_dog_places) <-
  c("2000", "2001", "2002", "2003", "2004",
    "2005", "2006", "2007", "2008", "2009", "2010")

hot_dog_matrix <- as.matrix(hot_dog_places)

# Yau's example
barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0, 200),
        xlab="Year", ylab="Hot dogs and buns (HDBs) eaten",
        main="Hot Dog Eating Contest Results, 1980-2010")
# Create a data frame for ggplot
hdp_df <- data.frame(names(hot_dog_places), hot_dog_matrix[1,], hot_dog_matrix[2,], hot_dog_matrix[3,])
names(hdp_df) <- c("Year", "1st", "2nd", "3rd")
hdp_m <- melt(hdp_df, id = 'Year')

# Plot
ggplot(hdp_m, aes(fill=variable, y=value, x=Year)) + 
  geom_bar(position="stack", stat="identity") + scale_fill_manual(values = c("tomato", "salmon1", "lightgoldenrod")) + labs(title = "Hot Dog Eating Contest Results", y = "Hot Dogs Eaten", subtitle = "Nathan's Contest Results From Year 1980 to 2010", caption = "Yau, N. (2011). Visualize this: The flowingdata guide to design, visualization, and statistics. Wiley Pub. ", fill = "Places")

# Economics dataset
data("economics")
e <- as.data.frame(economics)
p <- ggplot(e, aes(x = date, y = pce)) + geom_line() + labs(title = "Personal Consumption Expenditures (in Billions USD)")
p1 <- ggplot(e, aes(x = date, y = pop)) + geom_line() + labs(title = "Population")
p2 <- ggplot(e, aes(x = date, y = psavert)) + geom_line() + labs(title = "Personal Savings Rate")
p3 <- ggplot(e, aes(x = date, y = uempmed)) + geom_line() + labs(title = "Median Duration of Unemployment (in weeks)")
p4 <- ggplot(e, aes(x = date, y = unemploy)) + geom_line() + labs(title = "Number of Unemployed in Thousands")
grid.arrange(p,p1,p2,p3,p4)

