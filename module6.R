library(AER)
data("Guns")
# inspect 
head(Guns)
# Subset data into two based on the shall carry law
guns_y <- Guns[Guns$law=="yes",]
guns_n <- Guns[Guns$law=="no",]

# boxplots
boxplot(guns_y$violent, guns_n$violent, main="Violent Crime Rate", names = c("Guns", "No Guns"))
boxplot(guns_y$murder, guns_n$murder, main="Murder Rate", names = c("Guns", "No Guns"))
boxplot(guns_y$robbery, guns_n$robbery, main="Robbery Rate", names = c("Guns", "No Guns"))
boxplot(guns_y$prisoners, guns_n$prisoners, main="Incarceration Rate", names = c("Guns", "No Guns"))

# histogram
hist(guns_y$violent , main = "Violent Crime Rate when Guns are legal", xlab = "Violent Crime Rate", ylab = "Frequency", xlim = c(0,3000), ylim = c(0,250))
hist(guns_n$violent , main = "Violent Crime Rate when Guns are not legal", xlab = "Violent Crime Rate", ylab = "Frequency")

library(reshape2)
library(ggplot2)
# Melt data by law
# select columns with crime rate data and law
g_crime <- Guns[,c(2:5,13)]
gc_melted <- melt(g_crime, id = "law")

# create a boxplot
ggplot(gc_melted, aes(x = value, y = variable, color = law)) +
  geom_boxplot() + 
  labs(x = "\nCrime Rates", y = "Types\n ", title = "\n Overall Crime Rates\n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 10))

# Aggregate data by law
gm_agg <- g_crime %>% group_by(law) %>% 
  summarise(mean_violent = mean(violent),
            mean_murder = mean(murder),
            mean_robbery = mean(robbery),
            mean_prisoners = mean(prisoners),
            .groups = 'drop')
gm_agg_df <- as.data.frame(gm_agg)
colnames(gm_agg_df) <-  c("Guns_Allowed", "Violent", "Murder", "Robbery", "Incarceration ")
gm_agg_m <- melt(gm_agg_df, id = "Guns_Allowed")

# create a histogram with ggplot
ggplot(data = gm_agg_m, aes(x = variable, y = value, fill = Guns_Allowed)) +
  geom_bar(stat = "identity", position = position_dodge())  +
  ylim(0,600) +
  labs(x = "\nType", y = "Crime Rates\n ", title = "\n Overall Crime Rates \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 10)) 

# Convert law data to 1 and 0
GunsM <- Guns %>%
  mutate(guns_allowed = ifelse(law == "no",0,1))
# Aggregate data by state
Guns_agg <- GunsM %>% group_by(state) %>% 
  summarise(violent = mean(violent),
            murder = mean(murder),
            robbery = mean(robbery),
            prisoners = mean(prisoners),
            density = mean(density),
            income = mean(income),
            guns_allowed = mean(guns_allowed),
            .groups = 'drop')

Guns_agg_df <- as.data.frame(Guns_agg)
row.names(Guns_agg_df) <- Guns_agg_df$state
Guns_agg_df <- Guns_agg_df[,2:8]
stars(Guns_agg_df, main="PieChart of Different Crimes and Factors", flip.labels = FALSE, key.loc = c(15, 1.5), draw.segments=TRUE)

