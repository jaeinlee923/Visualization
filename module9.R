library(ggplot2)
library(reshape2)
library(dplyr)
library(ggrepel)

d <- read.csv("Dog Intelligence.csv")
# Delete rows after 136 since they are filled with NA values. 
d <- d[1:136,]

d$height <- rowMeans(d[,c(2,3)])
d$weight <- rowMeans(d[,c(4,5)])
d$rep <- rowMeans(d[,c(6,7)])

rownames(d) <- d$Breed

d <- d[,-1:-7]

ggplot(d, aes(x = weight, y = height, color = rep)) +
  scale_color_gradient(low="moccasin", high="navy") +
  geom_point() +
  labs(x = "\nHeight", y = "Weight\n", color = "Repititions", alpha = "Repititions", title = "Number of Repetitions Required for Dog Breeds to Learn New Commands")

ggplot(d, aes(x = weight, y = height, color = rep, size = rep, label = row.names(d))) +
  labs(x = "\nHeight", y = "Weight\n", color = "Repititions", alpha = "Repititions", title = "Number of Repetitions Required for Dog Breeds to Learn New Commands") +
  geom_text(vjust = "inward", hjust = "inward", position=position_jitter(width=1,height=1), fontface = "bold") +
  scale_color_gradient(low="moccasin", high="navy")


admi <- UCBAdmissions 
dat <- as.data.frame(admi)
df_gender <- dat %>% 
  subset(Admit %in% "Admitted") %>% 
  group_by(Gender) %>%
  summarize(n = n(),
            mean = mean(Freq), 
            sd = sd(Freq),
            se = sd / sqrt(n),
            ci = qt(0.975, df = n - 1) * se)

# calculate means and standard errors by Gender, Admit, Department
dat_plot <- dat %>%
  group_by(Gender, Admit, Dept) %>%
  summarize(mean = mean(Freq))

# create better labels for discipline
dat_plot$Admit <- factor(dat_plot$Admit,
                              labels = c("Admitted",
                                         "Rejected"))
# create plot
ggplot(dat_plot, 
       aes(x = Gender, 
           y = mean,
           fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0, 900, 50)) +
  facet_grid(. ~ Dept + Admit) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x="", 
       y="", 
       title="Gender Ratio of Total Applicants and Admission Rate for Each Department\n") +
  scale_fill_manual(values = c("steelblue", "red"))
