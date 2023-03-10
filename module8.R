library(ggplot2)
library(reshape2)
library(mice)
f <- read.csv("BenderlyZwick.csv")
# impute data
i_f <- mice(f, m = 5,  method = "rf")
f_compl <- complete(i_f, 1)
which(is.na(f_compl))
# Drop the first column since it's simply listing the row numbers
corr <- cor(f_compl[,2:6], use="complete.obs", method="pearson")
corr

# Make upper triangular matrix by setting NA to lower triangular part
upper_corr <- corr
upper_corr[lower.tri(upper_corr)] <- NA
# Melt this upper triangular matrix and remove NA values
upper_corr_m <- melt(upper_corr, na.rm = TRUE)
# Plot the triangular matrix
qplot(x = Var1, y = Var2, data = upper_corr_m, fill = value, geom = "tile") + 
  labs(title = "Correlation Matrix of Economic Data\n", caption = paste("source: Benderly and Zwick (1985), p. 1116"), x = element_blank(), y = element_blank(), fill = "correlation\ncoefficients") +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
            fontface = "bold", size = 4) +
  scale_fill_gradient(low="lightyellow", high="red")


# get means, standard deviations, and 95% confidence intervals for adimssion rate by sex 
library(dplyr)
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


p <- ggplot(df_gender, 
            aes(x = Gender, y = mean, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_y_continuous(limits = c(-20, 450)) +
  labs(title = "Student Admissions at UC Berkeley by Gender",
       subtitle = "Average Number of Students Admitted to UC Berkeley in 1973",
       caption = paste("source: Built in R documentation, 
UCBAdmissions {datasets}"),
       x = "Gender",
       y = "Admission") 

# plot with confidence limits
p +  geom_errorbar(aes(ymin = mean - ci, 
                       ymax = mean + ci), 
                   width = .1, position = position_dodge(.5)) +
  ggplot2::annotate("text", 
                    label = "I-bars are 95% \nconfidence intervals", 
                    x = 2, 
                    y = 220,
                    fontface = "italic",
                    size = 3)


# Group them by department
df_dept <-  aggregate(Freq ~ Dept + Gender + Admit, data = dat, FUN = sum)
# Get Admission Ratio
df_dept_admit <- df_dept %>% 
  group_by(Dept, Admit) %>% 
  summarize(Total = sum(Freq)) %>%
  mutate(AdmissionRate = paste0(round(Total/sum(Total)*100, 2))) %>% 
  subset(Admit %in% "Admitted")
# Drop unnecessary data
df_dept_admit <- df_dept_admit[,-2:-3]

# This time, we will ignore the admission rate and check the number of students who applied to each departements
df_dept_gen <- df_dept %>% 
  group_by(Dept, Gender) %>% 
  summarize(ApplicantTotal = sum(Freq)) %>%
  mutate(GenderRatio = paste0(round(ApplicantTotal/sum(ApplicantTotal)*100, 2))) 
# Merge data
df_merge <- merge(df_dept_admit, df_dept_gen, by = "Dept")

# Plot
ggplot(df_merge, aes(x = Dept, y = as.numeric(GenderRatio), group = Gender, fill = Gender)) +
  geom_bar(stat="identity", width = 0.5) +
  geom_text(aes(label = GenderRatio), size = 3, position = position_stack(vjust = 0.5)) +
  geom_line(mapping = aes(x = Dept, y = as.numeric(AdmissionRate)), color = "red")+
  geom_point(mapping = aes(x = Dept, y = as.numeric(AdmissionRate)), show.legend = FALSE, color = "red")  +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*1, name="Admission Rate")) + 
  labs(title = "Student Admissions at UC Berkeley",
       subtitle = "Gender Ratio of Total Applicants and Admission Rate for Each Department",
       x = "Department",
       y = "Gender Ratio") +
  ggplot2::annotate("label", 
                    label = "The red line represents\n the admission rate", 
                    x = "B", 
                    y = 74,
                    fontface = "italic",
                    size = 3) +
  theme(legend.position="bottom", legend.box = "horizontal")

dat_fem <- subset(dat, Gender %in% "Female")

ggplot(dat_fem, aes(x = Dept, y = Freq, shape = as.factor(Admit), color = as.factor(Admit))) +   
  geom_point()+
  geom_smooth(method = lm, se = FALSE)
