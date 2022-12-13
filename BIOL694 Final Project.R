# Title: BIOL694 Final Project
# Name: Isaac Van Flein
# Date: Dec 6, 2022

# Get everything set up and imported
setwd("~/Desktop")
titanic <- read.csv(file = "./titanic_data.csv")
library("tidyverse")

# Rename some of the columns
colnames(titanic)
colnames(titanic) <- c("Passenger", "Survived", "Class", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked")
colnames(titanic)

# Select columns of interest
titanic2 <- titanic[, c("Survived", "Class", "Sex", "Age", "Fare")]
head(titanic2)

# Calculate average age of male and female passengers
str(titanic2)
titanic2 %>% 
  filter(!is.na(Age)) %>% 
  group_by(Sex) %>% 
  summarize(mean_age = mean(Age))
# Here we can see the average age of women is 27.9 years old
# and the average age of men is 30.7 years old. 

# Calculate average cost of ticket by class, removing "Age" NAs
str(titanic2)
titanic2 %>% 
  filter(!is.na(Age)) %>% 
  filter(!is.na(Fare)) %>% 
  group_by(Class) %>% 
  summarize(mean_fare = mean(Fare))
# Here we can see a cost breakdown by class, showing that
# 1st class passengers paid the most (88.0 per ticket on
# average), whereas 2nd class passengers paid less than a
# quarter of that (21.5 per ticket), with 3rd class
# passengers paying an average of 13.2 per ticket.

# Displays number of male and female passengers by class (Ugly Version)
titanic2 %>% 
  count(Class, Sex, sort = TRUE) %>% 
  arrange(Class, desc(n))

# Displays average ticket cost of male and female passengers by class
titanic3 <- data.frame(titanic2)
titanicTable1 <- titanic3 %>% 
  group_by(Class, Sex) %>% 
  summarize(mean_fare = mean(Fare))
str(titanicTable1)
titanic_wide <- titanicTable1 %>% 
  pivot_wider(names_from = Sex, values_from = mean_fare)
str(titanic_wide)
# We see that women paid more for their tickets in all classes,
# especially in 1st class

# Installing packages to have fun with data plots
install.packages("corrplot")
library("corrplot")

# Basic plot
plot(x = titanic3$Age, y = titanic3$Fare)

# Making a plot with better fit and color options
plot(x = titanic3$Age, y = titanic3$Fare,
     main = "Fare Paid by Age",
     xlab = "Age (Years)", ylab = "Ticket Fare ($)",
     xlim = c(0, 100), ylim = c(0, 300), las = 1,
     pch = 23, col = "black", bg = "cornflowerblue")
axis(side = 1, labels = F, lwd = 1.5)
axis(side = 2, labels = F, lwd = 1.5)
axis(side = 3, labels = F, lwd = 1.5, tck = -0.02)
axis(side = 4, labels = F, lwd = 1.5, tck = -0.02)
axis(side = 1, labels = F, lwd = 1.5, tck = 0.02)
axis(side = 2, labels = F, lwd = 1.5, tck = 0.02)
axis(side = 3, labels = F, lwd = 1.5, tck = 0.02)
axis(side = 4, labels = F, lwd = 1.5, tck = 0.02)
box(lwd = 1.5)

#Save plot defaults
opar <- par(no.readonly = TRUE)

# Save the plot as a PDF
dev.copy2pdf(file = "./titanic3plot1.pdf",
             width = 6, height = 4, bg = "white", compress = F, out.type = "pdf")

# Install ggplot packages
install.packages("ggplot2")
library("ggplot2")

# Use ggplot to chart Fare by Age and Sex
ggplot(data = titanic3, aes(x = Age, y = Fare, color = Sex)) +
  geom_point() +
  labs(title = "Fare paid by Age and Sex") +
  xlab("Passenger Age") +
  ylab("Fare Paid") +
  ylim(c(0,300)) +
  theme_light(base_size = 14)
# I limited the y axis to exclude anomolies (3 passengers paid a LOT for that max level suite)

# Save the ggplot
ggsave("titanic3plot2.pdf", plot=last_plot(), device=NULL, path=NULL,
       scale=1, width=6, height=4, dpi=300, limitsize=TRUE)

# Use ggplot to chart Fare by Age and Survival
ggplot(data = titanic3, aes(x = Age, y = Fare, color = Survived)) +
  geom_point() +
  labs(title = "Survival by Age and Fare") +
  xlab("Passenger Age") +
  ylab("Fare Paid") +
  ylim(c(0,300)) +
  theme_light(base_size = 14)

# Use ggplot to chart Fare by Sex and Survival
ggplot(data = titanic3, aes(x = Survived, y = Fare, color = Sex)) +
  geom_point() +
  labs(title = "Survival by Fare Paid and Sex") +
  xlab("Survived the Titanic Sinking") +
  ylab("Fare Paid") +
  ylim(c(0,300)) +
  theme_light(base_size = 14)
# Graph looks weird, but you can see that women survived much more often than men

# Install vegan 
install.packages("vegan")
library("vegan")

# Install other stuff from the Data Reduction lesson that will come in handy here
install.packages("remotes")
remotes::install_github("lter/lterdatasampler")
library("lterdatasampler")

# Installing packages to have fun with data plots
install.packages("corrplot")
library("corrplot")
# Had to do that again for some reason. Let's try some correlations!

# Make a sweet biplot!
titanic4 <- na.omit(titanic3)
titanic4$Sex <- ifelse(titanic4$Sex=="female",1,-1)
titanic5 <- data.frame(titanic4)
titanic5_std <- scale(titanic5, center = T, scale = T) # Z-scale
titanic_pca <- prcomp(na.omit(titanic5_std))
biplot(titanic_pca)
