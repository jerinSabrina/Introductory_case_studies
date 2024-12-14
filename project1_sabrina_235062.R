# Personal Information ----
title <- "Paper 1: Descriptive Data Analysis"
author <- "Sabrina Sultana"
date_of_analysis <- Sys.Date()  # Automatically sets today's date

# Display personal information with formatted date
cat("Title:", title, "\nAuthor:", author, "\nDate of Analysis:", format(date_of_analysis, "%Y-%m-%d"), "\n\n")


install.packages("ggcorrplot")
install.packages("tidyr")


library(corrplot)
library(tidyr)
library(dplyr)
library(ggplot2)
library("reshape2")
library(ggcorrplot)


file.exists("/Users/sabrina/Downloads/census2004_2024.csv")
# Reading a CSV file into a data frame
census2004_2024 <- read.csv("/Users/sabrina/Downloads/census2004_2024.csv", header = TRUE)
# Omit rows with NA in any column
census2004_2024 <- na.omit(census2004_2024)
# To view the first few rows of the data
head(census2004_2024)
unique(census2004_2024$Region)

#1

### frequency Distributions of life.expectancy.across.sexes.2024 #####

# Filter, select, and reshape the data
life.expectancy.across.sexes.2024 <- census2004_2024 %>%
  filter(Year == 2024) %>% 
  select(c(Life.Expectancy.at.Birth..Males, Life.Expectancy.at.Birth..Females, Name)) %>% 
  pivot_longer(
    cols = !Name,
    names_to = "Sex",
    values_to = "Age"
  ) %>%
  mutate(Sex = gsub("\\Life.Expectancy.at.Birth..", "", Sex))

# Calculate mean and median
life.expectancy.across.sexes.2024.measures <- life.expectancy.across.sexes.2024 %>%
  group_by(Sex) %>%
  summarise(Mean = mean(Age), Median = median(Age)) %>%
  pivot_longer(!Sex, names_to = "Measures", values_to = "Measure_Value")

# Print mean and median values
print("Mean and Median Values:")
print(life.expectancy.across.sexes.2024.measures)

# Plot the histogram with mean and median lines
life.expectancy.across.sexes.2024.plot <- life.expectancy.across.sexes.2024 %>%
  ggplot(aes(Age, color= Sex, fill = Sex)) +
  geom_histogram(alpha=0.8, binwidth = 5, show.legend = FALSE) +
  theme(
    panel.spacing = unit(0.5, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  ylab("Frequency") +
  xlab("Age") +
  scale_x_continuous(breaks = seq(50, 100, by = 5)) +
  ggtitle("Life Expectancy at birth for different sexes") +
  facet_wrap((~Sex)) +
  geom_vline(aes(xintercept = Measure_Value, color = Measures), life.expectancy.across.sexes.2024.measures) +
  scale_color_manual(
    name = "Measures",
    values = c("Mean" = "brown", "Median" = "black")
  )
# Display plot
life.expectancy.across.sexes.2024.plot



### frequency Distributions of under.age.5.mortality.across.sexes.2024 #####

# Filter, select, and reshape the data
under.age.5.mortality.across.sexes.2024 <- census2004_2024 %>%
  filter(Year == 2024) %>% 
  select(c(Under.Age.5.Mortality..Males, Under.Age.5.Mortality..Females, Name)) %>% 
  pivot_longer(
    cols = !Name,
    names_to = "Sex",
    values_to = "Age"
  ) %>%
  mutate(Sex = gsub("\\Under.Age.5.Mortality..", "", Sex))

# Calculate mean and median
under.age.5.mortality.across.sexes.2024.measures <- under.age.5.mortality.across.sexes.2024 %>%
  group_by(Sex) %>%
  summarise(Mean = mean(Age), Median = median(Age)) %>%
  pivot_longer(!Sex, names_to = "Measures", values_to = "Measure_Value")
# Print mean and median values
print("Mean and Median Values:")
print(under.age.5.mortality.across.sexes.2024.measures)

# Plot the histogram with mean and median lines
under.age.5.mortality.across.sexes.2024.plot <- under.age.5.mortality.across.sexes.2024 %>%
  ggplot(aes(Age, color = Sex, fill = Sex)) +
  geom_histogram(alpha=0.8, binwidth = 10, show.legend = FALSE) +
  theme(
    panel.spacing = unit(0.5, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  ylab("Frequency") +
  xlab("Mortality Count") +
  scale_x_continuous(breaks = seq(0, 160, by = 20)) +
  ggtitle("Mortality rate for children under age of 5 out of 1000 live child births") +
  facet_wrap((~Sex)) +
  geom_vline(aes(xintercept = Measure_Value, color = Measures), under.age.5.mortality.across.sexes.2024.measures) +
  scale_color_manual(
    name = "Measures",
    values = c("Mean" = "brown", "Median" = "black")
  )
# Display plot
under.age.5.mortality.across.sexes.2024.plot

#2
### variabilities of the variables###

life.expectancy.europe <- census2004_2024 %>%
  filter(Year == 2024, Region == "Europe") %>%
  select(c(Life.Expectancy.at.Birth..Both.Sexes, Under.Age.5.Mortality..Both.Sexes, Subregion)) %>% 
  pivot_longer(
    cols = !Subregion,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(Variable = gsub("\\..Both.Sexes", "(Both Sexes)", Variable)) %>%
  mutate(Variable = gsub("\\.", " ", Variable))

life.expectancy.europe.plot <- life.expectancy.europe %>% 
  ggplot(aes(x=Subregion, y=Value, fill=Subregion)) +
  geom_boxplot() +
  facet_wrap(~Variable, scale="free")

life.expectancy.europe.plot

census2004_2024 %>% filter(Region == "Europe", Subregion == "Northern Europe") %>% select(Name) %>% distinct()

census2004_2024 %>% select(Region) %>% distinct()

#3

#########  Scatterplot ##############################

pairs(census2004_2024[ census2004_2024$Year == 2024, c(3,6)])
# Create an autumn color palette using colorRampPalette
autumn_colors <- colorRampPalette(c("red","orange", "yellow", "blue", "green"))(length(unique(census2004_2024$Region)))

# Create the pairs plot with color based on Region and using the autumn color palette
pairs(census2004_2024[census2004_2024$Year == 2024, c(3, 6)], 
      col = autumn_colors[as.factor(census2004_2024$Region[census2004_2024$Year == 2024])], 
      pch = 16)


############ Pearson Coefficients #######################

# #compute correlations for several pairs of variables
cor_matrix = cor(census2004_2024[ census2004_2024$Year == 2024, c(3:4,6:7)],method = "pearson")
#correlation matrix presents the correlation coefficients by coloring the coefficients based on their sign
# show only upper side
corrplot(cor_matrix, method = "number",type = "upper",title="Pearson coefficients",mar=c(0,0,1,0))


#4

# Box Plot for Under 5 Age Mortality by Region with region boundary
data_mort_box_region <- melt(census_2024_2004[, c("Name", "Region", "Year", "Under.Age.5.Mortality..Both.Sexes")],
                             id.vars = c("Name", "Region", "Year"),
                             measure.vars = "Under.Age.5.Mortality..Both.Sexes")

ggplot(data = data_mort_box_region, aes(x = factor(Year), y = value, fill = factor(Year))) +
  geom_boxplot() +
  facet_wrap(~ Region) +
  ggtitle("Box Plot of Under 5 Age Mortality for Both Sexes (2004 vs 2024) by Region") +
  labs(x = "Year", y = "Under 5 Age Mortality") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Adds boundary for each facet
    plot.title = element_text(size = 10)
  )

# Box Plot for Life Expectancy at Birth by Region with region boundary
data_life_exp_box_region <- melt(census_2024_2004[, c("Name", "Region", "Year", "Life.Expectancy.at.Birth..Both.Sexes")],
                                 id.vars = c("Name", "Region", "Year"),
                                 measure.vars = "Life.Expectancy.at.Birth..Both.Sexes")

ggplot(data = data_life_exp_box_region, aes(x = factor(Year), y = value, fill = factor(Year))) +
  geom_boxplot() +
  facet_wrap(~ Region) +
  ggtitle("Box Plot of Life Expectancy at Birth for Both Sexes (2004 vs 2024) by Region") +
  labs(x = "Year", y = "Life Expectancy at Birth") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Adds boundary for each facet
    plot.title = element_text(size = 10)
  )

