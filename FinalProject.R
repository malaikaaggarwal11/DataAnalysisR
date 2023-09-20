#Loading Packages:
library(tidyverse)
library(readxl)
library(ggplot2)
library(tidyr)
library(plm) #install package plm for panal data: install.package("plm")
library(dplyr)

#Loading the Data
Lifedata <- read_excel("/Users/malaikaaggarwal/Desktop/Life.xlsx")
names(Lifedata)[5:13] <- c("infantdeaths", "Alcohol1", "expenditureper", "HepatitsB", "Measles", "BMI", "Under5deaths", "polio", "totexp")
#Getting familiar with my dataset
head(Lifedata)
tail(Lifedata)
glimpse(Lifedata)
str(Lifedata) #the varibles are saved in the right format
colnames(Lifedata)

#checking for duplicate values in the dataset
duplicat_rows <- duplicated(Lifedata)
Lifedata[duplicat_rows, ] #there are no duplicates

#to check for missing values, I find various missing values in a lot of varibles, so i drop them by creating a new tibble. 
colSums(is.na(Lifedata))
Lifedata_full <- na.omit(Lifedata)
colSums(is.na(Lifedata_full))

summary(Lifedata_full)
#Making a correlation matrix to check for regression analysis. 
data_for_cor <- Lifedata_full |>
  select(-c(Country, Status))

cor_matrix <- cor(as.matrix(data_for_cor))
#Making new columns for GDP and Population for analysis. 
Lifedata_full <- Lifedata_full |>
  mutate(log_GDP = log(GDP))|>
  mutate(log_pop = log(Population))

#making the dataset longer to plot 
d_long <- pivot_longer(Lifedata_full, cols = c("LifeExpectancy", "infantdeaths", "Under5deaths", "Schooling", "log_GDP"), names_to = "Variable", values_to = "Value")

# Plotting the distribution
graph1<- ggplot(d_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("graph1.png", graph1)

d_long2 <- pivot_longer(Lifedata_full, cols = c("Alcohol1", "HepatitsB", "polio"), names_to = "Variable", values_to = "Value")

graph2<- ggplot(d_long2, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,500)) +
  theme(legend.position = "none")
ggsave("graph2.png", graph2)

#Now i tried to plot the first 10 countries with the max life Expectancy rate
top10 <- Lifedata_full |>
  filter(Year == 2014) |>
  group_by(Country) |>
  summarize(LifeExpectancy = max(LifeExpectancy)) |>
  arrange(desc(LifeExpectancy)) |>
  head(10) |>
  pull(Country)

#box plot for Life Expectancy variable for top 10 countries
graph6 <- ggplot(Lifedata_full %>% filter(Country %in% top10), aes(x = Country, y = LifeExpectancy)) +
  geom_boxplot() +
  labs(x = "Country", y = "Life Expectancy") +
  theme_minimal() +
  ggtitle("Countries with the most Life expectancy")
ggsave("graph6.png", graph6)

bottom10 <- Lifedata_full |>
  filter(Year == 2014) |>
  group_by(Country) |>
  summarize(LifeExpectancy = min(LifeExpectancy)) |>
  arrange(desc(LifeExpectancy)) |>
  tail(10) |>
  pull(Country)

graph7<- ggplot(Lifedata_full %>% filter(Country %in% bottom10), aes(x = Country, y = LifeExpectancy)) +
  geom_boxplot() +
  labs(x = "Country", y = "Life Expectancy") +
  theme_minimal()+
  ggtitle("Countries with the least Life expectancy")
ggsave("graph7.png", graph7)

colnames(Lifedata_full)
#Regressions:
model <- lm(LifeExpectancy ~ Alcohol1 + expenditureper + Measles + Under5deaths + polio + totexp, data = Lifedata_full)
model2 <-  lm(LifeExpectancy ~ Alcohol1 + expenditureper + log_GDP  + Under5deaths + log_pop + totexp, data = Lifedata_full)
model3 <- lm(LifeExpectancy ~ Schooling + log_pop + log_GDP, data = Lifedata_full)
summary(model3)$r.squared
#for panel regression with fixed effects: 
panel_data <- pdata.frame(Lifedata_full)
model4 <- plm(LifeExpectancy ~ Schooling + log_pop + log_GDP,
                data = panel_data,
                index = c("Country", "Year"),
                model = "within")
summary(model4)$r.squared
#Graphs:
#This plot is for the entire Dataset showing the relationship between GDP and Life expectancy.
graph3 <- ggplot(data=Lifedata_full, aes(x=log_GDP, y=LifeExpectancy)) + 
  geom_point(color= "green") +
  geom_smooth() +
  guides(color=FALSE)+
  ggtitle("Life Expectancy vs. GDP") +
  xlab("GDP") + ylab("Life Expectancy")
ggsave("graph3.png", graph3)
  

#I wanted to see the scatter plot for one year for a better visual. 
data_2014 <- Lifedata_full |>
  filter(`Year` == 2014)
  
graph4 <- ggplot(data=data_2014, aes(x=log_GDP, y=LifeExpectancy)) + 
  geom_point(aes(size=Population), color="blue") +
  ggtitle("Life Expectancy vs. GDP") +
  xlab("log GDP") + ylab("Life Expectancy") +
  guides(color=FALSE, size=FALSE) +
  geom_smooth()
ggsave("graph4.png", graph4)
## We can see a positive upward trend between GDP and Life Expectancy, which 
#is consistent with the hypothesis. 

#This graph shows the relationship between Schooling and Life Expectancy. 
graph5 <- ggplot(data=Lifedata_full, aes(x=Schooling, y=LifeExpectancy)) + 
  geom_point(color= "purple") +
  geom_smooth() +
  ggtitle("Life Expectancy vs. GDP") +
  xlab("GDP") + ylab("Life Expectancy") +
  guides(color=FALSE)
ggsave("graph5.png", graph5)

graph8 <- ggplot(data=Lifedata_full, aes(x=polio, y=LifeExpectancy)) + 
  geom_point(color= "red") +
  geom_smooth() +
  ggtitle("Life Expectancy vs. Polio") +
  xlab("Polio") + ylab("Life Expectancy") +
  guides(color=FALSE)
ggsave("graph8.png", graph8)                     

graph9 <- ggplot(data=Lifedata_full, aes(x=log_pop, y=LifeExpectancy)) + 
  geom_point(color= "green") +
  geom_smooth() +
  ggtitle("Life Expectancy vs. pop") +
  xlab("pop") + ylab("Life Expectancy") +
  guides(color=FALSE)
ggsave("graph9.png", graph9)
