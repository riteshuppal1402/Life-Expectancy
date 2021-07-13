library("dplyr")
library("ggpubr")

#LOAD DATA
data <- read.csv("Life.csv")
head(data)

#Q1
Developing_X <- data %>% filter(Status == "Developing") %>% group_by(Country) %>% summarise(Average_life = mean(Life_expectancy))
Developed_Y <- data %>% filter(Status == "Developed") %>% group_by(Country) %>% summarise(Average_life = mean(Life_expectancy))

var.test(Developed_Y$Average_life,Developing_X$Average_life)
 
t.test(Developed_Y$Average_life,Developing_X$Average_life, alternative = "greater", var.equal=FALSE)

#Q2

data3<- data %>% group_by(Country) %>% summarise(Average_life = mean(Life_expectancy), Average_schooling = mean(Schooling))

x<- data3 %>% filter( Average_schooling <=8.0)
y<- data3 %>% filter( Average_schooling >8.0 & Average_schooling <=12.0)
z<- data3 %>% filter( Average_schooling >12.0)
y1<-data.frame(Average_life = x$Average_life)
y1$Education ='Low'
y2<-data.frame(Average_life= y$Average_life)
y2$Education ='Middle'
y3<-data.frame(Average_life= z$Average_life)
y3$Education ='High'
combined_g <-data.frame(rbind(y1,y2,y3))
combined_g

Anova_Results <- aov(Average_life ~ Education, data= combined_g)
summary(Anova_Results)

#Q3

data1 <- data %>% group_by(Country) %>% summarise(Average_life = mean(Life_expectancy), Average_HDI = mean(Income_composition_of_resources))
data1
ggscatter(data1, x = "Average_HDI", y = "Average_life", add = "reg.line",
          conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "HDI (Income composition of resources)", ylab = "Life_Expectancy")

cor.test(data1$Average_HDI, data1$Average_life)

#Q4

data2 <- data %>% filter(Country == "India")
X <- select(data2, Total_expenditure)
t.test(X, mu = 5.2, alternative = "two.sided")

#Q5

data4 <- data %>% group_by(Country) %>% summarise(Average_life = mean(Life_expectancy), Average_infant_deaths = mean(infant_deaths), Average_underfive_deaths = mean(under_five_deaths))

countx <- ceiling(mean(data4$Average_infant_deaths))
county <- ceiling(mean(data4$Average_underfive_deaths))
arg1 <- c(countx, county)
arg2 <- c(1000,1000)
prop.test(arg1,arg2, correct =FALSE) 

#Q6

data7 <- data %>% group_by(Country) %>% summarise(Average_Life_Expectancy= mean(Life_expectancy), Average_alcohol = mean(Alcohol))
ggscatter(data7, x = "Average_alcohol", y = "Average_Life_Expectancy", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol)", 
	    ylab = "Life Expectancy (Average)")

cor.test(data7$Average_alcohol, data7$Average_Life_Expectancy)

data6 <- data %>% group_by(Country) %>% summarise(Average_Adult_Mortality= mean(Adult_Mortality), Average_alcohol = mean(Alcohol))
ggscatter(data6, x = "Average_alcohol", y = "Average_Adult_Mortality", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Alcohol, recorded per capita (15+) 
	    consumption (in litres of pure alcohol)", ylab = "Adult Mortality Rate")

cor.test(data6$Average_alcohol, data6$Average_Adult_Mortality, method ='pearson')

#Q7

hist(data$Life_expectancy, # histogram
 col="lightblue", # column color
 border="black",
 prob = TRUE, # show densities instead of frequencies
 ylim =c(0,0.07),
 xlab = "Life Expectancy (Years)",
 main = "Life Expectancy (2000-2015)")

lines(density(data$Life_expectancy), # density plot
 lwd = 2, # thickness of line
 col = "black")

#Q8

#LINEAR REGRESSION
names(data)
library(caTools)
set.seed(123)
data1<- subset(data,select =-c(Country, Status))
split = sample.split(data1$Life_expectancy, SplitRatio = 0.8)

training_set = subset(data1, split == TRUE)
test_set = subset(data1, split == FALSE)

training_set = scale(training_set)
test_set = scale(test_set)

regressor = lm(formula = Life_expectancy ~ .,data = data.frame(training_set))
summary(regressor)
y_pred = predict(regressor, newdata = data.frame(test_set))

#Q9

data5<- data %>% group_by(Country) %>% summarise(Average_life = mean(Life_expectancy), Average_Polio = mean(Polio), Average_Diphtheria = mean(Diphtheria))
x1<- data5 %>% filter( Average_Polio <=85)
x2<- data5 %>% filter( Average_Polio >85)
y1<- data5 %>% filter( Average_Diphtheria <=85)
y2<- data5 %>% filter( Average_Diphtheria >85) 
a1<-data.frame(Average_life = x1$Average_life, Country  = x1$Country)
a1$Polio ='Low'
a2<-data.frame(Average_life= x2$Average_life, Country  = x2$Country)
a2$Polio ='High'
df1 <-data.frame(rbind(a1,a2))


b1<-data.frame(Average_life = y1$Average_life, Country  = y1$Country)
b1$Diphtheria ='Low'
b2<-data.frame(Average_life= y2$Average_life, Country  = y2$Country)
b2$Diphtheria ='High'
df2 <-data.frame(rbind(b1,b2))

df <- merge(df1,df2, by = "Country")

Anova_Results <- aov(Average_life.x ~ Polio + Diphtheria, data= df)
summary(Anova_Results)













