# title: "Fossil Data Quality and Analysis"
#author: "Niket Choudhary"


q1 <- read.csv(file.choose(), header = T, na.strings = c("NA",""," ",".")) # Load the data from file, after saving xlsx file as csv in Excel.

summary(q1)

# See if our Data has Missing values
sapply(q1, function(x){sum(is.na(x))})

# We have two empty columns, Transaction.Amount.in.USD..Pre.Tax. and Local.Time. We have 2 missing values in Zip, 2685 missing in State.

#Transaction.Code should have unique values, let's check
length(unique(q1$Transaction.Code))

length(unique(q1$Store.Name))

#Well it seems we have repetitions here, we should only consider rows with unique Transaction.Code
q1 <- q1[!duplicated(q1$Transaction.Code),]

#Let's check again
length(unique(q1$Transaction.Code))

#In our analysis we can drop certain columns
q1$Transaction.Amount.in.USD..Pre.Tax. <- NULL
q1$State <- NULL
q1$Zip <- NULL
q1$CN.ZIP..First.4. <- NULL
q1$Local.Time <- NULL
q1$YYYYMMDD <- NULL

#Let us extract name of the month 
q1$Date <- as.Date(q1$Date.and.Time.In.China, format="%m/%d/%Y")

q1$Month_name <- as.factor(format(q1$Date, "%B" ))
q1$Month <- as.factor(format(q1$Date, "%m" ))

#Let's look at the Age distribution
summary(q1$Age)

#Let's classify Age in groups
q1$Age_group <- '65+'
q1$Age_group[q1$Age < 65 & q1$Age >= 55] <- '55-65'
q1$Age_group[q1$Age < 55 & q1$Age >= 45] <- '45-55'
q1$Age_group[q1$Age < 45 & q1$Age >= 35] <- '35-45'
q1$Age_group[q1$Age < 35] <- '21-35'

#Let's look at the Month distribution
table(q1$Month_name)

#Let's classify Month in Quarters
q1$Quarters <- quarters(q1$Date)

#Let's look which gender is purchasing more
agg_gender <- aggregate(Customer.ID ~ Gender , data = q1, length)
agg_gender

#We have more Male customers.

#Total spending per country
agg_amount <- aggregate(Transaction.Amount.in.USD ~ Country , data = q1, sum)
agg_amount <- agg_amount[with(agg_amount,order(-Transaction.Amount.in.USD)),]
agg_amount

library(ggplot2)

ggplot(agg_amount, aes(x = Country, y = Transaction.Amount.in.USD)) + 
geom_bar(aes(fill = Transaction.Amount.in.USD), stat = "identity") +
xlab("Country") + ylab("Total Amount of Transactions") + labs(title = "Total Transactions  per Country", fill="Transaction Amount") +
theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.6))
# geom_text(aes(label = Country), vjust = -1) 

#The 5 biggest market for Fossil are Canada, Germany, USA, Italy, UK

#Let's look in detail at top 5 countries
agg_country <- aggregate(Transaction.Amount.in.USD ~ Country + Quarters + Gender, data = q1, sum)
agg_country <- agg_country[with(agg_country,order(-Transaction.Amount.in.USD)),]
agg_country

Canada <- agg_country[agg_country$Country == "Canada",]
Germany <- agg_country[agg_country$Country == "Germany",]
USA <- agg_country[agg_country$Country == "USA",]
Italy <- agg_country[agg_country$Country == "Italy",]
UK <- agg_country[agg_country$Country == "UK",]

ggcanada <- ggplot(Canada, aes(x = Quarters, y = Transaction.Amount.in.USD, colour = Gender, group = Gender)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 2.8, shape = 21) +
  labs(y = " ", x = " ") + theme(text = element_text(size=20))
#scale_colour_manual(values=c("blue", "red"))  

gggermany <- ggplot(Germany, aes(x = Quarters, y = Transaction.Amount.in.USD, colour = Gender, group = Gender)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 2.8, shape = 21) +
  labs(y = " ", x = " ") + theme(text = element_text(size=20))
# scale_colour_manual(values=c("blue", "red")) 

ggusa <- ggplot(USA, aes(x = Quarters, y = Transaction.Amount.in.USD, colour = Gender, group = Gender)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 2.8, shape = 21) +
  labs(y = " ", x = " ") + theme(text = element_text(size=20))
# scale_colour_manual(values=c("blue", "red")) 

gggitaly <- ggplot(Italy, aes(x = Quarters, y = Transaction.Amount.in.USD, colour = Gender, group = Gender)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 2.8, shape = 21) + 
  labs(y = " ", x = " ") + theme(text = element_text(size=20))
#scale_colour_manual(values=c("blue", "red")) 

gguk <- ggplot(UK, aes(x = Quarters, y = Transaction.Amount.in.USD, colour = Gender, group = Gender)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 2.8, shape = 21) +
  labs(y = " ", x = "Quarters") + theme(text = element_text(size=20))
#scale_colour_manual(values=c("blue", "red"))

#library("gridExtra")
#library("cowplot")
library(ggpubr)

figure <- ggarrange(ggcanada, gggermany, ggusa, gggitaly, gguk, heights = c(8, 8, 8, 8, 8),
                    ncol = 1, nrow = 5, labels = c("CA", "GE", "US", "IT", "UK"), common.legend = TRUE)


annotate_figure(figure, bottom = text_grob("Transactions per Quarter by Gender across Canada, Germany, USA, Italy, UK", color = "black", face = "bold", size = 12.9))

#We can see a general trend of lower transactions in Q4 compared to Q1.

#Now let's look at total spending per City in USA by age group
agg_amount_USA <- aggregate(Transaction.Amount.in.USD[q1$Country == 'USA'] ~ City[q1$Country == 'USA'] + Age_group[q1$Country == 'USA'], data = q1, sum)
agg_amount_USA <- agg_amount_USA[with(agg_amount_USA,order(-`Transaction.Amount.in.USD[q1$Country == "USA"]`)),]
agg_amount_USA <- agg_amount_USA[1:30,]
agg_amount_USA

#Let's plot a graph with the top 30 transactions across major cities
ggplot(agg_amount_USA, aes(x = `City[q1$Country == "USA"]`, y = `Transaction.Amount.in.USD[q1$Country == "USA"]`, fill = `Age_group[q1$Country == "USA"]`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("City") + ylab("Total Amount of Transactions") + labs(title = "Total Transactions  per City across Age Groups in USA",  fill="# Age Group") +
  theme(text = element_text(size = 19), axis.text.x = element_text(angle = 90, vjust = 0.6))

#We observe that Age group 21-35 does most shopping followed by others 

library(ggplot2)
theme_set(theme_classic())

# Plot
g <- ggplot(agg_amount_USA, aes(`Transaction.Amount.in.USD[q1$Country == "USA"]`))

g + geom_density(aes(fill = factor(`Age_group[q1$Country == "USA"]`), y=..scaled..), alpha = 0.5) + 
  coord_cartesian(xlim=c(1200, 10600)) +
  labs(title = "Density plot", 
       subtitle = "Transaction Grouped by Number of Age",
       x = "Transaction",
       fill="# Age Group") 

#Here we can clearly observe that Age group 21-35 has a high Transaction amount.

