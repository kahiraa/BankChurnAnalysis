install.packages('ggplot2')
install.packages('caTools')
install.packages('ggpubr')
install.packages('dplyr')
library(ggplot2)
library(caTools)
library(ggpubr)
library(dplyr)

Churn.data <-read.csv("/Users/kahirakalbhor/Downloads/BankChurnAnalysis/bankdata.csv", header=TRUE)

str(Churn.data)
summary(Churn.data)

df0 <- tibble(Churn.data %>%
               group_by(churn) %>%
               summarise(count=n()))

piepercent<- round(100 * df0$count / sum(df0$count), 1)
colors <- c("#80d4f2", "#1faadb")
pie(df0$count, label = piepercent, main = "Churn Pie Chart", col = colors)
legend("topright", c("Retained", "Churned"), fill = colors)


ggplot(Churn.data, aes(x=age))+
  geom_histogram(color="Gray", fill="#1faadb", binwidth = 3) +
  labs(x= "Age of Customer", y="Count", title="Histogram of Customer Age")
mean(Churn.data$age)

Churn <- as.factor(Churn.data$churn)

ggplot(Churn.data, aes(x=credit_score, fill = Churn))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')

ggplot(Churn.data, aes(x=age, fill = Churn))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')

ggplot(Churn.data, aes(x=balance, fill = Churn))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')

df <- tibble(Churn.data %>%
  group_by(country, churn) %>%
  summarise(count=n()))

ggplot(df,
       aes(x=country,y=count,fill=churn)) + 
  geom_bar(position = 'dodge2', stat = "identity")+
  geom_text(aes(label = count), position=position_dodge2(width=0.9), vjust=-0.25)

df1 <- tibble(Churn.data %>%
               group_by(gender, churn) %>%
               summarise(count=n()))

ggplot(df1,
       aes(x=gender,y=count,fill=churn)) + 
  geom_bar(position = 'dodge2', stat = "identity")+
  geom_text(aes(label = count), position=position_dodge2(width=0.9), vjust=-0.25)

df2 <- tibble(Churn.data %>%
               group_by(credit_card, churn) %>%
               summarise(count=n()))

ggplot(df2,
       aes(x=credit_card,y=count,fill=churn)) + 
  geom_bar(position = 'dodge2', stat = "identity")+
  geom_text(aes(label = count), position=position_dodge2(width=0.9), vjust=-0.25)

df3 <- tibble(Churn.data %>%
                group_by(active_member, churn) %>%
                summarise(count=n()))

ggplot(df3,
       aes(x=active_member,y=count,fill=churn)) + 
  geom_bar(position = 'dodge2', stat = "identity")+
  geom_text(aes(label = count), position=position_dodge2(width=0.9), vjust=-0.25)

df4 <- tibble(Churn.data %>%
                group_by(products_number, churn) %>%
                summarise(count=n())) 

ggplot(df4,
       aes(x=products_number,y=count,fill=churn)) + 
  geom_bar(position = 'dodge2', stat = "identity")+
  geom_text(aes(label = count), position=position_dodge2(width=0.9), vjust=-0.25)

df5 <- tibble(Churn.data %>%
                group_by(tenure, churn) %>%
                summarise(count=n())) 

ggplot(df5,
       aes(x=tenure,y=count,fill=churn)) + 
  geom_bar(position = 'dodge2', stat = "identity")+
  geom_text(aes(label = count), position=position_dodge2(width=0.9), vjust=-0.25)

df6 <- tibble(Churn.data %>%
              group_by(estimated_salary, credit_score, churn) %>%
              summarise())

ggplot(df6, aes(x=estimated_salary, y=credit_score)) + 
  geom_point(aes(colour=churn, fill=churn))

df7 <- tibble(Churn.data %>%
                group_by(estimated_salary, age, churn) %>%
                summarise())

ggplot(df7, aes(x=age, y=estimated_salary)) + 
  geom_point(aes(colour=churn, fill=churn))

