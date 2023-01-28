getwd()
setwd(readline())
C:\DirveD\Data-Scinece\8-ML in R\Session2\linear_Regression

#loading dataset
dir()
df  <-  read.csv("Linear_Regression.csv")

### Data Exploration 
#1) General Overview
str(df)
summary(df)

#2) Missing Values
sum(is.null(df))

#3) Factoring the character Feuture
table(df$sex)

# colnames(df)       ==> showing datafram column's name
# sapply(df, class)  ==> showing type of each column

# colnames(df[sapply(df, class)=="character"]) ==> anja iy ke dar dataframe ma, sotoon ha "char" hastan
#sex
df$sex <- factor(x = df$sex , levels = c("male", "female"),labels = c("Mard" , "zan"))
library(ggplot2)
ggplot(data = df, aes(x =sex)) +
  geom_bar()

#smoker
table(df$smoker)
df$smoker <- factor(x = df$smoker , levels = c("yes" , "no"))
ggplot(data = df , mapping = aes(x=smoker)) +
  geom_bar(col= "yellow" ,fill = "brown")

#region
table(df$region)
df$region <- factor(x = df$region , c("northeast", 'northwest', "southeast", "southwest"))
ggplot(data = df,mapping = aes(x=region))+
  geom_bar()




ggplot(data = df,mapping = aes(x=expenses)) +
  geom_histogram(bins = 50, col ="red" , fill= "blue")+
  theme_bw()
 
ggplot(df,mapping = aes(x=age , y =expenses)) + 
  geom_point(col="blue" , alpha = 0.5)

ggplot(df,mapping = aes(x=children , y =expenses)) + 
  geom_point(col="blue" , alpha = 0.5)

ggplot(df,mapping = aes(x=bmi , y =expenses)) + 
  geom_point(col="blue" , alpha = 0.5)

ggplot(df,mapping = aes(x=smoker , y =expenses)) + 
  geom_point(col="blue" , alpha = 0.5)

### splliting data

library(caTools)
set.seed(0)
Target <- df$expenses

sample1 <- sample.split(Y = Target, SplitRatio = .80)


train <- subset(x = df, sample1 ==TRUE)
test <- subset(x= df, sample1==FALSE)


##modeling
attach(df) 
ins_model <- lm(expenses~ age + children + bmi + sex + smoker + region ,data = train)
summary(ins_model)

model2 <- lm(expenses~ age + (age^2) + children + bmi + bmi*smoker + sex + smoker + region ,data = train)
summary(model2)


################ 2#####################


#loading dataset
df1  <-  read.csv("Linear_Regression.csv" , stringsAsFactors = TRUE)

df1$age2 <- df1$age^2
df1$bmi30 <- ifelse(df1$bmi>= 30 , 1 , 0)
### splliting data

library(caTools)
set.seed(0)
Target1 <- df1$expenses

sample2 <- sample.split(Y = Target1, SplitRatio = .80)


train_df1 <- subset(x = df1, sample2 ==TRUE)
test_df1 <- subset(x= df1, sample2==FALSE)


##modeling
attach(df1) 
ins_model_df1 <- lm(expenses~ age +age2 + children + bmi + sex + smoker*bmi30 + region ,data = train_df1)
summary(ins_model_df1)

ins_model_df2 <- lm(expenses~ age2 + children + bmi + sex + smoker*bmi30 + region ,data = train_df1)
summary(ins_model_df2)


test_df1$predict <- predict(ins_model_df2, test_df1)
cor(test_df1$predict, test_df1$expenses)

ggplot(data = test_df1,mapping = aes(x=predict ,y = expenses)) +
  geom_point() + geom_abline(a=0 ,b=1 ,col="red" , lty=2)
 

########## Use model for real case
new_Person <- data.frame(age = 30 , age2= 30^2 ,children=2,
                         bmi= 30 , bmi30=1,sex ="male",
                         smoker="no", region="northwest")

predict(ins_model_df2,new_Person)
