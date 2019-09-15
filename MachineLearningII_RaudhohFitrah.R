#Title  : Classification with Decision Tree and Random Forest
#Author : Raudhoh Fitra Humamy

#install.packages("rpart")
#install.packages("randomForest")

#load necessary package
library(rpart) #decision tree
library(randomForest) #random forest
library(caret) #classification and regression training

#load dataset from github
df <- read.csv("https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/HR_comma_sep.csv");

head(df)

#check null value
summary(df)


#metadata
#satisfaction_level : employee satisfaction rate
#last_evaluation : last evaluation value
#number_project : number of working project
#average_montly_hours : average working hours per month
#time_spend_company : time have been on the company
#Work_accident : number of work accident
#left : number of resign employee
#promotion_last_5years : work promotion status
#sales : name of department
#salary : salary category


unique(df$left)

df2 <- dummyVars(~ ., data=df)

df2 <- data.frame(predict(df2, newdata=df))

head(df2)

summary(df2)


sales <- c()
i = 1
for(job in df$sales){
  sales[[i]]=ifelse(job=="sales",1,
         ifelse(job=="accounting",2,
                ifelse(job=="hr",3,
                       ifelse(job=="technical",4,
                              ifelse(job=="management",5,
                                     ifelse(job=="IT",6,
                                            ifelse(job=="product_mng",7,
                                                   ifelse(job=="marketing",8,
                                                          ifelse(job=="support",9,10)))))))));
    i = i+1;
}

salaries <- c()
i = 1
for(sal in df$salary){
  salaries[[i]] = ifelse(sal=="low", 1,ifelse(sal=="medium",2,3))
  i = i+1
}


df$sales <- sales
df$salaries <- salaries


#split into train and test

idx <-  sample(1:nrow(df),as.integer(0.8*nrow(df)))
train_df <- df[idx,]
test_df <- df[-idx,]

#inspect number of row
nrow(train_df)
nrow(test_df)


head(df)

#classification with decision tree

tree <- rpart(left ~ ., 
              data = data.frame(train_df), method = "class")

classif_dt <- predict(tree, data.frame(test_df), type="class")
classiftable_dt <- table(test_df[,'left'],predict(tree, data.frame(test_df),type="class"))


#random forest
randomFor <- randomForest(left ~ ., data = data.frame(train_df), ntree=10000, importance = TRUE)
predicted_rf<- predict(randomFor, data.frame(test_df),type="class")
pretable_rf <- table(test_df[,'salary'],predict(tree, data.frame(test_df), type="class"))


#measure accuracy
accurary_dt <- sum(diag(pretable_dt))/sum(pretable_dt)
accurary_dt <- sum(diag(pretable_rf))/sum(pretable_rf)

#confusion matrix 
cm_dt <- confusionMatrix(predicted_dt, test_df$salary)
cm_rf <- confusionMatrix(predicted_rf, test_df$salary)


