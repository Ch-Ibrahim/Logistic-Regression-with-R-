# Logistic regression 

#Read and load data 
Mydata <- read.csv(file.choose(),header = TRUE)
str(Mydata)
dim(Mydata)
summary(Mydata)

#Change Variables from integer to Categorial 

Mydata$admit <- as.factor(Mydata$admit)
Mydata$rank <- as.factor(Mydata$rank)

# 2-way table of factor varaibles to check that there is ant cell with zero values 

 xtabs(~Mydata$admit + Mydata$rank, data = Mydata)
 
 # Data Partition traing set (80 %) test set (20 %)
 
 set.seed(1234)
 Part <- sample(2,nrow(Mydata), replace = T, prob = c(0.8,0.2))
 
 Trainset <- Mydata[Part == 1,]
 Testset <- Mydata[Part == 2 ,]
 
 # Logistic regression Model 
 # we noticed that gre is no significant 
 Mymodel <- glm(admit ~  rank + gpa, data = Trainset , family = "binomial" )
 summary(Mymodel)
 
 # prediction 
 
pred <- predict(Mymodel, Trainset, type = 'response') 
head(pred)
head(Trainset)


# Misclassification error ,in  Trainset 

Missclass <- ifelse(pred > 0.5, 1, 0)
Tab  <- table(Predicted = Missclass, Actual = Trainset$admit)
Tab
 
1-sum(diag(Tab))/sum(Tab)


pred1 <- predict(Mymodel, Testset, type = 'response')

Missclass1 <- ifelse(pred1 > 0.5, 1, 0)
Tab1  <- table(Predicted = Missclass1, Actual = Testset$admit)
Tab1
1-sum(diag(Tab1))/sum(Tab1)

