library(MASS)                 # Load MASS library
d<-read.csv("bank-additional-full-PREPROCESSED.csv") # load dataset
d$duration<-NULL
names(d)


set.seed(74020)   # Use student id to generate sample
n = max(row(d))
id = sample(1:n, size = n*9/10)  # generate random row index for d1
d_train = d[id,]		   	  # training dataset
d_test = d[-id,]			  # testing dataset
dim(d_train)
dim(d_test)


lda<-lda(d_train$y~.,data=d_train) # Save result to lda
pred <- predict(lda)$class                  # Compute the predicted class and save it to pred
table1<-table(pred,d_train$y)                   # Classification table
table1
n<-sum(table1)                                               # Number of data
misclass_train<-(n-sum(diag(table1)))/n                      # Compute d1 misclassification rate
misclass_train 

pred_test <- predict(lda,d_test)$class      # Compute the predicted class and save it to pred
table2<-table(pred_test,d_test$y)                  # Classification table
table2
n<-sum(table2)                                               # Number of data
misclass_test<-(n-sum(diag(table2)))/n                      # Compute d1 misclassification rate
misclass_test 
