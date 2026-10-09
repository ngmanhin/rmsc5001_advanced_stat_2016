start.time <- Sys.time()

d<-read.csv("bank-additional-full-PREPROCESSED.csv") # load dataset
d$duration<-NULL
names(d)
                    

set.seed(74020+82860+74011+71533)   # Use student id to generate sample
n = max(row(d))
id = sample(1:n, size = n*9/10)  # generate random row index for d_train
d_train = d[id,]		   	  # training dataset
d_test = d[-id,]			  # testing dataset
dim(d_train)
dim(d_test)

lreg<-glm(d_train$y ~ ., data = d_train, binomial) # save logstic reg
summary(lreg)
lreg<-step(lreg)								  # perform stepwise selection
summary(lreg)
readline("Hit <Return> to continue:")


c1 <- predict(lreg, d_train)                 # Prediction on Training Data Set
pred<-exp(c1)/(1+exp(c1))
pred<-round((pred>0.5))
table1<-table(pred,d_train$y) 
table1                                      # Display training data classification table
n<-sum(table1)                              # Number of data
misclass_train<-(n-sum(diag(table1)))/n        # Compute training misclassification rate
misclass_train


c2 <- predict(lreg, d_test)			  # Prediction on testing dataset
pred<-exp(c2)/(1+exp(c2))
pred<-round((pred>0.5))
table2<-table(pred,d_test$y)
table2                                      # Display testing data classification table
n<-sum(table2)                              # Number of data
misclass_test<-(n-sum(diag(table2)))/n        # Compute testing misclassification rate
misclass_test

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

