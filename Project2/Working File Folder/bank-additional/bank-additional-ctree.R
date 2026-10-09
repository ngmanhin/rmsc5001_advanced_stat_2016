start.time<-Sys.time()		# Measure running time
library(rpart)			# load rpart library
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

ctree = rpart(d_train$y~.,data=d_train, 
control=rpart.control(maxdepth=10),method="class")	# run the classification tree
plot(ctree, asp=30, main="Business Success")     # Plot ctree # Please change asp to suitable value
text(ctree, use.n=T, cex = 0.6)		            # Add text   # Please change cex to suitable value
print(ctree)                                          # Write down the classification tree in words

prob=predict(ctree, d_train)                         # Prediction on training data
col=max.col(prob)                           		# Col is the col. no s.t. pr is max
table1 = table(col,d_train[,20])                    	# Cross tabulation table for training dataset new_d
table1

n<-sum(table1)                              # Number of data
misclass_d1<-(n-sum(diag(table1)))/n        # Compute d_train misclassification rate
misclass_d1

probD2=predict(ctree,d_test)                		# Prediction on testing data
col2=max.col(probD2)                        		# Col is the col. no s.t. pr is max
table2 = table(col2,d_test[,20])                  	# Cross tabulation table for training dataset d_test
table2

n<-sum(table2)                              # Number of data
misclass_d2<-(n-sum(diag(table2)))/n        # Compute d_test misclassification rate
misclass_d2

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

