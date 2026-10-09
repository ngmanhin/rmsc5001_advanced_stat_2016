start.time <- Sys.time()

library(nnet)                 # Load nnet library
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


# ANN
ann<-function(x,y,size,minsize=1, maxsize=10, maxit=100,linout=F,try=5) {               # build the improved ann function
for(j in minsize:maxsize){  
	ann1<-nnet(y~.,data=x,size=j,maxit=maxit,linout=linout)      # save the first result
	  v1<-ann1$value                                                  # save the first value  

	   for (i in 2:try) {
	   	  ann<-nnet(y~.,data=x,size=j,maxit=maxit,linout=linout)   # i-th trial of ann
	   	  if (ann$value<v1) {                                         # if the record is better then ever, save it
	   	  	v1<-ann$value                                           # save the best value
	   	  	ann1<-ann                                               # save the best result
	   	  }
	   }
	if (j == minsize){                                                  
		v2<-v1                                                         # save the first value when j = minsize
		ann2<-ann1                                                     # save the first result when j = minsize
	}
	if(v1<v2){
		v2<-v1
		ann2<-ann1                                                      # return the best result
	}
}
ann2
}

final_ann <- ann(d_train[,1:19],d_train[,20], minsize = 1, maxsize = 18, linout=F)  # Run ANN, selecting best node size from 1 to 18 corresponding to testing data 
ann_value <- final_ann$value

summary(final_ann)
                                                             # Show summary of final_ann
pred<-1*(final_ann$fit>1/2)
table1<-table(pred,d_train$y)  
table1
n<-sum(table1)                                               # Number of data
misclass_train<-(n-sum(diag(table1)))/n                      # Compute d_train misclassification rate
misclass_train                                                       # Show cross tabulate table for training dataset

pr<-predict(final_ann, d_test)
pred<-1*(pr>1/2)
table2<-table(pred, d_test$y)
table2
n<-sum(table2)                                               # Number of data
misclass_test<-(n-sum(diag(table2)))/n                      # Compute d_test misclassification rate
misclass_test 

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
