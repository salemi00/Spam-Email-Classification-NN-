#DATA630 9042
#Assignment 4
#By: Sina Alemi

library("neuralnet")

#This projects uses a dataset of email charactersitics to classify emails as spam or not-spam. 

#dataframe creation
setwd("D:/school/DATA630/module 5")
phish<-read.csv(file='phishing.csv',head=TRUE, sep=",")
str(phish)
summary(phish)


#scaling the input variables (first 30)
phish[1:30]<-scale(phish[1:30])
#converting Result values of '-1' to '0'
phish$Result[phish$Result=='-1']<-0


#make sure that the result is reproducible
set.seed(12345)
#split the data into a training and test set
ind <- sample(2, nrow(phish), replace = TRUE, prob = c(0.7, 0.3))
train.data <- phish[ind == 1, ]
test.data <- phish[ind == 2, ]


#building the model
myformula= (Result~having_IP_Address+URL_Length+Shortining_Service+having_At_Symbol+double_slash_redirecting
            +Prefix_Suffix+having_Sub_Domain+SSLfinal_State+Domain_registeration_length+Favicon+port+HTTPS_token+Request_URL
            +URL_of_Anchor+Links_in_tags+SFH+Submitting_to_email+Abnormal_URL+Redirect+on_mouseover+RightClick+popUpWidnow
            +Iframe+age_of_domain+DNSRecord+web_traffic+Page_Rank+Google_Index+Links_pointing_to_page+Statistical_report)

nn<-neuralnet(formula = myformula, data = train.data, hidden=2, err.fct="ce", linear.output = FALSE, threshold= 0.1, lifesign = 'full')
nn

#Run the commands to display the network properties
nn$call
nn$response[1:15]
nn$covariate[1:10,1:5]
nn$model.list
nn$net.result[[1]][1:15]
nn$weights
nn$startweights
nn$result.matrix

#visualization
plot(x=nn,  col.intercept = 'blue', col.hidden.synapse = 'green', col.out.synapse='red', 
     fontsize = 10, dimension = 10, show.weights=F)

#Model Evaluation
#obtaining predicted values 
mypredict<-compute(nn, nn$covariate)$net.result
#rounding results to nearest integer
mypredict<-apply(mypredict, c(1), round)

#confusion matrix for the training set
mytable<-table(mypredict, train.data$Result, dnn =c("Predicted", "Actual"))
mytable
#Classification Accuracy %
mean(mypredict==train.data$Result)
#mosaic plot of train data evaluation
mosaicplot(mytable, train.data$Result,
           shade=TRUE, xlab="Actual Values", ylab= "Predicted Values", main='Train Data Classification Accuracy')

# confusion matrix for the test set
testPred <- compute(nn, test.data[, 0:30])$net.result
testPred<-apply(testPred, c(1), round)
testtable<-table(testPred, test.data$Result, dnn =c("Predicted", "Actual"))
testtable
mean(testPred==test.data$Result)
mosaicplot(testtable, test.data$Result,
           shade=TRUE, xlab="Actual Values", ylab= "Predicted Values", main='Test Data Classification Accuracy')
