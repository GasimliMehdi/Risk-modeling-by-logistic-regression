data <- read.csv("C:/Users/mehdi/Desktop/credit_risk_dataset.csv")
View(data)
summary(data)


hist(data$person_age,breaks=10,xlab = "Age", 
     main = "Histogram Age")
hist(data$person_income,breaks=10,xlab = "Income", 
     main = "Histogram Income")
hist(data$person_emp_length,breaks=10,xlab = "Employment Lengths", 
     main = "Histogram Employment Lengths")
hist(data$loan_amnt,breaks=10,xlab = "Loan amount", 
     main = "Histogram Loan Amount")
hist(data$loan_int_rate,breaks=10,xlab = "Loan Interest Rate", 
     main = "Histogram Interest Rate")
hist(data$loan_percent_income,breaks=10,xlab = "Loan Percent Income", 
     main = "Histogram Loan Percent Income")
hist(data$cb_person_cred_hist_length,breaks=10,xlab = "Credit History Lengths", 
     main = "Histogram Credit History Lengths")



plot(data$person_age, data$person_income, xlab = "Age", ylab = "Income")
plot(data$person_age, xlab = "Age")
boxplot(data$person_age, main="Boxplot Age", xlab="Person_Age", ylab="Value")
data <- data[data$person_age <= 40, ]
boxplot(data$person_age, main="Boxplot Age", xlab="Person_Age", ylab="Value")
plot(data$person_age, xlab = "Age")



plot(data$loan_int_rate,data$loan_percent_income, xlab = "Interest Rate", ylab = "Loan Percent Income")
plot(data$loan_percent_income, ylab = "Loan Percent Income")
boxplot(data$loan_percent_income, main="Boxplot Loan Percent Income", xlab="Loan Percent Income", ylab="Value")
data <- data[data$loan_percent_income <= 0.5, ]
plot(data$loan_percent_income, ylab = "Loan Percent Income")
boxplot(data$loan_percent_income, main="Boxplot Loan Percent Income", xlab="Loan Percent Income", ylab="Value")
data <- data[data$loan_percent_income <= 0.45, ]
plot(data$loan_percent_income, ylab = "Loan Percent Income")
boxplot(data$loan_percent_income, main="Boxplot Loan Percent Income", xlab="Loan Percent Income", ylab="Value")



options(scipen = 999)
plot(data$person_income, ylab = "Income")
boxplot(data$person_income, main="Boxplot Person Income", xlab="Person Income", ylab="Value")
data <- data[data$person_income <= 1000000, ]
plot(data$person_income, ylab = "Income")
boxplot(data$person_income, main="Boxplot Person Income", xlab="Person Income", ylab="Value")
data <- data[data$person_income <= 200000, ]
plot(data$person_income, ylab = "Income")
boxplot(data$person_income, main="Boxplot Person Income", xlab="Person Income", ylab="Value")
data <- data[data$person_income <= 140000, ]
plot(data$person_income, ylab = "Income")
boxplot(data$person_income, main="Boxplot Person Income", xlab="Person Income", ylab="Value")
data <- data[data$person_income <= 130000, ]
plot(data$person_income, ylab = "Income")
boxplot(data$person_income, main="Boxplot Person Income", xlab="Person Income", ylab="Value")



plot(data$loan_int_rate, ylab = "Interest Rate")
boxplot(data$loan_int_rate, main="Boxplot Interest Rate", xlab="Interest Rate", ylab="Value")
data <- data[data$loan_int_rate <= 20, ]
plot(data$loan_int_rate, ylab = "Interest Rate")
boxplot(data$loan_int_rate, main="Boxplot Interest Rate", xlab="Interest Rate", ylab="Value")



plot(data$cb_person_cred_hist_length, ylab = "Credit History Lengths")
boxplot(data$cb_person_cred_hist_length, main="Boxplot Person Credit History Lengths", xlab="Credit History Lengths", ylab="Value")
data <- data[data$cb_person_cred_hist_length <= 13, ]
plot(data$cb_person_cred_hist_length, ylab = "Credit History Lengths")
boxplot(data$cb_person_cred_hist_length, main="Boxplot Person Credit History Lengths", xlab="Credit History Lengths", ylab="Value")


boxplot(data$loan_amnt, main="Boxplot Loan Amount", xlab="Loan Amount", ylab="Value")
data <- data[data$loan_amnt <= 23000, ]
boxplot(data$loan_amnt, main="Boxplot Loan Amount", xlab="Loan Amount", ylab="Value")
data <- data[data$loan_amnt <= 20000, ]
boxplot(data$loan_amnt, main="Boxplot Loan Amount", xlab="Loan Amount", ylab="Value")


boxplot(data$person_emp_length, main="Boxplot Person Employment Lengths", xlab="Employment Lengths", ylab="Value")
data <- data[data$person_emp_length <= 15, ]
boxplot(data$person_emp_length, main="Boxplot Person Employment Lengths", xlab="Employment Lengths", ylab="Value")



summary(data)
sum(is.na(data$loan_grade))
data <- na.omit(data)


summary(data)



set.seed(333)
index_train <- sample(1:nrow(data), 0.7 * nrow(data))
training_set <- data[index_train, ]
test_set <- data[-index_train, ]

library(glm)


model_3_logit <- glm(loan_status ~ person_income  + loan_grade +
                           person_home_ownership  , family = "binomial", data = training_set)
model_3_logit
summary(model_3_logit)



model_5_logit <- glm(loan_status ~ person_income  + loan_grade + loan_amnt +person_income
                       +person_home_ownership  , family = "binomial", data = training_set)
model_5_logit
summary(model_5_logit)




model_3_probit <- glm(loan_status ~ person_income  + loan_grade +
                       person_home_ownership  , family = binomial(link = "probit"), data = training_set)
model_3_probit
summary(model_3_probit)



model_5_probit <- glm(loan_status ~ person_income  + loan_grade + loan_amnt + person_income
                     +person_home_ownership  , family = binomial(link = "probit"), data = training_set)
model_5_probit
summary(model_5_probit)




model_3_clog <- glm(loan_status ~ person_income  + loan_grade +
                              person_home_ownership  , family = binomial(link = "cloglog"), data = training_set)
model_3_clog
summary(model_3_clog)



model_5_clog <- glm(loan_status ~ person_income  + loan_grade + loan_amnt + person_income
                      +person_home_ownership  , family = binomial(link = "cloglog"), data = training_set)
model_5_clog
summary(model_5_clog)


model_7_probit <- glm(loan_status ~ person_income  + loan_grade + loan_amnt + person_income
                      +person_home_ownership+loan_intent+person_emp_length  , family = binomial(link = "probit"), data = training_set)
model_7_probit
summary(model_7_probit)



model_7_clog <- glm(loan_status ~ person_income  + loan_grade + loan_amnt + person_income
                      +person_home_ownership+loan_intent+person_emp_length  , family = binomial(link = "cloglog"), data = training_set)
model_7_clog
summary(model_7_clog)



model_7_logit <- glm(loan_status ~ person_income  + loan_grade + loan_amnt + person_income
                      +person_home_ownership+loan_intent+person_emp_length  , family = binomial(link = "logit"), data = training_set)
model_7_logit
summary(model_7_logit)





model_8_probit <- glm(loan_status ~ person_income  + loan_grade + loan_amnt + person_income
                      +person_home_ownership+loan_intent+person_emp_length+cb_person_cred_hist_length  , family = binomial(link = "probit"), data = training_set)
model_8_probit
summary(model_8_probit)



model_8_clog <- glm(loan_status ~ person_income  + loan_grade + loan_amnt + person_income
                                             +person_home_ownership+loan_intent+person_emp_length+cb_person_cred_hist_length  , family = binomial(link = "cloglog"), data = training_set)
model_8_clog
summary(model_8_clog)



model_8_logit <- glm(loan_status ~ person_income  + loan_grade + loan_amnt + person_income
                                             +person_home_ownership+loan_intent+person_emp_length+cb_person_cred_hist_length  , family = binomial(link = "logit"), data = training_set)
model_8_logit
summary(model_8_logit)




predictions_model_3_logit <- predict(model_3_logit, newdata = test_set, type = "response")
predictions_model_3_probit <- predict(model_3_probit, newdata = test_set, type = "response")
predictions_model_3_clog <- predict(model_3_clog, newdata = test_set, type = "response")
predictions_model_5_logit <- predict(model_5_logit, newdata = test_set, type = "response")
predictions_model_5_probit <- predict(model_5_probit, newdata = test_set, type = "response")
predictions_model_5_clog <- predict(model_5_clog, newdata = test_set, type = "response")
predictions_model_7_probit <- predict(model_7_probit, newdata = test_set, type = "response")
predictions_model_7_logit <- predict(model_7_logit, newdata = test_set, type = "response")
predictions_model_7_clog <- predict(model_7_clog, newdata = test_set, type = "response")
predictions_model_8_probit <- predict(model_8_probit, newdata = test_set, type = "response")
predictions_model_8_logit <- predict(model_8_logit, newdata = test_set, type = "response")
predictions_model_8_clog <- predict(model_8_clog, newdata = test_set, type = "response")


install.packages("proc")
library(proc)


ROC_3_logit <- roc(test_set$loan_status,predictions_model_3_logit)
ROC_3_probit <- roc(test_set$loan_status,predictions_model_3_probit)
ROC_3_clog <- roc(test_set$loan_status,predictions_model_3_clog)
ROC_5_logit <- roc(test_set$loan_status,predictions_model_5_logit)
ROC_5_probit <- roc(test_set$loan_status,predictions_model_5_probit)
ROC_5_clog <- roc(test_set$loan_status,predictions_model_5_clog)
ROC_7_probit <- roc(test_set$loan_status,predictions_model_7_probit)
ROC_7_logit <- roc(test_set$loan_status,predictions_model_7_logit)
ROC_7_clog <- roc(test_set$loan_status,predictions_model_7_clog)
ROC_8_probit <- roc(test_set$loan_status,predictions_model_8_probit)
ROC_8_logit <- roc(test_set$loan_status,predictions_model_8_logit)
ROC_8_clog <- roc(test_set$loan_status,predictions_model_8_clog)



plot(ROC_3_logit)
lines(ROC_3_probit, col = "blue")
lines(ROC_3_clog, col = "red")
lines(ROC_5_logit, col = "green")
lines(ROC_5_clog, col = "yellow")
lines(ROC_5_clog, col = "purple")
lines(ROC_7_logit, col = "orange")
lines(ROC_7_clog, col = "pink")
lines(ROC_7_clog, col = "lilac")






auc(ROC_3_logit)
auc(ROC_3_probit)
auc(ROC_3_clog)
auc(ROC_5_logit)
auc(ROC_5_probit)
auc(ROC_5_clog)
auc(ROC_7_probit)
auc(ROC_7_clog)
auc(ROC_7_logit)
auc(ROC_8_probit)
auc(ROC_8_clog)
auc(ROC_8_logit)



cutoff<-quantile(predictions_model_7_logit ,0.8)
cutoff
result_after_cutoff_20 <- ifelse(predictions_model_7_logit  > cutoff, 1, 0)
table(test_set$loan_status, result_after_cutoff_20)

accuracy <- (5000 + 861) / sum(table(test_set$loan_status, result_after_cutoff_20))
cat("Accuracy:", accuracy, "\n")
specificity <- 5000 / (5000 + 518)
cat("Specificity:", specificity, "\n")
sensitivity <- 861 / (513 + 861)
cat("Sensitivity:", sensitivity, "\n")



real_and_prediction <-cbind(test_set$loan_status,result_after_cutoff_20)
real_and_prediction
accepted_loans <-real_and_prediction[result_after_cutoff_20==0,1]
bad_rate <-sum(accepted_loans)/length(accepted_loans)
bad_rate



