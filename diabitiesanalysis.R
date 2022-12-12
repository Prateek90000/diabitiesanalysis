rm(list=ls()) #clearing the enviornment
diabetes_test = read.csv("diabetes_test.csv") #load csv file
diabetes_train = read.csv("diabetes_train.csv") #load csv file

library(caTools) #load library caTools
colnames(diabetes_train)#finding column names of train dataset 
model_1 = glm(Outcome ~.,data =diabetes_train,family = binomial)#creating logistin function
summary(model_1)#finding summary of model_1
#making improvise model as per required and important variable
model_2 = glm(Outcome ~ Pregnancies+Glucose+BloodPressure+BMI+DiabetesPedigreeFunction,data =diabetes_train,family = binomial)
summary(model_2)#finding summary of model_2

predictions = predict(model_2, type="response", newdata=diabetes_test)#prediction on model based on test dataset
diabetespredDF = data.frame(diabetes_test,predictions)#creating data and adding predictions in dataset
diabetespredDF$diabetesprediction = diabetespredDF$predictions>0.5

#confusion matrix -  categorize the predictions against the actual values

cf = table(diabetespredDF$Outcome,diabetespredDF$diabetesprediction)# above 0.5 will be TRUE and below will be FALSE as per prediction values

cf = as.data.frame.matrix(table(diabetespredDF$Outcome,diabetespredDF$diabetesprediction))# to check if the function is a data frame

View(cf)# view table of Cf
accuracy = (96+32)/(96+12+28+32)# evaluating the performance of the model 
truepositive = (96)/(96+12+28+32)
truenegative = (32)/(96+12+28+32)
Falsepositive = (28)/(96+12+28+32) # also called type1 error
Falsenegative = (12)/(96+12+28+32) # also called type2 error

# decision tree

library(rpart)
colnames(diabetes_train)
model_3= rpart(Outcome~Pregnancies+Glucose+BMI+DiabetesPedigreeFunction, data=diabetes_train, method = "class")
plot(model_3, uniform=TRUE, 
     main="Classification Tree for Diabetes")
text(model_3, use.n=TRUE, all=TRUE, cex=.8)
box(which = "outer", lty = "solid")

treePred <- predict(model_3, diabetes_test, type = 'class')
(conf_matrix_dtree<-table(treePred, diabetes_test$Outcome))
accuracy1 = (99+33)/(99+27+9+33)# evaluating the performance of the model 
truepositive1 = (99)/(99+27+9+33)
truenegative1 = (33)/(99+27+9+33)
Falsepositive1 = (9)/(99+27+9+33) # also called type1 error
Falsenegative1 = (33)/(99+27+9+33) # also called type2 error

#better accuracy through tree model