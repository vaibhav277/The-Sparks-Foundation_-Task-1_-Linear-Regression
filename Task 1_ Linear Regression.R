#PROBLEM STATEMENT
#Predict the percentage of an student based on the no. of study hours.
#What will be predicted score if a student studies for 9.25 hrs/ day?

#AUTHOR
#VAIBHAV GARG

#READING THE DATASET
DATASET<-read.csv("student_scores dataset.csv")
View(DATASET)

#LOOKING FOR RELATION IN THE DATASET
cor(DATASET)
#A PLOT CHECKING FOR RELATIONS
library(ggplot2)
ggplot(data=DATASET, aes(x=Scores,y=Hours))+geom_point()

#SETTING UP THE  LINEAR MODEL
Linear_Regression<-lm(DATASET$Scores~DATASET$Hours)
summary(Linear_Regression)

#SPLITTING THE DATASET IN TRAIN AND TEST DATASETS
library(caTools)
set.seed(1111)
SPLITTING_DATA<-sample.split(DATASET$Hours, SplitRatio = 0.7)
SPLITTING_DATA

training_data <- subset(DATASET,SPLITTING_DATA == TRUE)
View(training_data)
test_data <- subset(DATASET,SPLITTING_DATA == FALSE)
View(test_data)

#TRAINING THE LINEAR MODEL
LINEAR_MOD<-lm(Scores~Hours,data=training_data)
LINEAR_MOD

plot(DATASET)
abline(LINEAR_MOD,col="Blue")

#PREDICTING THE RESULTS ON THE BASIS OF OUR MODEL
PREDICT<-predict(LINEAR_MOD, test_data)
RESULTS<-data.frame(cbind(Actual_Scores=test_data$Scores, 
                          Predicted_Scores=PREDICT))
RESULTS

#COMPUTING RESULTS FOR HOURS=9.25
X=data.frame(Hours=9.25)
OWN_PREDICT<-predict(LINEAR_MOD, X)
print("Predicting the Score if the Student Studies for 9.25 Hours.")
OWN_PREDICT

#CHECKING THE ACCURACY OF OUR MODEL
ACCURACY<-aov(LINEAR_MOD)
ACCURACY
