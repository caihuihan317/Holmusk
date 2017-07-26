# read 4 data files
setwd("~/Downloads/Holmusk")
bill_id <- read.csv("bill_id.csv",header = TRUE)
bill_amount <- read.csv("bill_amount.csv",header = TRUE)
demographics <- read.csv("demographics.csv",header = TRUE)
clinical_data <- read.csv("clinical_data.csv",header = TRUE)

# merge bill_id and bill_amount
new_bill <- merge(bill_id,bill_amount,by="bill_id")

# calculate total bill amount for each patient
total_bill <- aggregate(new_bill$amount, by=list(new_bill$patient_id), FUN=sum)
colnames(total_bill) <- cbind("patient_id","total_bill")

# merge total_bill and demographics
bill_demo <- merge(total_bill, demographics, by="patient_id" )

# selet the nearest clinical record of each patient
library(lubridate)
new_clinical <- clinical_data[order(-as.numeric(ymd(clinical_data$date_of_admission))), ] 
new_clinical <- new_clinical[!duplicated(new_clinical$id),]

# calculate number of visits of each patient
library(plyr)
new_clinical$visit <- count(clinical_data,"id")$freq

# merge bill_demo and new_clinical
new_data <- merge(bill_demo,new_clinical, by.x="patient_id",by.y="id")
new_data$average_pay <- new_data$total_bill/new_data$visit

# calculate age of the patients at date_of_admission
new_data$age <- as.numeric(year(new_data$date_of_admission))- as.numeric(year(new_data$date_of_birth))
summary(new_data)

# calculate BMI values for each patient
new_data$bmi <- new_data$weight/(new_data$height/100)^2

# standardize variables
new_data$gender[new_data$gender == "m"] <- "Male"
new_data$gender[new_data$gender == "f"] <- "Female"
new_data$race[new_data$race == "chinese"] <- "Chinese"
new_data$race[new_data$race == "India"] <- "Indian"
new_data$resident_status[new_data$resident_status == "Singapore citizen"] <- "Singaporean"
new_data$medical_history_3[new_data$medical_history_3 == "Yes"] <- 1
new_data$medical_history_3[new_data$medical_history_3 == "No"] <- 0
new_data$medical_history_3 <- as.numeric(new_data$medical_history_3)
summary(new_data)

# create dummies for gender, race, resident_status, bmi
new_data$Male <- ifelse(new_data$gender=="Male",1,0) 
new_data$Chinese <- ifelse(new_data$race=="Chinese",1,0) 
new_data$Malay <- ifelse(new_data$race=="Malay",1,0) 
new_data$Indian <- ifelse(new_data$race=="Indian",1,0) 
new_data$Singaporean <- ifelse(new_data$resident_status=="Singaporean",1,0) 
new_data$PR <- ifelse(new_data$resident_status=="PR",1,0) 
new_data$Overweight <- ifelse(new_data$bmi >= 25,1,0) 
new_data$Underweight <- ifelse(new_data$bmi <= 18.5,1,0) 

# filter out incomplete data
complete_data <- na.omit(new_data)

# split training and testing data
data_train <- complete_data[as.numeric(year(complete_data$date_of_admission))<=2014,]
data_test <- complete_data[as.numeric(year(complete_data$date_of_admission))>2014,]

# select variables to build model
data_model <- data_train[,-cbind(1,2,3,4,5,6,7,8,30,31,35)]

# multiple linear regression with all variables
model1 <- lm(visit~., data = data_model)
summary(model1)


# stepwise model selction
library(MASS)
model2 <- stepAIC(model1, data=data_model,direction="both")
summary(model2)



### clustering ###
# clustering all data without knowing visit for 3 to 20 clusters
clustering <- complete_data[,c(1,7,32)]
  
clusters <- 3
while (clusters <= 20){
  fit <- kmeans(complete_data[-c(1,2,3,4,5,6,7,8,30,31,32,35)], clusters,iter.max=20)
  clustering <- data.frame(clustering, fit$cluster)
  colnames(clustering)[ncol(clustering)] <- as.character(clusters)
  clusters = clusters + 1
}

# use training data to calculate a revisit tendency score for each cluster 
cluster_train <- clustering[as.numeric(year(clustering$date_of_admission))<=2014,]

ctrain <- matrix(data = NA, nrow=20, ncol=18)
ctrain <- data.frame(ctrain)
colnames(ctrain) <- c(3:20)

clusters <- 3
while (clusters <= 20){
  for (i in 1:clusters){
    ctrain[,clusters-2][i] <- sum(cluster_train[cluster_train[,clusters+1]==i,]$visit)/nrow(cluster_train[cluster_train[,clusters+1]==i,])
  }
  clusters <- clusters + 1
}

normalize <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -min(x, na.rm=TRUE))}
ctrain_normed <- as.data.frame(lapply(ctrain, normalize))




