#code is not optimized! (for example we read again the csv. This is done for practical reason for the reviewer)
# Import libraries
library(e1071)
library(caTools)


#accuracy vector
accVec <- rep(1, 10)


# Load data
set.seed(42)

urlCsvFile <- "/home/gigi/Desktop/ML/leaf/leaf con nomi.csv"

dataSvm <- read.csv(urlCsvFile, header=FALSE)
colnames(dataSvm) <- c("Species","Specimen_Number","Eccentricity","Aspect_Ratio","Elongation","Solidity","Stochastic_Convexity","Isoperimetric_Factor","Maximal_Indentation_Depth","Lobedness","Average_Intensity","Average_Contrast","Smoothness","Third_moment","Uniformity","Entropy")
dataSvm <- data[,-2]
dataSvm <- data[1:10]
dataSvm$Species <- as.factor(dataSvm$Species)
dataSvm
dataSvm_scale <- scale(dataSvm[, 2:9])
dataSvm <- cbind(dataSvm[1],dataSvm_scale)
dataSvm

shuffled_dataSvm= dataSvm[sample(nrow(dataSvm)), ]
shuffled_dataSvm

for (variable in seq(1:10)) {
  data_subset <- shuffled_dataSvm %>% 
    group_by(Species) %>% 
    filter(row_number() <= 10 | n() <= 10)
  data_subset
  datatest <- anti_join(data,data_subset)
  
  model <- svm(Species ~ ., data=shuffled_dataSvm, kernel="radial", cost=variable, scale=TRUE)
  model
  
  predictions <- predict(model, datatest)
  # Print the accuracy of the model
  accuracy <- mean(predictions == datatest$Species)
  print(paste("Accuracy:", accuracy))
  
}

