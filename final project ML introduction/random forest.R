library(dplyr)
library(randomForest)
library(caret)

set.seed(42)

urlCsvFile <- "/home/gigi/Desktop/ML/leaf/leaf con nomi.csv"
nTreeRandomForest <- 500

data <- read.csv(urlCsvFile, header=FALSE)
colnames(data) <- c("Species","Specimen_Number","Eccentricity","Aspect_Ratio","Elongation","Solidity","Stochastic_Convexity","Isoperimetric_Factor","Maximal_Indentation_Depth","Lobedness","Average_Intensity","Average_Contrast","Smoothness","Third_moment","Uniformity","Entropy")
data <- data[,-2]
data <- data[1:9]
data$Species <- as.factor(data$Species)
data

species_counts <- as.data.frame(table(data$Species))
species_counts
# subData <- subset(data, Species!=species_to_remove[1])
# subData

data
dataFiltered <- data[data$Species != "Celtis sp.",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Magnolia soulangeana",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Primula vulgaris",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Salix atrocinera",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Alnus sp.",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Acca sellowiana",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Betula pubescens",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Euonymus japonicus",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Hydrangea sp.",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Ilex perado ssp. azorica",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Magnolia grandiflora",]
dataFiltered
dataFiltered <- dataFiltered[dataFiltered$Species !=  "Quercus suber",]
dataFiltered


data_subset <- dataFiltered %>% 
  group_by(Species) %>% 
  filter(row_number() <= 12 | n() <= 12)
data_subset

data_subset$Species <- factor(data_subset$Species)
data_subset <- data_subset[sample(nrow(data_subset)),]
# datatest <- datatest[sample(nrow(datatest)),]

model <- randomForest(Species ~ ., data_subset,  ntree=500)

oob_error <- model$err.rate[, "OOB"]
print( mean(oob_error))

for (variable in seq(1:1000)) {
  
  
  # species_to_remove <- sample(levels(data_subset$Species), size = 2, replace = FALSE)
  # 
  # dataFiltered <- data[data$Species != species_to_remove[1],]
  # dataFiltered <- data[data$Species != species_to_remove[2],]
  
  data
  dataFiltered <- data[data$Species != "Celtis sp.",]
  dataFiltered
  dataFiltered <- data[data$Species !=  "Magnolia soulangeana",]
  dataFiltered

  data_subset <- dataFiltered %>% 
    group_by(Species) %>% 
    filter(row_number() <= 12 | n() <= 12)
  data_subset
  
  data_subset$Species <- factor(data_subset$Species)
  data_subset <- data_subset[sample(nrow(data_subset)),]
  # datatest <- datatest[sample(nrow(datatest)),]
  
  model <- randomForest(Species ~ ., data_subset,  ntree=500)
  
  oob_error <- model$err.rate[, "OOB"]
 
  mean <- mean(oob_error)
  # if (mean < 0.30) {
    print("")
    cat(mean)
    cat(" - ")
    cat(species_to_remove[1])
    cat(" - ")
    cat(species_to_remove[2])
  # }
  
  
}

data_subset <- data %>% 
  group_by(Species) %>% 
  filter(row_number() <= 9 | n() <= 9)
data_subset
# utrain <- upSample(x = data_subset,
#                    y = data_subset$Species)
# species_counts <- as.data.frame(table(utrain$Species))
# species_counts
# utrain[utrain$Species == "Crataegus monogyna",]


# datatest <- anti_join(data,data_subset)
# 
# datatest <- datatest %>% 
#   group_by(Species) %>% 
#   filter(row_number() <= 2 | n() <= 2)


species_counts <- as.data.frame(table(dataFiltered$Species))
species_counts

data_subset
data_subset <- data_subset[sample(nrow(data_subset)),]
# datatest <- datatest[sample(nrow(datatest)),]

model <- randomForest(Species ~ ., data_subset,  ntree=500, oob.print = TRUE)

oob_error <- model$err.rate[, "OOB"]
mean(oob_error)
model

imp <- importance(model)
# Sort the feature importance values in descending order
imp <- imp[order(imp[, "MeanDecreaseGini"], decreasing = TRUE), ]
# Print the top features
head(imp)

predictions <- predict(model, datatest)

confusionMatrix(data,predictions)


species_counts <- as.data.frame(table(data$Species))
species_counts

confusionMatrix <- table(testData$Species, predictions)

# Print the accuracy of the model
accuracy <- mean(predictions == datatest$Species)

print(paste("Accuracy:", accuracy))







# for (maxObservationsPerSpecies in seq(5,13)) {
#   
#   
# }



