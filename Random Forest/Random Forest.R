library(randomForest)
# randomForest(formula, data)

# Load the party package. It will automatically load other required packages.
library(party)

set.seed(123)

# Print some records from data set readingSkills.
print(head(readingSkills))

# Create the forest.
output.forest <- randomForest(nativeSpeaker ~ age + shoeSize + score,
                              data = readingSkills, 
                              ntree=2000)

# View the forest results.
print(output.forest)
#                Type of random forest: classification
#                      Number of trees: 2000
# No. of variables tried at each split: 1
# 
#          OOB estimate of  error rate: 1%
# Confusion matrix:
#     no yes class.error
# no  99   1        0.01
# yes  1  99        0.01

# Importance of each predictor.
importance(output.forest)
varImpPlot(output.forest)
#          MeanDecreaseGini
# age              13.98086
# shoeSize         18.18051
# score            57.76019

# Predict sample
test <- readingSkills[1:6, ]
pred <- predict(output.forest, newdata = test)
table(pred, test$nativeSpeaker)
# pred  no yes
#   no   1   0
#   yes  0   5