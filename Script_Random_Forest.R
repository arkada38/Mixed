library(randomForest)
# randomForest(formula, data)

# Load the party package. It will automatically load other required packages.
library(party)


# Print some records from data set readingSkills.
print(head(readingSkills))

# Create the forest.
output.forest <- randomForest(nativeSpeaker ~ age + shoeSize + score,
                              data = readingSkills, 
                              ntree=2000)

# View the forest results.
print(output.forest)

# Importance of each predictor.
importance(output.forest)
varImpPlot(output.forest)
