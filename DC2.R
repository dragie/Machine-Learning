# FILE: Classifying Breast Cancer as Benign or Malignant
# AUTHOR: Timothy P. Jurka

library(RTextTools);

document <- read.delim("https://s3.amazonaws.com/hr-testcases/597/assets/trainingdata.txt", header = FALSE)
temp <- document
document <- temp
View(document)
dim(document)
n <- document[1,1]
write.csv(document, "Document.csv")
#document <- document[-1,]

category <- as.data.frame(substring(document[[1]], 1, 1))
textData <- as.data.frame(substring(document[[1]], 3))
View(textData)
names(category) <- "class"
names(textData) <- "doc"
textData$class <- category$class
textData <- textData[-1,]
training_data <- textData

# CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
# WE WILL TRAIN ON THE Title and Subject COLUMNS
matrix <- create_matrix(training_data, language="english", removeNumbers=FALSE, stemWords=FALSE, removePunctuation=FALSE)

# CREATE A container THAT IS SPLIT INTO A TRAINING SET AND A TESTING SET
# WE WILL BE USING t(training_codes) AS THE CODE COLUMN. WE DEFINE A 200 
# ARTICLE TRAINING SET AND A 400 ARTICLE TESTING SET.
container <- create_container(matrix,training_data$class,trainSize=1:200, testSize=201:600,virgin=FALSE)


# THERE ARE TWO METHODS OF TRAINING AND CLASSIFYING DATA.
# ONE WAY IS TO DO THEM AS A BATCH (SEVERAL ALGORITHMS AT ONCE)
models <- train_models(container, algorithms=c("MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"))
results <- classify_models(container, models)


# VIEW THE RESULTS BY CREATING ANALYTICS
analytics <- create_analytics(container, results)

# RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
# analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
# analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
# analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()

analytics@ensemble_summary
