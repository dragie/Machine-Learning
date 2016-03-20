setwd("D:\\HackerRank\\MachineLearning")
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

library(tm)
library(plyr)
library(class)

doc <- Corpus(VectorSource(textData$doc))
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, stripWhitespace)
doc <- tm_map(doc, tolower)
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, PlainTextDocument)

tdm <- TermDocumentMatrix(doc)
dtm<- DocumentTermMatrix(doc, control = list(minWordLength = 1))
inspect(dtm[1:5, 1:5])

dtm<- DocumentTermMatrix(ingredients,  control = list(bounds = list(global= c(0.006*length(ingredients), .20*length(ingredients)))))

tdm <- removeSparseTerms(tdm, 0.7)
train <- as.data.frame(inspect(dtm))
dim(train)
library(rpart)
dt <- rpart(class ~., train)

View(train)
train$classtarget <- factor(textData$class)
library(h2o)
h2o.server <- h2o.init( nthreads= -1)
train.hex <- as.h2o(train)
features <- colnames(train[,-19601 ])
gbmF_model_1 <- h2o.gbm( x=features,
                         y = "classtarget",
                         training_frame =train.hex ,
                         max_depth = 3,
                         distribution = "multinomial",
                         ntrees =500,
                         learn_rate = 0.05,
                         nbins_cats = 5891
)





test <- (1:nrow(tdm))[-train]
knn.pred <- knn(tdm[train], tdm[test])

finalData <- as.data.frame(dtm)
finalData$class <- category$class
write.csv(as.data.frame(as.matrix(doc)), "temp.csv")


tdm <- as.data.frame(tdm)
