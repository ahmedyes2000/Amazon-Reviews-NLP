reviw <- read.csv(file = "E:\\Summer2018\\Independent Study\\reviews_Baby.csv", header = FALSE)
head(reviw)

obj <- read.csv(file = "E:\\Summer2018\\Independent Study\\objective5000.csv", header = FALSE)
subj <- read.csv(file = "E:\\Summer2018\\Independent Study\\subjective5000.csv", header = FALSE)
###############################################
# V6 : Cleaning review comment
###############################################
# Cleaning
install.packages("tm")
install.packages("SnowballC")

library(tm)
library(SnowballC)

# Cleaning Text
# (1)
temp <- reviw$V6
temp <- Corpus(VectorSource(temp))
# Lowercase, Remove punctuations, whitespace, numbers and Stop Words
temp <- tm_map(temp, content_transformer(tolower))
temp <- tm_map(temp,content_transformer(removeNumbers))
temp <- tm_map(temp,content_transformer(removePunctuation))
temp <- tm_map(temp, stripWhitespace)
temp <- tm_map(temp, removeWords, stopwords('english'))
#temp <- tm_map(temp, removeWords, stopwords_minimal)
#temp <- tm_map(temp, removeWords, stopwords_prod)
#temp <- tm_map(temp, removeWords, stopwords_prod2)
#temp <- tm_map(temp, removeWords, stopwords_ranks)
#temp <- tm_map(temp, removeWords, stopwords_terrier)
#temp <- tm_map(temp, removeWords, stopwords_webconfs)
#temp <- tm_map(temp, removeWords, stopwords_xpo6)
temp <- tm_map(temp, stripWhitespace)
# Removing Contextual Words (high frequency Contextual Words)
# Stemming
temp <- tm_map(temp, stemDocument)
# print(temp[[1]]$content)
# print(temp[[2]]$content)
# print(temp[[200]]$content)
# print(temp[[2000]]$content)
# New Column with Cleaned Text
txt1 <- as.list(temp)

####################
# objective subjective
########################
objtxt <- obj$V1
objtxt <- Corpus(VectorSource(objtxt))
# Lowercase, Remove punctuations, whitespace, numbers and Stop Words
objtxt <- tm_map(objtxt, content_transformer(tolower))
objtxt <- tm_map(objtxt,content_transformer(removeNumbers))
objtxt <- tm_map(objtxt,content_transformer(removePunctuation))
objtxt <- tm_map(objtxt, stripWhitespace)
objtxt <- tm_map(objtxt, removeWords, stopwords('english'))
objtxt <- tm_map(objtxt, stripWhitespace)
# Removing Contextual Words (high frequency Contextual Words)
# Stemming
objtxt <- tm_map(objtxt, stemDocument)
objtxt_fl <- as.character(objtxt)



subtxt <- subj$V1
subtxt <- Corpus(VectorSource(subtxt))
# Lowercase, Remove punctuations, whitespace, numbers and Stop Words
subtxt <- tm_map(subtxt, content_transformer(tolower))
subtxt <- tm_map(subtxt,content_transformer(removeNumbers))
subtxt <- tm_map(subtxt,content_transformer(removePunctuation))
subtxt <- tm_map(subtxt, stripWhitespace)
subtxt <- tm_map(subtxt, removeWords, stopwords('english'))
subtxt <- tm_map(subtxt, stripWhitespace)
# Removing Contextual Words (high frequency Contextual Words)
# Stemming
subtxt <- tm_map(subtxt, stemDocument)

subtxt_fl <- as.character(subtxt)

obj$txt1 <- objtxt_fl
subj$txt1 <- subtxt_fl

dataf1 <- rbind(obj,subj)

dataf1 <- as.data.frame(dataf1)

#######################################################
# Classification
####
# logiditic and naive baise 
install.packages("caret")
install.packages("tm")
install.packages("plyr")
install.packages("class")

library(tm)
library(plyr)
library(class)
library(caret)

# Change path to where the CSV is on your desktop
#smallTest <- read.csv("C:/Users/Gaurav Jetley/Desktop/smallTest2010s_1000.csv", stringsAsFactors=FALSE, header = FALSE)
#View(smallTest)

# Removing all spaces from Class Columns
#smallTest$V1
#smallTest$V1 <- gsub("[[:space:]]", "", smallTest$V1)
#smallTest$V1

#smallTest$V2 <- gsub("[[:space:]]", "", smallTest$V2)
#smallTest$V3 <- gsub("[[:space:]]", "", smallTest$V3)


# Converting Document Column to Corpus object type and removing extra spaces
corpus <- VCorpus(VectorSource(dataf1$V1))
corpus
corpus <- tm_map(corpus, stripWhitespace)

# Making the Term Document Matrix
tdm <- DocumentTermMatrix(corpus)
tdm

# Removing Sparse Terms
# Only use below if the TDM is very sparse. Like above 90%
tdm <- removeSparseTerms(tdm, 0.975)
tdm
as.matrix(tdm)
# Dimentions of the TDM. First number of the number of documents. Second number is total distinct words
dim(tdm)


# Convert back to a data.frame
tdm <- as.matrix(tdm)
tdm1 <- as.data.frame(tdm)


tdm <- cbind(dataf1$V2, tdm1)
dim(tdm)
head(tdm)
tdm <- as.data.frame(tdm)
unique(tdm$family)  # finding the uniques values to check the meaning of values in variables
# fit <- train(V2 ~ V1, data = dataf1, method = "bayesglm")
#m1 <- glm(V2 ~ as.character(txt1), data = dataf1, method = "bayesglm")
#m1 <- glm(V2 ~ as.character(txt1), data = dataf1)

length(dataf1$V2)
colnames(tdm)[1] <- "subobj"
for (i in 2:187){
tdm[,i] = as.factor(tdm[,i])
}



smp_size <- floor(0.75 * nrow(tdm))

## set the seed to make your partition reproducible
set.seed(345)
train_tdm <- sample(seq_len(nrow(tdm)), size = smp_size)

train <- tdm[train_tdm, ]
test <- tdm[-train_tdm, ]

#for (i in 1:2500){
#if (test$action[i] == 2) { 
#test$action[i] = 1
#}
#}

unique(train$action)
unique(test$action)

fit <- glm(as.numeric(as.factor(train$subobj)) ~ . , data = train[ ,]) 
#fit <- glm(as.numeric(as.factor(tdm$subobj)) ~ . , data = tdm[ ,]) 
#sample(1:187,size = 150,replace = FALSE)
summary(fit)

#tdm_sample <- tdm[sample(1:10000,size = 1000,replace = FALSE),]
#fit_p <- predict(fit, tdm_sample[,-1])
fit_p <- predict(fit, test[,-1])
fit_p
#fit_p <- predict(fit, dataf1[sample(1:10000,size = 100,replace = FALSE),])
#fit_p <- predict(fit, tdm[sample(1:10000,size = 100,replace = FALSE),-1])

########################
# Validating the model using
#######################
View(test)
as.data.frame(fit_p)
length(fit_p)
dim(fit_p)
conf <- cbind(fit_p,test$subobj)


conf <- as.data.frame(conf)
View(conf)
summary(conf$fit_p)

is.data.frame(conf)
conf$fit_p <- round(conf$fit_p)

conf$CORR <- 0
View(conf)

for (i in 1:2500)
{
if (conf$fit_p[i] == conf$V2[i]){ conf$CORR[i]=1
}
}
View(conf)
Conc <- sum(conf$CORR)
Conc
discor=2500 - Conr
discor
##########################################################
# Sentiment Analysis
##########################################################
install.packages("SentimentAnalysis")
library(SentimentAnalysis)
analyzeSentiment(temp)

# Assigning Classes and other variables to each Document
tdm <- cbind(smallTest[,c(1,2,3)], tdm)
tdm

################################
# Training and Test sets
train <- tdm[tdm$V1 == "cong" , c(-1,-2)]
train$V3 <- as.factor(train$V3)

#test <- tdm[tdm$V1 == "sec", c(-1,-2)]
test$V3 <- as.factor(test$V3)


# Making BayesGLM Classifier and fitting using Train data.
# YOU CAN CHANGE THE METHOD = " " BELOW TO WHATEVER METHOD YOU WANT.
# Use "nb" for Naive Bayes
# Use "svmLinear" for SVM
# Use "knn" for K-NN
# Use "bayesglm" for Bayesian GLM
# IF the terminal asks to install package them select option 1. Type 1 and hit enter
fit <- train(V3 ~ ., data = train, method = "bayesglm")
# Summary of model and Accuracy on Training Set
fit
predict(fit, newdata = train)
# Actual
train$V3


# Accuracy on Test Data
predict(fit, newdata = test)
# Actual
test$V3

# Confusion Matrix
confusionMatrix(reference = test$V3, data = predict(fit, newdata = test))

