###############################################
# Code with 0.99 sparcity
###############################################
obj <- read.csv(file = "E:\\Summer2018\\Independent Study\\objective5000.csv", header = FALSE)
subj <- read.csv(file = "E:\\Summer2018\\Independent Study\\subjective5000.csv", header = FALSE)

install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)
########################
# Cleaning  objective
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
####################
# Cleaning  subjective
########################
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
#View(subtxt_fl)

obj$txt1 <- objtxt_fl
subj$txt1 <- subtxt_fl

#View(obj)
dataf1 <- rbind(obj,subj)

dataf1 <- as.data.frame(dataf1)
#View(dataf1)

#######################################################
# Classification
####
# logiditic and naive baise 
#install.packages("caret")
#install.packages("tm")
#install.packages("plyr")
#install.packages("class")
#install.packages("psych")

#library(tm)
#library(plyr)
#library(class)
#library(psych)
#library(caret)

# Converting Document Column to Corpus object type and removing extra spaces
corpus <- VCorpus(VectorSource(dataf1$V1))
corpus <- tm_map(corpus, stripWhitespace)
View(corpus)

# Making the Term Document Matrix
tdm <- DocumentTermMatrix(corpus)
#View(tdm)

# Removing Sparse Terms
# Only use below if the TDM is very sparse. Like above 90%
tdm <- removeSparseTerms(tdm, 0.99)
tdm
# Dimentions of the TDM. First number of the number of documents. Second number is total distinct words
dim(tdm)

# Convert back to a data.frame
tdm <- as.matrix(tdm)
tdm1 <- as.data.frame(tdm)
is.data.frame(tdm1)

tdm <- cbind(dataf1$V2, tdm1)
dim(tdm)
is.data.frame(tdm)

unique(tdm$family)  # finding the uniques values to check the meaning of values in variables


colnames(tdm)[1] <- "subobj"

# in some of the documents we have repeated words in same row, 
#we are considering only one occurance of such senarios 
# this is a reason we are converting all 2 to 1 in the data frame.

for(i in 2:187){
  for(j in 1:10000){
  if (tdm[j,i] > 0) {tdm[j,i] = 1}
  }
}

for (i in 2:187){
  tdm[,i] = as.factor(tdm[,i])
}

unique(tdm$subobj)

dim(tdm)
smp_size <- floor(0.75 * nrow(tdm))

## set the seed to make your partition reproducible
set.seed(345)
train_tdm <- sample(seq_len(nrow(tdm)), size = smp_size)

train <- tdm[train_tdm, ]
test <- tdm[-train_tdm, ]

unique(train$subobj)
unique(test$action)
dim(test)

fit <- glm(as.numeric(as.factor(train$subobj)) ~ . , data = train[ ,]) 
summary(fit)
dim(test)
fit_p <- predict(fit, test[,2:187])
#View(fit_p)
summary(fit_p)

########################
# Validating the model using
#######################
conf <- cbind(fit_p,test$subobj)
conf <- as.data.frame(conf)
dim(conf)
#View(conf)
summary(conf$fit_p)

is.data.frame(conf)

conf$fit_p <- round(conf$fit_p)

conf$CORR <- 0
#View(conf)

for (i in 1:2500)
{
  if (conf$fit_p[i] == conf$V2[i]){ conf$CORR[i]=1
  }
}
#View(conf)
Conc <- sum(conf$CORR)
Conc
discor=2500 - Conc
discor
