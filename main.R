
# load the required packages

if (!require("rpart.plot")) {
    install.packages("rpart.plot", dependencies = TRUE, quiet = TRUE)
    library("rpart.plot", quietly = TRUE)
}

if (!require("glmnet")) {
    install.packages("glmnet", dependencies = TRUE, quiet = TRUE)
    library(glmnet, quietly = TRUE)
}

if (!require("randomForest")) {
    install.packages("randomForest", dependencies = TRUE, quiet = TRUE)
    library(randomForest, quietly = TRUE)
}

# 
# 
train.mnb <- function (dtm,labels) 
{
  call <- match.call()
  V <- ncol(dtm)
  N <- nrow(dtm)
  prior <- table(labels)/N
  labelnames <- names(prior)
  nclass <- length(prior)
  cond.probs <- matrix(nrow=V,ncol=nclass)
  dimnames(cond.probs)[[1]] <- dimnames(dtm)[[2]]
  dimnames(cond.probs)[[2]] <- labelnames
  index <- list(length=nclass)
  for(j in 1:nclass){
    index[[j]] <- c(1:N)[labels == labelnames[j]]
  }
  
  for(i in 1:V){
    for(j in 1:nclass){
      cond.probs[i,j] <- (sum(dtm[index[[j]],i])+1)/(sum(dtm[index[[j]],])+V)
    }
  }
  list(call=call,prior=prior,cond.probs=cond.probs)    
}

predict.mnb <-
  function (model,dtm) 
  {
    classlabels <- dimnames(model$cond.probs)[[2]]
    logprobs <- dtm %*% log(model$cond.probs)
    N <- nrow(dtm)
    nclass <- ncol(model$cond.probs)
    logprobs <- logprobs+matrix(nrow=N,ncol=nclass,log(model$prior),byrow=T)
    classlabels[max.col(logprobs)]
  }


# Reads a confusion matrix generated with table() and reports quality measures
# Inputs:
# =======
# t: The input confusion matrix
interpret.cf <- function(t = c()) {
    print ("Confusion Matrix")
    print (t)
    
    acc <- (t[1,1] + t[2,2]) / sum(t)
    print (paste("Accuracy: ", acc, sep = ''))
    
    mis.class.rate <- (t[1,2] + t[2,1]) / sum(t)
    print (paste("Misclassification Rate: ", mis.class.rate, sep = ''))
    
    tp.rate <- t[2,2] / (t[2,1] + t[2,2])
    print (paste("True Positive Rate: ", tp.rate, sep = ''))
    
    sp <- t[1,1] / (t[1,1] + t[1,2])
    print (paste("Specificity: ", sp, sep = ''))
    
    pr <- t[2,2] / (t[1,2] + t[2,2])
    print (paste("Precision: ", pr, sep = ''))
}

# 
read.train.from.directory <- function(dir = "") {
    d <- paste(dir, "fold", 1, "/",sep = '')
    corp <- VCorpus(DirSource(d, encoding="UTF-8"))
    
    for (i in 2:4) {
        d <- paste(dir, "fold", i, "/", sep = '')
        corp <- c(corp, VCorpus(DirSource(d, encoding="UTF-8")))
    }
    return (corp)
}

# 
preprocess <- function(corpus) {
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, content_transformer(tolower))
    
    return (corpus)
}


#setwd('/Users/Niels/Desktop/Practicum_2')

neg.dec.all <- read.train.from.directory(dir = "database/negative_polarity/deceptive_from_MTurk/")
reviews.neg.dec.test <- VCorpus(DirSource("database/negative_polarity/deceptive_from_MTurk/fold5/", encoding="UTF-8"))


neg.truth.all <- read.train.from.directory(dir = "database/negative_polarity/truthful_from_Web/")
reviews.neg.truth.test <- VCorpus(DirSource("database/negative_polarity/truthful_from_Web/fold5/", encoding="UTF-8"))


reviews.all      <- c(neg.dec.all, neg.truth.all)
reviews.all.test <- c(reviews.neg.dec.test, reviews.neg.truth.test)
reviews.all.test <- preprocess(reviews.all.test)

labels.test <- c(rep(0,80),rep(1,80))
reviews.all <- preprocess(reviews.all)

reviews.neg.dec.test <- preprocess(reviews.neg.dec.test)

labels <- c(rep(0,320), rep(1,320))

index.neg <- sample(320,100)
index.pos <- 320+sample(320,100)
index.train <- c(index.neg, index.pos)


train.dtm <- DocumentTermMatrix(reviews.all[index.train])
train.dtm.verysparse <- train.dtm
train.dtm.sparse <- removeSparseTerms(train.dtm, 0.95)
train.dtm.lessparse <- removeSparseTerms(train.dtm, 0.6)


# Naive Bayes normal
NaiveBayesNormal <-function(data){
  
  reviews.mnb <- train.mnb(as.matrix(data), labels[index.train])
  
  test.dtm <- DocumentTermMatrix(reviews.all[-index.train],
                                 list(dictionary=dimnames(data)[[2]]))
  
  
  reviews.mnb.pred <- predict.mnb(reviews.mnb, as.matrix(test.dtm))
  nb.cm <- table(reviews.mnb.pred, labels[-index.train])
  interpret.cf(nb.cm)
}

#need this for bigrams
BigramTokenizer <-function(x) unlist(lapply(ngrams(words(x), c(1,2,3)), paste, collapse = " "), use.names = FALSE)


#naive bayes bigram <- not working yet
NaiveBayesBi <- function(){
  # extract bigrams
  train.dtm2 <- DocumentTermMatrix(reviews.all[index.train],
                                   control = list(tokenize = BigramTokenizer))
  
  train.dtm2 <- removeSparseTerms(train.dtm2,0.99)
  
  train.dat1 <- as.matrix(train.dtm)
  train.dat2 <- as.matrix(train.dtm2)
  train.dat <- cbind(train.dat1,train.dat2)
  
  
  reviews.mnb <- train.mnb(as.matrix(train.dat), labels[index.train])
  
  #using training dic
  test3.dtm <- DocumentTermMatrix(reviews.all[-index.train],
                                  list(dictionary=dimnames(train.dat)[[2]]))
  
  #convert to ordinary matrix
  test3.dat <- as.matrix(test3.dtm)
  
  # get columns in the same order as on the training set
  test3.dat <- test3.dat[,dimnames(train.dat)[[2]]]
  

  reviews.mnb.pred <- predict.mnb(reviews.mnb, as.matrix(test3.dtm))
  
  nb.cm <- table(reviews.mnb.pred, labels[-index.train])
  interpret.cf(nb.cm)
}


print('Not removed anything')
NaiveBayesNormal(train.dtm.verysparse)

print('Removed 0.95')
NaiveBayesNormal(train.dtm.sparse)

print('Removed 0.6')
NaiveBayesNormal(train.dtm.lessparse)

###################################
# Classification Tree
# Grow the tree
reviews.rpart <- rpart(label~., data = data.frame(as.matrix(train.dtm), 
                       label=labels[index.train]), cp=0, method="class")
# simple tree for plotting
reviews.rpart.pruned <- prune(reviews.rpart, cp=1.37e-02)
rpart.plot(reviews.rpart.pruned)
# tree with lowest cv error
reviews.rpart.pruned <- prune(reviews.rpart, cp=0.001)
rpart.plot(reviews.rpart.pruned)
# make predictions on the test set
reviews.rpart.pred <- predict(reviews.rpart.pruned,
                                newdata=data.frame(as.matrix(test.dtm)), type="class") # show confusion matrix

s <- table(reviews.rpart.pred, labels[-index.train])
interpret.cf(s)
# accuracy is worse than naive Bayes!



###################################
# logistic regression with lasso penalty
logisticRegNormal <- function() {

  test.dtm <- DocumentTermMatrix(reviews.all[-index.train],
                                 list(dictionary=dimnames(train.dtm)[[2]]))
  
  reviews.glmnet <- cv.glmnet(as.matrix(train.dtm), labels[index.train], family="binomial",type.measure="class")
  
  plot(reviews.glmnet)
  coef(reviews.glmnet, s="lambda.1se")
  
  reviews.logreg.pred <- predict(reviews.glmnet,
                                 newx=as.matrix(test.dtm), s="lambda.1se", type="class")
  
  # show confusion matrix
  lr <- table(reviews.logreg.pred, labels[-index.train])
  interpret.cf(lr)
  
}

###################################
# logistic regression with lasso penalty 7 using Bigrams
logisticRegBi <- function() {
  
  # extract bigrams
  train.dtm2 <- DocumentTermMatrix(reviews.all[index.train],
                                   control = list(tokenize = BigramTokenizer))
  train.dtm2 <- removeSparseTerms(train.dtm2,0.99)
  
  train.dat1 <- as.matrix(train.dtm)
  train.dat2 <- as.matrix(train.dtm2)
  train.dat <- cbind(train.dat1,train.dat2)
  
  # fit regularized logistic regression model
  # use cross-validation to evaluate different lambda values
  reviews3.glmnet <- cv.glmnet(train.dat,labels[index.train],
                               family="binomial",type.measure="class")
  # show coefficient estimates for lambda-1se
  coef(reviews3.glmnet,s="lambda.1se")
  
  #using training dic
  test3.dtm <- DocumentTermMatrix(reviews.all[-index.train],
                                  list(dictionary=dimnames(train.dat)[[2]]))
  
  #convert to ordinary matrix
  test3.dat <- as.matrix(test3.dtm)
  
  # get columns in the same order as on the training set
  test3.dat <- test3.dat[,dimnames(train.dat)[[2]]]
  
  # make predictions using lambda.1se
  reviews.logreg.pred3 <- predict(reviews3.glmnet,newx=test3.dat,
                                    s="lambda.1se",type="class")
  
  # show confusion matrix
  lr <- table(reviews.logreg.pred3, labels[-index.train])
  interpret.cf(lr)
  
}





#############################
# Random Forest
#############################
rf <- randomForest(label~., data = data.frame(as.matrix(train.dtm), 
                        label=as.factor(as.character(labels[index.train]))), 
                        cp=20, method="class")

reviews.rf.pred <- predict(rf,
                              newdata=data.frame(as.matrix(test.dtm)), 
                              type="class")

rf.cm <- table(reviews.rf.pred, labels[-index.train])

interpret.cf(rf.cm)
