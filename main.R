###################################################################################################
# ass2.R                                                                                          #
# @author:  Ioannis Pavlos Panteliadis <i.p.panteliadis@students.uu.nl>                           #
# @author:  Renis Krisafi <r.krisafi@students.uu.nl>                                              #
# @brief:   This file contains function declarations and implementations for the 2nd assignment   #
#           in the Data Mining class of the Utrecht University.                                   #
###################################################################################################

# load the required packages

if (!require("rpart.plot")) {
    install.packages("rpart.plot", dependencies = TRUE, quiet = TRUE)
    library("rpart.plot", quietly = TRUE)
}


if (!require("tm")) {
    install.packages("slam", dependencies = TRUE, quiet = TRUE)
    install.packages("tm", repos="http://R-Forge.R-project.org", dependencies = TRUE, quiet = TRUE)
    library(tm, quietly = TRUE)
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
train.mnb <- function (dtm=train.dtm,labels=c()) {
    call <- match.call()
    V <- ncol(dtm)
    N <- nrow(dtm)
    prior <- table(labels)/N
    labelnames <- names(prior)
    nclass <- length(prior)
    cond.probs <- matrix(nrow=V , ncol=nclass)
    dimnames(cond.probs)[[1]] <- dimnames(dtm)[[2]] 
    dimnames(cond.probs)[[2]] <- labelnames
    index <- list(length=nclass)
    for (j in 1:nclass){
        index[[j]] <- labels == labelnames[j]
    }
    
    for (i in 1:V) {
        for (j in 1:nclass){
            
            cond.probs[i,j] <- (sum(dtm[index[[j]],i])+1)/(sum(dtm[index[[j]],])+V) 
        }
    } 
    list(call=call,prior=prior,cond.probs=cond.probs)
}

# 
predict.mnb <- function (model=model,dtm=dtm) {
    classlabels <- dimnames(model$cond.probs)[[2]]
    logprobs <- dtm %*% log(model$cond.probs)
    N <- nrow(dtm)
    nclass <- ncol(model$cond.probs)
    logprobs <- logprobs + matrix(nrow=N, ncol=nclass, log(model$prior), byrow=T)
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
# 
preprocess <- function(corpus) {
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, content_transformer(tolower))
    
    return (corpus)
}


neg.dec.all <- read.train.from.directory(dir = "../../op_spam_v1.4/negative_polarity/deceptive_from_MTurk/")
reviews.neg.dec.test <- VCorpus(DirSource("../../op_spam_v1.4/negative_polarity/deceptive_from_MTurk/fold5/", encoding="UTF-8"))


neg.truth.all <- read.train.from.directory(dir = "../../op_spam_v1.4/negative_polarity/truthful_from_Web/")
reviews.neg.truth.test <- VCorpus(DirSource("../../op_spam_v1.4/negative_polarity/truthful_from_Web/fold5/", encoding="UTF-8"))


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
train.dtm <- removeSparseTerms(train.dtm, 0.95)

##################################
# Naive Bayes
reviews.mnb <- train.mnb(as.matrix(train.dtm), labels[index.train])

test.dtm <- DocumentTermMatrix(reviews.all[-index.train],
                               list(dictionary=dimnames(train.dtm)[[2]]))


reviews.mnb.pred <- predict.mnb(reviews.mnb, as.matrix(test.dtm))
nb.cm <- table(reviews.mnb.pred, labels[-index.train])
interpret.cf(nb.cm)

###################################
# Classification Tree
# Grow the tree
reviews.rpart <- rpart(label~., 
                       data = data.frame(as.matrix(train.dtm), 
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

# logistic regression with lasso penalty
reviews.glmnet <- cv.glmnet(as.matrix(train.dtm), labels[index.train], family="binomial",type.measure="class")
plot(reviews.glmnet)
coef(reviews.glmnet, s="lambda.1se")
reviews.logreg.pred <- predict(reviews.glmnet,
                               newx=as.matrix(test.dtm), s="lambda.1se", type="class")

# show confusion matrix
lr <- table(reviews.logreg.pred, labels[-index.train])
interpret.cf(lr)





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
