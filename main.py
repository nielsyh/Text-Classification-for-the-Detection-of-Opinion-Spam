from file_importer import Read_Text
from nltk.tokenize import word_tokenize

from textblob.classifiers import *

import nltk
nltk.download('punkt')

a = Read_Text()
training_set = a.get_negative_training_set()
test_set = a.get_negative_test_set()


# Naive Bayes
cl = NaiveBayesClassifier(training_set)

#obs #predict
true_true = 0
true_false = 0
false_true = 0
false_false = 0

for tup in test_set:
    observed = tup[1]
    prediction = cl.classify(tup[0])

    if(observed == 'legit'):
        if(prediction == 'legit'):
            true_true = true_true + 1
        else:
            true_false = true_false + 1
    else:
        if(prediction == 'fake'):
            false_false = false_false +1
        else:
            false_true = false_true + 1


print('True positives: ' + str(true_true))
print('True negatives: ' + str(false_false))
print('False postives: ' + str(false_true))
print('False negatives: ' + str(true_false))


cl = DecisionTreeClassifier(training_set)