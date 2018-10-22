import glob

class Read_Text:

    path_neg_true = 'database/negative_polarity/truthful_from_Web/*'
    path_neg_false = 'database/negative_polarity/deceptive_from_MTurk/'

    path_pos_true = 'database/positive_polarity/truthful_from_Web/'
    path_pos_false = 'database/positive_polarity/deceptive_from_MTurk/'


    def __init__(self):
        print('Read_Text init')

    def read_a_file(self, path):
        f = open(path, "r")  # opens file with name of "test.txt"
        text = f.read()
        return(text)


 ####  1 NEGATIVE REVIEWS BUT TRUE
    def get_training_neg_true(self):
        fold1_4 = []

        for i in range(1,5):
            path = self.path_neg_true + 'fold' + str(i) + '/*'
            files = glob.glob(path)

            for file in files:
                text = self.read_a_file(file)
                fold1_4.append(text)

        return(fold1_4)

    def get_test_neg_true(self):
        fold = []

        path = self.path_neg_true + 'fold5/*'
        files = glob.glob(path)

        for file in files:
            text = self.read_a_file(file)
            fold.append(text)

        return(fold)

#### 2  NEGATIVE REVIEWS BUT FALSE
    def get_training_neg_false(self):
        fold1_4 = []

        for i in range(1, 5):
            path = self.path_neg_false + 'fold' + str(i) + '/*'
            files = glob.glob(path)

            for file in files:
                text = self.read_a_file(file)
                fold1_4.append(text)

        return (fold1_4)

    def get_test_neg_false(self):
        fold = []

        path = self.path_neg_false + 'fold5/*'
        files = glob.glob(path)

        for file in files:
            text = self.read_a_file(file)
            fold.append(text)

        return (fold)

    #### 3  POSTIVE REVIEWS BUT TRUE
    def get_training_pos_true(self):
        fold1_4 = []

        for i in range(1, 5):
            path = self.path_pos_true + 'fold' + str(i) + '/*'
            files = glob.glob(path)

            for file in files:
                text = self.read_a_file(file)
                fold1_4.append(text)

        return (fold1_4)

    def get_test_pos_true(self):
        fold = []

        path = self.path_pos_true + 'fold5/*'
        files = glob.glob(path)

        for file in files:
            text = self.read_a_file(file)
            fold.append(text)

        return (fold)

#### 4 POSTIVE REVIEWS BUT FALSE
    def get_training_pos_false(self):
        fold1_4 = []

        for i in range(1, 5):
            path = self.path_pos_false + 'fold' + str(i) + '/*'
            files = glob.glob(path)

            for file in files:
                text = self.read_a_file(file)
                fold1_4.append(text)

        return (fold1_4)

    def get_test_pos_false(self):
        fold = []

        path = self.path_pos_false + 'fold5/*'
        files = glob.glob(path)

        for file in files:
            text = self.read_a_file(file)
            fold.append(text)

        return (fold)

####