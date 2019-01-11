import pandas as pd  
import numpy as np 

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix
from sklearn.externals import joblib
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
from sklearn.feature_selection import SelectFromModel
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import StratifiedShuffleSplit

class RandomForest():
    def __init__(self, dataset):
        self.encoder = LabelEncoder()
        self.dataset = dataset.apply(LabelEncoder().fit_transform)

        self.x_labels = ['age', 'stars', 'languages', 'main_language', 'owner_type', 'core_contributors', 'has_license', 'domain', 'has_readme', 'has_contributing', 'has_wiki', 'time_for_merge']
        self.y_label = ['cluster']

        self.x = self.dataset[self.x_labels]
        self.y = self.dataset[self.y_label]
        
        self.remove_high_correlation()

        stratified_split = StratifiedShuffleSplit(n_splits=1, test_size=0.25)
        stratified_split.get_n_splits(self.x.values, self.y.values)
        

        for train_index, test_index in stratified_split.split(self.x.values, self.y.values):
            self.x_train, self.x_test = self.x.values[train_index], self.x.values[test_index]
            self.y_train, self.y_test = self.y.values[train_index], self.y.values[test_index]

        self.standardize_features()
        self.run_classifier()

    def remove_high_correlation(self):
        threshold = 0.7
        correlation_matrix = self.x.corr(method='spearman')

        for i in range(len(correlation_matrix.columns)):
            for j in range(i):
                if correlation_matrix.iloc[i, j] >= threshold:
                    removed_feature = correlation_matrix.columns[i]

                    if removed_feature in self.x.columns:
                        print('Removing column ' + removed_feature + ' highly correlated with ' + correlation_matrix.columns[j])
                        print('Spearmans Rho: ' + str(correlation_matrix.iloc[i, j]) + ' (Greater than 0.7)')
                        self.x = self.x.drop(removed_feature, 1)

    def standardize_features(self):
        scaler = StandardScaler()
        self.x_train = scaler.fit_transform(self.x_train)
        self.x_test = scaler.transform(self.x_test)

    def run_classifier(self):
        classifier = RandomForestClassifier(n_estimators = 50, random_state = 46)
        classifier.fit(self.x_train, self.y_train.ravel())

        '''
        ('stars', 0.19078521342857344)
        ('time_for_merge', 0.15183768399599212)
        ('core_contributors', 0.11803287754298111)
        ('age', 0.1114337600451946)
        ('languages', 0.11112598934025578)
        ('main_language', 0.1076871666749779)
        ('domain', 0.08008741290250447)
        ('has_contributing', 0.03816656529616988)
        ('owner_type', 0.025743846806127405)
        ('has_license', 0.02544065380868748)
        ('has_readme', 0.00771392259245028)
        ('has_wiki', 0.031944907566085465)
        '''

        sfm = SelectFromModel(classifier)
        sfm.fit(self.x_train, self.y_train)

        for feature_list_index in sfm.get_support(indices=True):
            print(self.x_labels[feature_list_index])

        y_pred = classifier.predict(self.x_test)

        for feature in zip(self.x_labels, classifier.feature_importances_):
            print(feature)
  
        print(classification_report(self.y_test, y_pred))  
        print(accuracy_score(self.y_test, y_pred))


dataset = pd.read_csv('../../spreadsheets/summary.csv') 
random_forest = RandomForest(dataset)