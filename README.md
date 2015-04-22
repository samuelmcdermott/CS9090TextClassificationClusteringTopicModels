# CS909 Text Classification Clustering and Topic Models
Submission for Warwick University: CS909 Data Mining
by Samuel McDermott (u1466355)

##Start here
This repository contains a selection of R functions that allow text classification, clustering and topic models.  It was designed for the Reuters-21578, although may be open enough to support other datasets.  (Its probably easier to source all the files in this project to make sure all the interconnected-ness works)

##Preprocessing
The Reuters-21578 dataset contains a set of 21578 documents.  Each document has been manually labelled (binary classification), has a document title and document text.  
The first stage is to get this data in the correct format.  This code, `preprocess.R`, separates the topics (assuming the column with the binary classification contains the string "topic"), so that each document has exactly 1 label.  It then selects the 10 most popular topic types (earn, acquisitions, money-fx, grain, crude, trade, interest, ship, wheat, corn) and outputs a dataframe  with the topic, title and text for these documents.
###Example usage:
`documents <- preprocess(filename)`
####Arguments:
`filename` is the filename of the dataset stored in the working directory (eg. `reuters.csv`).
####Values:
`documents` is a dataframe with columns `topic`,`title`,`text` for all the documents.


##Topic Models
To create topic models for this dataset, use `lda.R`. This uses a Latent Dirichlet Allocation to create topic models. The user can also change the n-gram feature representation, and the number of topics they want.  
###Example usage:
`topicmodel <- lda(documents, n, k)`
####Arguments:
`documents` is the preprocessed dataframe.

`n` is the *n* of *n*-grams. (eg. 1 for unigrams, 2 for bigrams).

`k` is the number of topics you want to create.

####Values:
`topicmodel` is an LDA topic model.  Further processing can be found at [topicmodels](http://cran.r-project.org/web/packages/topicmodels/index.html).  The most useful is `terms(topicmodel,10)` which shows the top 10 terms for each topic.

##Classification
This, `featureClassification.R`, is very open code, that allows you to classify text documents using a large number of classifiers and get suitable macro and micro analytics for it. The user can define the ngrams, the number of folds, the classifier, and any further parameters for the model.

###Example usage:
`classAnalysis <- featureClassification(documents, n, k, classifier,...)`

####Arguments:
`documents` is the preprocessed dataframe, although any dataframe will work if it has the columns `topic`, `title`, `text`.

`n` is the *n* of *n*-grams. (eg. 1 for unigrams, 2 for bigrams)

`k` is the number of folds for *k*-fold classification evaluation.

`classifier` is the type of classifier to use.  This extends the package [RTextTools](http://cran.r-project.org/web/packages/RTextTools/index.html) and further information about the classifiers (apart from Naive Bayes) can be found there The following are acceptable values:

* `"NB"`: Naive Bayes
*  `"SVM"`: Support Vector Machine
* `"GLMNET"`: Generalised Linear Models
* `"MAXENT"`: Maximum Entropy
* `"SLDA"`: Scaled Linear Discriminant Analysis
* `"BOOSTING"`: Boosting
* `"BAGGING"`: Bagging
* `"RF"`: Random Forest 
* `"NNET"`: Neural Networks
* `"TREE"`: Classification Tree

`...` Variables to be passed to the classifier (eg. `kernel = "linear"` for SVM)

####Values:

`classAnalysis` is a dataframe with the precision, accuracy and recall for the classifier with k-fold evaluation.  Both the micro and macro values are returned.  This dataframe is also stored to disk as a csv with filename `'n'gram_'k'fold_'classifier'.csv` where the values in quotes are replaced by the user inputted values.

##Clustering
This is code,`clustering.R` to perform cluster evaluation for text documents.  It uses unigrams to cluster data using k-means, hierarchical agglomerative and expectation maximisation.  In each case, the code creates 10 clusters, to try and match the 10 manually labels of the preprocessed Reuters dataset.  For each clustering algorithm, a suitable graph is also plotted.

###Example Usage:
`clusAnalysis <- clustering(documents)`

####Arguments:
`documents` is the preprocessed dataframe.

####Values:
`clusAnalysis` is a dataframe with the algorithms and their Rand index.  
