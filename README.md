# vignette-UnsupervisedDBScan

Your README file should contain five main pieces of information in the following order:

A one-sentence description at the very top before any (sub)headers:

Vignette on implementing density-based clustering on three class classification of Synthetic Personality Traits using DBScan, KMeans, and LogReg in R, created for PSTAT 197A.

Contributors: Aarti Garaye, Josephine Kaminaga, Jimmy Wu, Nicole Xu

Vignette abstract: a brief description in a few sentences of your vignette topic, example data, and outcomes.

Repository contents: an explanation of the directory structure of the repository

Reference list: 2 or more references to learn more about your topic.
Martin Ester, Hans-Peter Kriegel, Jörg Sander, and Xiaowei Xu. 1996. A density-based algorithm for discovering clusters in large spatial databases with noise. In Proceedings of the Second International Conference on Knowledge Discovery and Data Mining (KDD'96). AAAI Press, 226–231.

A typical README file would also contain instructions on use and instructions on contributing to the repository.

# Project Brainstorming
We will be brainstorming the specific prompt & the relevant tasks for our project.

## 1. What structure exists in this dataset, and how well can we predict an important outcome using both unsupervised (DBSCAN, K-means) and supervised (logistic regression, XGBoost) methods?
For this potential topic, the skeleton can be roughly divided into four sections (and so each of us may pick one if we decided to go forward):

- **Section 1: Data / Feature Exploration and transition to unsupervised clustering**. Conduct exploratory data analysis (identify key features for clustering and prediction) and conduct DBSCAN.
- **Section 2: Unsupervised Clustering**. Run K-means, compare DBSCAN and K-means, visualize clusters.
- **Section 3: Supervised Prediction**. Build logistic regression and XGBoost classifiers. Report accuracy.
- **Section 4: Integration & Comparison**. Conduct cluster-to-target analysis and combine the findings.

The above proposed tasks are not independent (but I've made sure that each person is able to meaningfully participate), so one person may need to wait for another to finish their task in order to proceed. Alternatively, we can also split the task into four independent sections, to which each of us can work independently.

## Aarti Update Today
I did some basic EDA, I think it was best to do it as a .RMD since it had a lot of visuals and rendering them as html helped and now we can just copy paste :)

Results of EDA: 
- There is high correlation between predictor variables so we need to include some interaction terms when modelling.
- The classes are pretty balanced so we don't need to use random strata for that
- ANOVA test tells that the most important variable is party-liking.
- Varibales that don't contribute much are creativity, stress-handling, and emotional-stability so we can drop those and the interaction terms with those

What still needs to be done
- I'm not sure why DBScan was in the EDA section but I didn't do that, feel free to add anything you think is worth it to have in the EDA section.
