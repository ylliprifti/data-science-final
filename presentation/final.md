Coursera Data Science Capstone Project
========================================================
author: Ylli Prifti
date: 25 Nov 2017
autosize: true

Word Prediction Engine

Project Details
========================================================

**The problem:**
---------------
Given a large ammount of data scraped from social networks, blogs and news sites, build a prediction engine that completes the current word when typing, and predicts the next word.

**The challanges:**
------------------
Large dataset, insufficent space, working memory and processing power. Building a fast algorthm that outputs results with near real time interaction. The final predictions must be usable.


The solution
========================================================

**Data Loading:**
-----------------
Low level C++ file processing, to fast read total number of lines on each file and only load into memory a random percentage of the file.

**NLP Engine:**
-----------------
Use a combination of R-Packages to tranform and clean the data and build NGrams matrixes

**Search Algorithm:**
---------------------
Fast pattern matching algorthm for completing current word and predicting the next one.


Word Cloud
========================================================

![word cloud](final-figure/word-cloud.png)



How to run the application
========================================================

**To test the application follow this link:** [https://ylli.shinyapps.io/WordPrediction/]

**To clone the source of the app follow this link:** [https://github.com/ylliprifti/data-science-final]


