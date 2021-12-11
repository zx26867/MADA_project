# Overview

The dataset I obtained is about jobs and salaries. The dataset contains variables including:
0 - age group
1 - industry
2 - job title
3 - salary
4 - country
5 - state
6 - years of professional experience
7 - total years of experience
8 - education level
9 - gender
10 - race

Most variables are categorical variables. Some of them are ordinal, such as age group, years of experience, which are categorical in nature but also have comparative order. The only numeric data is the outcome of interest, which is salary.

My Questions:
1. Is it possible to predict a job's salary based on these variables?
2. Which model results in the best performance in terms of RMSE?


# Dataset Link
https://oscarbaruffa.com/messy/


# Running the analysis

First, run code/processing_code/processingscript.R, which contains dataset loading, cleaning up and exploratory analysis.
Then, run code/processing_code/analysis_code/analysisscript.R, which performs the modeling analysis.
Finally, knit products/manuscript/Manuscript.Rmd to get the final manuscript.

