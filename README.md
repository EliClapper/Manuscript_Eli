# Manuscript_Eli
 Manuscript with Reproducible Workflow and HTML/md markup.

 This manuscript is a miniversion of my thesis which shows my skills in markup languages and doing research in a reproducible manner.

 It is mini in the sense that I do not calculate all text features/predictors (6 instead of 96) or use all algorithms that I would in the complete version (5 instead of 8). I edited this manuscript so that this does not show (most of the time), altough the introduction talks about overfitting and multicollinearity which is with 6 features on 970 observations less of an issue. 
 The results section are also shortened to only include the essential information and there is no discussion.
 
 If you are interested in my full thesis regarding the subject, please let me know and I will send it your way as soon as it is finished.


 # Modules and packages
 1. To run the R script the following R packages are required: `stringr` version (1.4.0), `stats` version (4.1.2), `glmnet` (version 4.3)
 2. To knit the manuscript to html, 'DT' version (0.20) is required
 3. To run python within Rstudio, library `reticulate` (version 1.22) is required.
 4. To run the Python script, the following python modules are required: `pandas` version (1.3.0), `numpy` version (1.21.0), and `rouge_score` version (0.0.4). 
 Do note that `rouge_score` is a third party module that requires online installation.
