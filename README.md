# R_Programming_Classification_ML_Models
Binary/ Multi-Class Classification Machine Learning Models In R Programming

Disclaimer: 
This code snippet is developed with an intension for generic use only. I hereby declare that the code was not part of any of my professional development work. Also, no sensitive or confidential data sources are used for this development. 

Description: 
The repository consists of below list of machine learning algorithms: 
1. Neural Network 
2. Linear SVM 
3. Polynomial SVM 
4. Radial SVM 
5. Ada-Boost 
6. Gradient Boost 
7. Random Forest 
8. Naïve Bayes 

Each of these machine learning algorithms will take following inputs: 
1. Modelling Data as R Data Frame 
2. Name of dependent variable as character 
3. No of Cross Validation required to be performed 
4. No of repeats to be Cross Validated 
5. Seed for repetition of the identical results 
Each algorithm will return best fitted model object from its respective genre using Repeated Cross Validation technique. These model objects can be used for scoring the newly available data. 

Note: 
1. These scripts can be best used for binary/ multi class classification problem. 
2. All independent variables should be numeric in nature. If any of the independent variable is of either factor or character data type then the variable needs to be dummy coded. This is because these algorithms will normalize predictors so that best result can be achieved. 
3. The dependent variable should be of factor data type. 
4. Depending on the size of the modelling data, the algorithm can take large time to be completely executed. Third and fourth parameter should be chosen carefully. 

Steps For Execution:

1. Copy the code file to the current working directory of R session. 
2. Import the data in to a R data frame (df_name). Kindly ensure the dependent variable is stored as factor data type and all the independent variable are stored as numeric data type. 
3. Execute the below commands: 

  df_name <- iris 

  dep_var <- "Species" 

  n_cv <- 5 

  n_repeats <- 2 

  seed <- 1234 

  source("Linear Support Vector.R") 

  source("Neural Network_Classification.R") 

  source("Polynomial Support Vector.R") 

  source("Radial Support Vector.R") 

  source("Random Forest.R") 

  source("Stocasting Gradient Boosting.R") 

  source("Stocasting Adaptive Boosting.R") 

  source("Naive Bayes Classifier.R") 

  x1 <- model_linear_svm(df_name, dep_var, n_cv, n_repeats, seed) 

  x1_pred <- predict(x1, df_name[,-5], type="prob") 

  x2 <- model_neural_network(df_name, dep_var, n_cv, n_repeats, seed) 

  x2_pred <- predict(x2, df_name[,-5], type="raw") 

  x3 <- model_polynomial_svm(df_name, dep_var, n_cv, n_repeats, seed) 

  x3_pred <- predict(x3, df_name[,-5], type="prob") 

  x4 <- model_radial_svm(df_name, dep_var, n_cv, n_repeats, seed) 

  x4_pred <- predict(x4, df_name[,-5], type = c("prob")) 

  x5 <- model_rf(df_name, dep_var, n_cv, n_repeats, seed) 

  x5_pred <- predict(x5, df_name[,-5], type = c("prob")) 

  x6 <- model_gbm(df_name, dep_var, n_cv, n_repeats, seed) 

  x6_pred <- predict(x6, df_name[,-5], n.trees= x6$n.trees, type = 'response') 

  x7 <- model_ada_boost(df_name, dep_var, n_cv, n_repeats, seed) 

  x7_pred <- predict(x7, df_name[,-5], type = c("prob")) 

  x8 <- model_naive_bayes(df_name, dep_var, n_cv, n_repeats, seed) 

  x8_pred <- as.data.frame(predict(x8, df_name[,-5], type = c("raW"))$posterior) 


Compatibility: 
The code is developed and tested on RStudio (Version 1.0.44) using R-3.3.2
