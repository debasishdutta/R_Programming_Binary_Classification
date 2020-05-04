########################################################################################
####################### Making Packages Available For R Session ########################
########################################################################################
pkgTest <- function(x){
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

########################################################################################
########################## Linear Support Vector Machine  ##############################
########################################################################################
######################## Input Parameter: 1. Data Frame     ############################
######################## 				  2. Dependent Var Name         ################
######################## 				  3. No of K-Fold Cross Validation   ###########
######################## 				  4. No of Repeated Cross Validation ###########
######################## 				  5. Seed                            ###########
######################## Output: 	1. Final Model Object                    ###########
########################################################################################
model_linear_svm <- function(df_name, dep_var, n_cv, n_repeats, seed){
  
  pkgTest("caret")
  pkgTest("e1071")
  pkgTest("kernlab")
  pkgTest("doMC") ##### install.packages("doMC", repos="http://R-Forge.R-project.org")
  pkgTest("foreach")
  
  x <- df_name[, -which(names(df_name) %in% dep_var)]
  y <- df_name[, which(names(df_name) %in% dep_var)]
  
  ##################### Setting Grid Parameters ######################
  set.seed(seed)
  control <- caret::trainControl(method="repeatedcv", 
                                 number=n_cv, 
                                 repeats=n_repeats,
                                 classProbs =  TRUE,
                                 verboseIter = TRUE,
                                 summaryFunction=defaultSummary)
  tunegrid <- expand.grid(C = c(seq(0.01, 0.05, 0.01), seq(0.1,5,0.1)))
  
  ##################### Grid Search ######################
  registerDoMC(cores = detectCores())
  linear_svm_gridsearch <- caret::train(x, y, method = "svmLinear", 
                                        metric = ifelse(is.factor(y), "Kappa", "RMSE"),
                                        maximize = ifelse(is.factor(y), TRUE, FALSE),
                                        prob.model=TRUE,
                                        trControl = control, 
                                        tuneGrid = tunegrid,
                                        preProcess = c("center", "scale"),
                                        tuneLength = 10,
                                        verbose = TRUE)
  
  ##################### Finalizing Model ######################
  return(linear_svm_gridsearch$finalModel)
}