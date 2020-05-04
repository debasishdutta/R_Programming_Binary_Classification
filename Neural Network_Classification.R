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
############## Neural Network (Classification Porblem Only)   ##########################
########################################################################################
######################## Input Parameter: 1. Data Frame     ############################
######################## 				  2. Dependent Var Name         ################
######################## 				  3. No of K-Fold Cross Validation    ##########
######################## 				  4. No of Repeated Cross Validation  ##########
######################## 				  5. Seed                             ##########
######################## Output: 	1. Final Model Object                     ##########
########################################################################################
model_neural_network <- function(df_name, dep_var, n_cv, n_repeats, seed){
  
  pkgTest("caret")
  pkgTest("nnet")
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
  tunegrid <- expand.grid(.size = c(1:floor(nrow(x)/ncol(x))),
                          .decay = seq(0,4,0.125))
  
  ##################### Grid Search ######################
  registerDoMC(cores = detectCores())
  nnet_gridsearch <- caret::train(x, y, method = "nnet",
                                  algorithm = "backprop",
                                  prob.model=TRUE,
                                  metric = ifelse(is.factor(y), "Kappa", "RMSE"),
                                  maximize = ifelse(is.factor(y), TRUE, FALSE),
                                  trControl = control, 
                                  tuneGrid = tunegrid,
                                  preProcess = c("center", "scale"),
                                  tuneLength = 10,
                                  verbose = TRUE)
  
  ##################### Finalizing Model ######################
  return(nnet_gridsearch$finalModel)
}