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
############################# Naive Bayes Classification   #############################
########################################################################################
######################## Input Parameter: 1. Data Frame     ############################
######################## 				  2. Dependent Var Name         ################
######################## 				  3. No of K-Fold Cross Validation    ##########
######################## 				  4. No of Repeated Cross Validation  ##########
######################## 				  5. Seed                             ##########
######################## Output: 	1. Final Model Object                     ##########
########################################################################################
model_naive_bayes <- function(df_name, dep_var, n_cv, n_repeats, seed){
  
  pkgTest("caret")
  pkgTest("e1071")
  pkgTest("doMC") ##### install.packages("doMC", repos="http://R-Forge.R-project.org")
  pkgTest("foreach")
  
  x <- df_name[, -which(names(df_name) %in% dep_var)]
  y <- df_name[, which(names(df_name) %in% dep_var)]
  
  ##################### Setting Grid Parameters ######################
  set.seed(seed)
  control <- caret::trainControl(method="repeatedcv", 
                                 number=n_cv, 
                                 repeats=n_repeats,
                                 verboseIter = TRUE,
                                 summaryFunction=defaultSummary)
  tunegrid <- expand.grid(.fL=c(0,0.5,1.0), 
                          .usekernel=c(TRUE,FALSE),
                          .adjust = c(0,0.5,1.0))
  
  ##################### Grid Search ######################
  registerDoMC(cores = detectCores())
  nb_gridsearch <- caret::train(x, y, method = "nb",
                                  metric = ifelse(is.factor(y), "Kappa", "RMSE"),
                                  maximize = ifelse(is.factor(y), TRUE, FALSE),
                                  trControl = control, 
                                  tuneGrid = tunegrid,
                                  preProcess = c("center", "scale"),
                                  tuneLength = 10)
  
  ##################### Finalizing Model ######################
  return(nb_gridsearch$finalModel)
}