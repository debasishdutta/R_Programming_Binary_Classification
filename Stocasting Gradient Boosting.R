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
########################### Stocasting Gradient Boosting  ##############################
########################################################################################
######################## Input Parameter: 1. Data Frame     ############################
######################## 				  2. Dependent Var Name         ################
######################## 				  3. No of K-Fold Cross Validation   ###########
######################## 				  4. No of Repeated Cross Validation ###########
######################## 				  5. Seed                            ###########
######################## Output: 	1. Final Model Object                    ###########
########################################################################################
model_gbm <- function(df_name, dep_var, n_cv, n_repeats, seed){
  
  pkgTest("caret")
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
  depth <- seq(floor(sqrt(ncol(df_name)-1))-5,floor(sqrt(ncol(df_name)-1)),1)
  depth <- depth[which(depth >=2)]
  tunegrid <- expand.grid(n.trees=c(seq(from= 100, to=1500, by=100)),
                          interaction.depth = depth, shrinkage = c(0.00001, 0.0001, 0.001),
                          n.minobsinnode=max(floor(nrow(x)*0.005),10))
  
  ##################### Grid Search ######################
  registerDoMC(cores = detectCores())
  gbm_gridsearch <- caret::train(x, y, method = "gbm", 
                          metric = ifelse(is.factor(y), "Kappa", "RMSE"),
                          maximize = ifelse(is.factor(y), TRUE, FALSE),
                          trControl = control, 
                          tuneGrid = tunegrid,
                          verbose = TRUE)
  
  ##################### Finalizing Model ######################
  return(gbm_gridsearch$finalModel)
}
  
  