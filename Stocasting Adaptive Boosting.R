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
########################### Stocasting Adaptive Boosting  ##############################
########################################################################################
######################## Input Parameter: 1. Data Frame     ############################
######################## 				  2. Dependent Var Name         ################
######################## 				  3. No of K-Fold Cross Validation   ###########
######################## 				  4. No of Repeated Cross Validation ###########
######################## 				  5. Seed                            ###########
######################## Output: 	1. Final Model Object                    ###########
########################################################################################
model_ada_boost <- function(df_name, dep_var, n_cv, n_repeats, seed){
  
  pkgTest("caret")
  pkgTest("ada")
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
  tunegrid <- expand.grid(iter= 100,
                          maxdepth = depth, 
                          nu = c(0.5,1,2))
  
  ##################### Grid Search ######################
  registerDoMC(cores = detectCores())
  ada_gridsearch <- caret::train(x, y, method = "ada", 
                                 metric = ifelse(is.factor(y), "Kappa", "RMSE"),
                                 maximize = ifelse(is.factor(y), TRUE, FALSE),
                                 trControl = control, 
                                 tuneGrid = tunegrid,
                                 verbose = TRUE)
  
  ##################### Finalizing Model ######################
  return(ada_gridsearch$finalModel)
}

