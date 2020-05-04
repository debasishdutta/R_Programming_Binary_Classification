########################################################################################
####################### Making Packages Available For R Session ########################
########################################################################################
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

########################################################################################
###################################### Random Forest ###################################
########################################################################################
######################## Input Parameter: 1. Data Frame     ############################
######################## 				  2. Dependent Var Name         ################
######################## 				  3. No of K-Fold Cross Validation    ##########
######################## 				  4. No of Repeated Cross Validation  ##########
######################## 				  5. Seed                             ##########
######################## Output: 	1. Random Forest Model Object             ##########
########################################################################################
model_rf <- function(df_name, dep_var, n_cv, n_repeats, seed){
  
  pkgTest("randomForest")
  pkgTest("caret")
  pkgTest("doMC") ##### install.packages("doMC", repos="http://R-Forge.R-project.org")
  pkgTest("foreach")
  
  x <- df_name[, -which(names(df_name) %in% dep_var)]
  y <- df_name[, which(names(df_name) %in% dep_var)]
  
  ###################### Setting Grid Parameters ######################
  set.seed(seed)
  control <- caret::trainControl(method="repeatedcv", 
                                 number=n_cv, 
                                 repeats=n_repeats, 
                                 classProbs =  TRUE,
                                 verboseIter = TRUE,
                                 search="grid")
  mtry <- (floor(sqrt(ncol(df_name)-1))-5):(floor(sqrt(ncol(df_name)-1))+5)
  mtry <- mtry[which(mtry >1)]
  tunegrid <- expand.grid(.mtry=mtry)
  
  ###################### Grid Search ######################
  registerDoMC(cores = detectCores())
  rf_gridsearch <- caret::train(x, y, method = "rf", 
                         metric = ifelse(is.factor(y), "Kappa", "RMSE"),
                         maximize = ifelse(is.factor(y), TRUE, FALSE),
                         prob.model=TRUE,
                         trControl = control, 
                         tuneGrid = tunegrid,
                         tuneLength = 10,
                         verbose = TRUE)
  best_mtry <- as.numeric(rf_gridsearch$bestTune)
  tunegrid_final <- expand.grid(.mtry=best_mtry)
  
  ###################### Optimizing ntree ######################
  modellist <- list()
  ntree_list <- c(seq(from= 100, to=1500, by=100))
  
  modellist <- foreach(i=1:length(ntree_list)) %dopar% {
  set.seed(seed)
  fit <- caret::train(x, y, method = "rf", 
                      metric = ifelse(is.factor(y), "Kappa", "RMSE"),
                      maximize = ifelse(is.factor(y), TRUE, FALSE),
                      trControl = control, 
                      tuneGrid = tunegrid_final,
                      tuneLength = ifelse(trControl$method == "none", 1, 3),
                      ntree=ntree_list[i],
                      verbose = TRUE)
    modellist[[i]] <- fit
  }
  
  ###################### Finalizing Model ######################
  results <- caret::resamples(modellist)
  temp_kappa <- as.data.frame((summary(results))$statistics$Kappa)
  temp_Accuracy <- as.data.frame((summary(results))$statistics$Accuracy)
  model_index <- which(temp_kappa$Mean ==max(temp_kappa$Mean))[1]
  final_model <- modellist[[model_index]]
  return(final_model$finalModel)
}
  