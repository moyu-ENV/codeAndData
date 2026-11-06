#####requirements#####
#R version 4.4.2
#xgboost_1.7.10.1 
######################

if(TRUE){
  rm(list = ls())  
  
  ############
  #data
  ############
  if(TRUE){  
    library(dplyr)

    wd <- paste0("C:/YM/CHES/TCMangroveRecovery/") #local
  
    train_std <- read.csv(paste0(wd,'/gitHub/xgb_train.csv'),stringsAsFactors = FALSE)
    test_std <- read.csv(paste0(wd,'/gitHub/xgb_test.csv'),stringsAsFactors = FALSE)
    
    y <- 'bestAIC'
    
    allXs <- c( 'disturbanceMax', 
                "proxCoastM","coastalSlope","stormFreq","forestPopulation", 
                "areaM2", "preEVI","canopyHeight","speciesN", 
                "meanTemp1.6mo","maxTemp1.6mo", "rainfallCHIRPS6.180d")
    sd <- 1234
    }
  
  
  
    #xgb
    if(TRUE){
      train_std_xgb <- train_std
      train_std_xgb$bestAIC <-as.numeric(as.factor(train_std_xgb$bestAIC))-1
      
      test_std_xgb <- test_std
      test_std_xgb$bestAIC <-as.numeric(as.factor(test_std_xgb$bestAIC))-1
      
      
      dtrain <- xgboost::xgb.DMatrix(data = data.matrix(train_std_xgb[, allXs]),
                                     label = train_std_xgb[, y])
      
      params <- list(objective = 'multi:softprob',
                     eval_metric ='auc', 
                     num_class = 4,
                     booster = "gbtree",
                     eta = 0.17, 
                     gamma = 1, 
                     alpha = 1, 
                     lambda =1,
                     max_depth = 17,
                     min_child_weight =2,
                     subsample = 0.6,
                     colsample_bytree = 0.7,
                     colsample_bylevel =0.9,
                     colsample_bynode = 0.9,
                     nthread = 1)
      
      set.seed(sd)
      model_xgb_bst <- xgboost::xgb.train(data = dtrain, 
                                          params = params,
                                          nrounds=70,
                                          verbose = FALSE)
      
      pred_train_prob <- predict(object = model_xgb_bst,data.matrix(train_std_xgb[,allXs])) 
      pred_test_prob <- predict(object = model_xgb_bst,data.matrix(test_std_xgb[,allXs])) 
      
      pred_train_bst <- max.col(matrix(pred_train_prob, ncol = 4, byrow = FALSE))-1
      pred_test_bst  <- max.col(matrix(pred_test_prob,  ncol = 4, byrow = FALSE))-1
      
      caret::confusionMatrix(as.factor(pred_train_bst),as.factor(train_std_xgb[,y]))
      caret::confusionMatrix(as.factor(pred_test_bst),as.factor(test_std_xgb[,y]))
}

}







