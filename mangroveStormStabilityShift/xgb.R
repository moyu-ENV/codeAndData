#####requirements#####
#R version 4.4.2
#xgboost_1.7.10.1 
######################
rm(list = ls())  
library(dplyr)

wd <- paste0('C:/YM/CHES/TCMangroveRe/github/') #local
   
data <- read.csv(paste0(wd,'/input_GMW.csv'),stringsAsFactors = FALSE)
    
allYs <- c('initialResistance','reactionRate','recoveryRate')
allXs <- c('windSpeed','proxTrack','sideTrack','travelSpeed','rainfall',
               'coastalSlope','proxCoast','stormFreq',"forestPopulation",
               'patchSize','preEVI','canopyHeight','speciesN',
               "postTmean","postTmax","postRainfall")
    
data <- data[,c('sampleID',allYs,allXs)]
   
#quantile normalization
if(TRUE){
      data_qm <- data
      allXs_qNorm <- allXs[-which(allXs%in%c('sideTrack'))]
      
      for(varN in 1:length(c(allYs,allXs_qNorm))){
      #varN <- 2
      var <- c(allYs,allXs_qNorm)[varN]
      hist(data[,var],main=paste0(var,', original'))  
        
      orderNorm_obj <-   bestNormalize::orderNorm(data[,var])  
      data_qm[,var] <- predict(orderNorm_obj)
      hist(data_qm[,var],main=paste0(var,', qm')) 
      x2 <- predict(orderNorm_obj, newdata = data_qm[,var], inverse = TRUE)
      hist(x2,main=paste0(var,', reverse'))
      print(paste0(var,' reversed? ',all.equal(x2, data[,var])))
      }
     }
#n=31612
    
#remove outliers
if(TRUE){
      data_ol <- data_qm
    
      allXs_outLiers <- allXs[-which(allXs%in%c('sideTrack'))]
      
      for(var in allXs_outLiers){  
        upcut <- 0.75
        lowcut <- 0.25
        wdth <- 3 
        
        q25 <- quantile(data_ol[,var],lowcut,na.rm = TRUE)
        q75 <- quantile(data_ol[,var],upcut,na.rm = TRUE)
        
        IQR <- q75-q25
        mn <- q25 - wdth*IQR
        mx <- q75 + wdth*IQR
        data_ol <- data_ol[which(data_ol[,var]>=mn & data_ol[,var]<=mx),]
        
        print(paste0('After ',var,':',nrow(data_ol)))
      }
    }
#n=20212 

sd <- 1234
  
#initial resistance 
if(TRUE){
y <- 'initialResistance'
feed <-data_ol[,c('sampleID',y,allXs)]
feed <- feed[complete.cases(feed),]
      
colnames(feed)[2]<-'y'
feed$prob <- 0
feed[which(feed$y > median(feed$y,na.rm=TRUE)),'prob']<-1
#n=20212
      
#split
if(TRUE){
          splt <- 10
          
          set.seed(sd);testRows0<-  sample(which(feed$prob==0), round(nrow(feed[which(feed$prob==0),])/splt,digits = 0), replace=FALSE)
          set.seed(sd);testRows1<-  sample(which(feed$prob==1), round(nrow(feed[which(feed$prob==1),])/splt,digits = 0), replace=FALSE)
          
          test_std <- feed[c(testRows0,testRows1),]
          train_std <- feed[-c(testRows0,testRows1),]
        }
     
#best
if(TRUE){
        train_std_xgb <- train_std
        test_std_xgb <- test_std
        
         dtrain <- xgboost::xgb.DMatrix(data = data.matrix(train_std_xgb[, allXs]), label = train_std_xgb[, 'prob'])
        dvalid <- xgboost::xgb.DMatrix(data = data.matrix(test_std_xgb[, allXs]),label = test_std_xgb[, 'prob'])
        
        params <- list(objective = 'binary:logistic',
                       eval_metric ='auc', 
                       booster = "gbtree",
                       eta = 0.05, 
                       gamma = 1, 
                       alpha = 1, 
                       lambda =2,
                       max_depth = 17,
                       min_child_weight =2,
                       subsample = 0.9,
                       colsample_bytree = 0.7,
                       colsample_bylevel =0.95,
                       colsample_bynode = 0.8,
                       nthread = 1)
        
        set.seed(sd)
        model_xgb_bst <- xgboost::xgb.train(data = dtrain, 
                                            params = params,
                                            nrounds=300,
                                            evals = list(validation = dvalid),
                                            early_stopping_rounds = 10,
                                            verbose = 0)
        

        pred_train_xgb <- predict(object = model_xgb_bst,data.matrix(train_std_xgb[,allXs])) 
        pred_test_xgb <- predict(object = model_xgb_bst,data.matrix(test_std_xgb[,allXs])) 
        
        pROC::roc(train_std_xgb$prob, pred_train_xgb)$auc
        pROC::roc(test_std_xgb$prob, pred_test_xgb)$auc
 }        

#SHAP        
xgbShap <- predict(model_xgb_bst, dtrain, predcontrib = TRUE, strict_shape = TRUE)
shapV <- data.frame(sampleID=train_std_xgb$sampleID) 
  for(vvN in 1:length(allXs)){
            #vvN <- 1
            shapV[,allXs[vvN]]<-  xgbShap[,,vvN]
         }
          
write.csv(shapV,paste0(wd,'/output_shapV_',y,'.csv'),row.names = FALSE)
}

#reaction rate 
if(TRUE){
  y <- 'reactionRate'
  feed <-data_ol[,c('sampleID',y,allXs)]
  feed <- feed[complete.cases(feed),]
  
  colnames(feed)[2]<-'y'
  feed$prob <- 0
  feed[which(feed$y > median(feed$y,na.rm=TRUE)),'prob']<-1
  #n=10209
  
  #split
  if(TRUE){
    splt <- 10
    
    set.seed(sd);testRows0<-  sample(which(feed$prob==0), round(nrow(feed[which(feed$prob==0),])/splt,digits = 0), replace=FALSE)
    set.seed(sd);testRows1<-  sample(which(feed$prob==1), round(nrow(feed[which(feed$prob==1),])/splt,digits = 0), replace=FALSE)
    
    test_std <- feed[c(testRows0,testRows1),]
    train_std <- feed[-c(testRows0,testRows1),]
  }
  
  #best
  if(TRUE){
    train_std_xgb <- train_std
    test_std_xgb <- test_std
    
    dtrain <- xgboost::xgb.DMatrix(data = data.matrix(train_std_xgb[, allXs]), label = train_std_xgb[, 'prob'])
    dvalid <- xgboost::xgb.DMatrix(data = data.matrix(test_std_xgb[, allXs]),label = test_std_xgb[, 'prob'])
    
    params <- list(objective = 'binary:logistic',
                   eval_metric ='auc', 
                   booster = "gbtree",
                   eta = 0.06, 
                   gamma = 1.5, 
                   alpha = 0.5, 
                   lambda =0,
                   max_depth = 16,
                   min_child_weight =5,
                   subsample = 0.95,
                   colsample_bytree = 0.9,
                   colsample_bylevel =0.75,
                   colsample_bynode = 0.5,
                   nthread = 1)
    
    
    set.seed(sd)
    model_xgb_bst <- xgboost::xgb.train(data = dtrain, 
                                        params = params,
                                        nrounds=300,
                                        evals = list(validation = dvalid),
                                        early_stopping_rounds = 10,
                                        verbose = 0)
    
    
    pred_train_xgb <- predict(object = model_xgb_bst,data.matrix(train_std_xgb[,allXs])) 
    pred_test_xgb <- predict(object = model_xgb_bst,data.matrix(test_std_xgb[,allXs])) 
    
    pROC::roc(train_std_xgb$prob, pred_train_xgb)$auc
    pROC::roc(test_std_xgb$prob, pred_test_xgb)$auc
  }        
  
  #SHAP        
  xgbShap <- predict(model_xgb_bst, dtrain, predcontrib = TRUE, strict_shape = TRUE)
  shapV <- data.frame(sampleID=train_std_xgb$sampleID) 
  for(vvN in 1:length(allXs)){
    #vvN <- 1
    shapV[,allXs[vvN]]<-  xgbShap[,,vvN]
  }
  
  write.csv(shapV,paste0(wd,'/output_shapV_',y,'.csv'),row.names = FALSE)
}

#recovery rate 
if(TRUE){
  y <- 'recoveryRate'
  feed <-data_ol[,c('sampleID',y,allXs)]
  feed <- feed[complete.cases(feed),]
  
  colnames(feed)[2]<-'y'
  feed$prob <- 0
  feed[which(feed$y > median(feed$y,na.rm=TRUE)),'prob']<-1
  #n=20212 
  
  #split
  if(TRUE){
    splt <- 10
    
    set.seed(sd);testRows0<-  sample(which(feed$prob==0), round(nrow(feed[which(feed$prob==0),])/splt,digits = 0), replace=FALSE)
    set.seed(sd);testRows1<-  sample(which(feed$prob==1), round(nrow(feed[which(feed$prob==1),])/splt,digits = 0), replace=FALSE)
    
    test_std <- feed[c(testRows0,testRows1),]
    train_std <- feed[-c(testRows0,testRows1),]
  }
  
  #best
  if(TRUE){
    train_std_xgb <- train_std
    test_std_xgb <- test_std
    
    dtrain <- xgboost::xgb.DMatrix(data = data.matrix(train_std_xgb[, allXs]), label = train_std_xgb[, 'prob'])
    dvalid <- xgboost::xgb.DMatrix(data = data.matrix(test_std_xgb[, allXs]),label = test_std_xgb[, 'prob'])
    
    params <- list(objective = 'binary:logistic',
                   eval_metric ='auc', 
                   booster = "gbtree",
                   eta = 0.01, 
                   gamma = 1, 
                   alpha = 0, 
                   lambda =1,
                   max_depth = 13,
                   min_child_weight =1,
                   subsample = 0.7,
                   colsample_bytree = 0.7,
                   colsample_bylevel =0.95,
                   colsample_bynode = 0.9,
                   nthread = 6,
                   tree_method="hist",
                   grow_policy = "lossguide",max_leaves  = 1024)
    
    set.seed(sd)
    model_xgb_bst <- xgboost::xgb.train(data = dtrain, 
                                        params = params,
                                        nrounds=500,
                                        evals = list(validation = dvalid),
                                        early_stopping_rounds = 10,
                                        verbose = 0)
    
    
    pred_train_xgb <- predict(object = model_xgb_bst,data.matrix(train_std_xgb[,allXs])) 
    pred_test_xgb <- predict(object = model_xgb_bst,data.matrix(test_std_xgb[,allXs])) 
    
    pROC::roc(train_std_xgb$prob, pred_train_xgb)$auc
    pROC::roc(test_std_xgb$prob, pred_test_xgb)$auc
  }        
  
  #SHAP        
  xgbShap <- predict(model_xgb_bst, dtrain, predcontrib = TRUE, strict_shape = TRUE)
  shapV <- data.frame(sampleID=train_std_xgb$sampleID) 
  for(vvN in 1:length(allXs)){
    #vvN <- 1
    shapV[,allXs[vvN]]<-  xgbShap[,,vvN]
  }
  
  write.csv(shapV,paste0(wd,'/output_shapV_',y,'.csv'),row.names = FALSE)
}



