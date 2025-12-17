#####requirments#####
#R version 4.4.2
#caret_7.0-1 (for knn)
#e1071_1.7.16  (for svm)
#gbm_2.2.2 
#randomForest_4.7.1.2
#rpart_4.1.23
#xgboost_1.7.10.1 
#####################
rm(list = ls())
  
#self define function(s)
if(TRUE){  
combineFiles <- function(fileList0,d=1){
      #fileList0 <- paste0(wd_para,fl[((col-1)*factorial(nXs-1)+1):((col-1)*factorial(nXs-1)+factorial(nXs-1))])
      for (f in 1:1) {
        data<-read.csv(fileList0[f],stringsAsFactors = FALSE)
        data_all<-data
      }
      if(d==1){
        for (f in 2:length(fileList0)) {
          data<-read.csv(fileList0[f],stringsAsFactors = FALSE)
          data_all<-rbind(data_all,data)
        }
      }
      if(d==2){
        for (f in 2:length(fileList0)) {
          data<-read.csv(fileList0[f],stringsAsFactors = FALSE)
          data_all<-cbind(data_all,data)
        }
      }
      for (f in 1:length(fileList0)){
        fn <- paste0(fileList0[f])
        file.remove(fn)  
      }
      return(data_all)
    }#end of combineFiles
}  

library(dplyr)
library(parallel)
 
#input
if(TRUE){  
wd <- paste0("C:/YM/CHES/TCMangroveModel") 
   
data <- read.csv(paste0(wd,'/result/model/input.csv'),stringsAsFactors = FALSE)
    
#no_cores<-detectCores()-1 
no_cores<-4
wd_para<-paste0(wd,"/result/model/outputOOB/")
    
    
#format input  
if(TRUE){
idcols <- c('sampleID')
      
data[which(data$sd=='sr'),'side'] <- 1
data[which(data$sd=='wl'),'side'] <- 0
}
    
#pick models
mdls <- c('rf')#'gbm','knn','rpart','svm','rf','xgboost'
OOB <-TRUE  #perform one-out-of bag testing? 
impt <-TRUE #save variable importance? 
sd <- 1234  #set seed 

    
#set up variables
if(TRUE){
allYs <- c("damage")
      
allXs <- c("windSpeed","windSpeed5day","travelSpeed","rainfall","coastalSlope",'coastDist')
}
    
#adjust 0
for(var in allXs){
#var <-allXs[1]
if(min(data[,var],na.rm = TRUE)<=0){data[,var] <- data[,var]+abs(min(data[,var],na.rm = TRUE))+1} 
}
}
  

output_all <- data.frame()#data.frame(matrix(nrow=0,ncol=8+length(idcols)+length(allXs)))
#colnames(output_all) <- c('method','scale',"Y",idcols,"obs","obs_scl",allXs,"pred","pred_OOB")
    
varImp_all <- data.frame()
    
start_time <- Sys.time()
    
for(scl in c('log10')){
      #scl <-  'log10'
      print(scl)  
      for (y in allYs){
        #y <- allYs[1]  
        print(y)
        
        feed <- data[,c(idcols,y, allXs)]
        
        #transform input
        if(TRUE){
          feed_scl <- feed
          if(scl=='log10'){
            for(x in c(y,allXs)){
              feed_scl[,x] <- log10(feed_scl[,x])  
              feed_scl[,x] [which(!is.finite(feed_scl[,x] ))]<- NA  
            }
          }
          
          feed_scl[,y] [which(!is.finite(feed_scl[,y] ))]<- NA  
          feed_scl <- feed_scl[complete.cases(feed_scl), ] 
        }
     
        #standardize input
        feed_std <- feed_scl
        for (col in c(y,allXs)){
          #col <- 3
          feed_std[,col] <- feed_std[,col]-mean(feed_std[,col])   
          feed_std[,col] <- feed_std[,col]/sd(feed_std[,col])   
        }
        
        set.seed(sd) 
        testRows<- sample(1:nrow(feed_std), round(nrow(feed_std)/10,digits = 0), replace=FALSE)
        test_std <- feed_std[testRows,]
        train_std <- feed_std[-testRows,]
        
        #modeling
        formula <- as.formula(paste(y,"~", paste(allXs, collapse = " + ")))  
        
        if('gbm' %in% mdls){
          #gbm not support binary
          mth <- 'gbm'
          print(mth)
          
          #Tune
          if(FALSE){
            hyper_grid_gbm <- expand.grid(
              n.trees = c(100), #GBMs often require many trees (it is not uncommon to have many thousands of trees) but since they can easily overfit 
              interaction.depth = seq(8,13,1), #Typical 3–8. Smaller depth trees are computationally efficient (but require more trees).higher depth trees allow the algorithm to capture unique interactions but also increase the risk of over-fitting.
              shrinkage = seq(0.01,0.1,0.01), #0-1, typically 0.001 to 0.3.  the smaller this value, the more accurate the model can be but also will require more trees in the sequence
              n.minobsinnode = c(1,2),
              cv.folds = c(10),
              trainR2 = NA,  
              testR2 = NA)
            
            for(i in 1:nrow(hyper_grid_gbm)){
              set.seed(sd)
              model_gbm <- gbm::gbm(formula = formula,
                                    distribution = "gaussian",
                                    data = train_std,
                                    n.trees = hyper_grid_gbm$n.trees[i],
                                    interaction.depth = hyper_grid_gbm$interaction.depth[i],
                                    shrinkage = hyper_grid_gbm$shrinkage[i],
                                    n.minobsinnode = hyper_grid_gbm$n.minobsinnode[i],
                                    cv.folds = hyper_grid_gbm$cv.folds[i],
                                    n.cores = NULL, # will use all cores by default
                                    verbose = FALSE)
              
              pred_train_gbm <- predict(model_gbm, newdata = train_std)
              pred_test_gbm <- predict(model_gbm, newdata = test_std)
              
              #hyper_grid_xgb$trainR2[i] <- 1-sum((pred_train_xgboost-train_std[,y])^2)/sum((train_std[,y]-mean(train_std[,y]))^2)
              #hyper_grid_xgb$testR2[i] <- 1-sum((pred_test_xgboost-test_std[,y])^2)/sum((test_std[,y]-mean(test_std[,y]))^2)
              hyper_grid_gbm$trainR2[i] <- sum((pred_train_gbm-mean(train_std[,y]))^2)/ 
                (sum((pred_train_gbm-mean(train_std[,y]))^2) + sum((pred_train_gbm-train_std[,y])^2))
              hyper_grid_gbm$testR2[i] <-  sum((pred_test_gbm-mean(test_std[,y]))^2)/ 
                (sum((pred_test_gbm-mean(test_std[,y]))^2) + sum((pred_test_gbm-test_std[,y])^2))
            }
          }
          
          set.seed(sd)  
          model <- gbm::gbm(formula           = formula,
                            data              = feed_std,
                            distribution      = "gaussian",
                            n.trees           = 500,
                            interaction.depth = 45,
                            shrinkage         = 0.14,
                            n.minobsinnode    = 5,
                            cv.folds          = 10,
                            verbose           = FALSE)
          
          #mean(model$results$Rsquared)
          pred <- predict(object = model,feed_std[,c(allXs)])
          #plot(feed_std[,y],pred)
          
          if(impt){
            varImp <- data.frame(gbm::summary.gbm(model))
            colnames(varImp) <- c('var','imp')
            varImp$imp <- varImp$imp/max(varImp$imp)*100
            varImp$mth <- mth 
            
            varImp$y <-y 
            varImp$scl <-scl
            varImp_all <- rbind(varImp_all,varImp)
          }
          
          output<-data.frame(sampleID=feed_std$sampleID,obs=feed[,y], obs_std=feed_std[,y],pred=pred)
          output$pred_rcl <- 10^(output$pred*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$Y <- y
          output$scale <- scl
          output$method <- mth
          
          ###OOB
          if(OOB){ 
            cl <- makeCluster(no_cores)  
            clusterExport(cl,c("wd_para",'mth',"feed_std",'idcols','y','allXs','sd','formula'))
            
            parLapply(cl,1:nrow(feed_std),
                      function(ind){
                        #ind<-1
                        train_data <- feed_std[-ind,]
                        test_data <- feed_std[ind,]
                        
                        set.seed(sd)
                        model_OOB<-gbm::gbm(formula           = formula,
                                            data              = train_data,
                                            distribution      = "gaussian",
                                            n.trees           = 500,
                                            interaction.depth = 45,
                                            shrinkage         = 0.14,
                                            n.minobsinnode    = 5,
                                            cv.folds          = 10,
                                            verbose           = FALSE)
                        pred_OOB<-predict(object = model_OOB,test_data[,allXs])
                        
                        output_OOB<-data.frame(sampleID=test_data[,c(idcols)],pred_OOB=pred_OOB)
                        write.csv(output_OOB,file=paste0(wd_para,"predOOB",ind,".csv"),row.names = FALSE)
                      })
            stopCluster(cl)
            
            fileList <- list.files(path=paste0(wd_para), pattern=paste0("predOOB"))
            pred_OOB <- combineFiles(paste0(wd_para,fileList),1)
            
            output <- merge(output,pred_OOB,by=c(idcols))
            output$pred_OOB_rcl <- 10^(output$pred_OOB*sd(feed_scl[,y])+mean(feed_scl[,y]))
          }
          
          output_all <- rbind(output_all,output)
        }
        
        if('knn' %in% mdls){
          mth <- 'knn'
          print(mth)
          
          fitControl <- caret::trainControl(method = "cv", number = 10, savePredictions = 'final')
          
          #Tune 
          if(FALSE){
            hyper_grid_knn=data.frame(k=seq(1,50,1), 
                                      trainR2 = NA, 
                                      testR2 = NA) 
            
            for(i in 1:nrow(hyper_grid_knn)){
              #i<-2
              model_knn <- caret::train(train_std[,c(allXs)],train_std[,y],method=mth ,trControl=fitControl,verbos=FALSE,
                                        tuneGrid = data.frame(k =hyper_grid_knn$k[i]))
              
              pred_train_knn <- predict(model_knn, newdata = train_std)
              pred_test_knn <- predict(model_knn, newdata = test_std)
              
              #hyper_grid_xgb$trainR2[i] <- 1-sum((pred_train_xgboost-train_std[,y])^2)/sum((train_std[,y]-mean(train_std[,y]))^2)
              #hyper_grid_xgb$testR2[i] <- 1-sum((pred_test_xgboost-test_std[,y])^2)/sum((test_std[,y]-mean(test_std[,y]))^2)
              hyper_grid_knn$trainR2[i] <- sum((pred_train_knn-mean(train_std[,y]))^2)/ 
                (sum((pred_train_knn-mean(train_std[,y]))^2) + sum((pred_train_knn-train_std[,y])^2))
              hyper_grid_knn$testR2[i] <-  sum((pred_test_knn-mean(test_std[,y]))^2)/ 
                (sum((pred_test_knn-mean(test_std[,y]))^2) + sum((pred_test_knn-test_std[,y])^2))
            }
          }
          
          model <- caret::train(feed_std[,c(allXs)],feed_std[,y],method=mth ,trControl=fitControl, 
                                tuneGrid = data.frame(k=9),verbos=FALSE)
          #mean(model$results$Rsquared)
          pred <- predict(object = model,feed_std[,c(allXs)])
          #plot(feed_std[,y],pred)
          
          output<-data.frame(sampleID=feed_std$sampleID,obs=feed[,y], obs_std=feed_std[,y],pred=pred)
          output$pred_rcl <- 10^(output$pred*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$Y <- y
          output$scale <- scl
          output$method <- mth
          
          ###OOB
          if(OOB){ 
            cl <- makeCluster(no_cores)  
            clusterExport(cl,c("wd_para",'mth',"feed_std",'idcols','y','allXs','fitControl'))
            
            parLapply(cl,1:nrow(feed_std),
                      function(ind){
                        #ind<-1
                        train_data <- feed_std[-ind,]
                        test_data <- feed_std[ind,]
                        
                        model_OOB<- caret::train(train_data[,c(allXs)],train_data[,y],method=mth,trControl=fitControl, 
                                                 tuneGrid = data.frame(k=9),verbos=FALSE)
                        pred_OOB<-predict(object = model_OOB,test_data[,allXs])
                        
                        output_OOB<-data.frame(sampleID=test_data[,c(idcols)],pred_OOB=pred_OOB)
                        write.csv(output_OOB,file=paste0(wd_para,"predOOB",ind,".csv"),row.names = FALSE)
                      })
            stopCluster(cl)
            
            fileList <- list.files(path=paste0(wd_para), pattern=paste0("predOOB"))
            pred_OOB <- combineFiles(paste0(wd_para,fileList),1)
            
            output <- merge(output,pred_OOB,by=c(idcols))
            output$pred_OOB_rcl <- 10^(output$pred_OOB*sd(feed_scl[,y])+mean(feed_scl[,y]))
             
          }
          
           output_all <- rbind(output_all,output)
        }
        
        if("rf" %in% mdls){
          mth <- 'rf'
          print(mth)
  
          #tune
          if(FALSE){
            hyper_grid_rf <- expand.grid(
              ntree = c(500),
              mtry = c(2,3,4), #Number of random variables collected at each split. sqrt(length(allXs))
              nodesize = seq(5,10,1), 
              sampsize = ceiling(seq(0.1,0.9,0.1)*nrow(train_std)), # ceiling(.632*nrow(x))
              replace = c(TRUE), #put back selection or not   
              trainR2 = NA,
              testR2 = NA)
              
            for(i in 1:nrow(hyper_grid_rf)) {
              print(paste0(i,'/',nrow(hyper_grid_rf)))
              set.seed(sd)
              model_rf <- randomForest::randomForest(formula = formula, 
                                                     data = train_std, 
                                                     ntree       = hyper_grid_rf$num.trees[i], #n_features * 10, 
                                                     mtry            = hyper_grid_rf$mtry[i],#Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                                     nodesize   = hyper_grid_rf$nodesize[i],#Minimum size of terminal nodes.
                                                     # maxnodes = hyper_grid_rf$maxnodes[i],
                                                     sampsize = hyper_grid_rf$sampsize[i],
                                                     replace         = hyper_grid_rf$replace[i],
                                                     verbose         = FALSE)
              
              pred_train_rf <- predict(model_rf, newdata = train_std)
              pred_test_rf <- predict(model_rf, newdata = test_std)
              
              #hyper_grid_xgb$trainR2[i] <- 1-sum((pred_train_xgboost-train_std[,y])^2)/sum((train_std[,y]-mean(train_std[,y]))^2)
              #hyper_grid_xgb$testR2[i] <- 1-sum((pred_test_xgboost-test_std[,y])^2)/sum((test_std[,y]-mean(test_std[,y]))^2)
              hyper_grid_rf$trainR2[i] <- sum((pred_train_rf-mean(train_std[,y]))^2)/ 
                (sum((pred_train_rf-mean(train_std[,y]))^2) + sum((pred_train_rf-train_std[,y])^2))
              hyper_grid_rf$testR2[i] <-  sum((pred_test_rf-mean(test_std[,y]))^2)/ 
                (sum((pred_test_rf-mean(test_std[,y]))^2) + sum((pred_test_rf-test_std[,y])^2))
            }
          }
          
          set.seed(sd)  
          model <- randomForest::randomForest(formula         = formula, 
                                              data            = feed_std, 
                                              ntree           = 500, 
                                              mtry            = 4,
                                              nodesize        = 6,
                                              sampsize        = ceiling(0.9*nrow(feed_std)),
                                              replace         = TRUE,
                                              verbose         = FALSE,
                                              importance      = TRUE)
          
          #mean(model$results$Rsquared)
          pred <- predict(object = model,feed_std[,c(allXs)])
          #plot(feed_std[,y],pred)
          
          if(impt){
            varImp<-data.frame(imp=caret::varImp(model,scale=TRUE)[[1]])  #by MSE
            colnames(varImp) <- 'imp'
            varImp$imp <- varImp$imp/max(varImp$imp,na.rm=TRUE)*100
            #library(randomForest)
            #importance(model$finalModel)
            varImp$var <- row.names(caret::varImp(model,scale=TRUE)) 
            varImp$mth <-mth 
            varImp$y <-y 
            varImp$scl <-scl
            varImp_all <- rbind(varImp_all,varImp)
          }
          
          output<-data.frame(sampleID=feed_std$sampleID,obs=feed[,y], obs_std=feed_std[,y],pred=pred)
          output$pred_rcl <- 10^(output$pred*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$Y <- y
          output$scale <- scl
          output$method <- mth
          
          ###OOB
          if(OOB){ 
            cl <- makeCluster(no_cores)  
            clusterExport(cl,c("wd_para",'mth',"feed_std",'idcols','y','allXs','sd','formula'))
            
            parLapply(cl,1:nrow(feed_std),
                      function(ind){
                        #ind<-1
                        train_data <- feed_std[-ind,]
                        test_data <- feed_std[ind,]
                        
                        set.seed(sd)
                        model_OOB<-randomForest::randomForest(formula         = formula, 
                                                              data            = train_data, 
                                                              ntree           = 500, 
                                                              mtry            = 3,
                                                              nodesize        = 5,
                                                              sampsize        = ceiling(0.9*nrow(train_data)),
                                                              replace         = TRUE,
                                                              verbose         = FALSE,
                                                              importance      = FALSE)
                        pred_OOB<-predict(object = model_OOB,test_data[,allXs])
                        
                        output_OOB<-data.frame(sampleID=test_data[,c(idcols)],pred_OOB=pred_OOB)
                        write.csv(output_OOB,file=paste0(wd_para,"predOOB",ind,".csv"),row.names = FALSE)
                      })
            stopCluster(cl)
            
            fileList <- list.files(path=paste0(wd_para), pattern=paste0("predOOB"))
            pred_OOB <- combineFiles(paste0(wd_para,fileList),1)
            
            output <- merge(output,pred_OOB,by=c(idcols))
            output$pred_OOB_rcl <- 10^(output$pred_OOB*sd(feed_scl[,y])+mean(feed_scl[,y]))
          }
          
          output_all <- rbind(output_all,output)
        }
        
        if('rpart' %in% mdls ){
          mth <- 'rpart'
          print(mth)
          
          #TUNE  0.36
          if(FALSE){
            hyper_grid_rpart <- expand.grid(
              cp = c(0.001,0.01,0.001),  #This parameter determines a threshold under which the split of a node is not worth the complexity.
              minsplit = c(1,2,3), 
              minbucket = c(1,2,3,4,5), 
              maxdepth = seq(20,25,1),
              trainR2 = NA,  
              testR2 = NA)
            
            for(i in 1:nrow(hyper_grid_rpart)) {
              control_rpart <- rpart::rpart.control(cp= hyper_grid_rpart$cp[i], 
                                                    minsplit = hyper_grid_rpart$minsplit[i], 
                                                    minbucket = hyper_grid_rpart$minbucket[i], 
                                                    maxdepth=hyper_grid_rpart$maxdepth[i])
              model_rpart <- rpart::rpart(formula, method="anova", data=train_std,
                                          control=control_rpart)
              pred_train_rpart <- predict(model_rpart, newdata = train_std)
              pred_test_rpart <- predict(model_rpart, newdata = test_std)
              
              hyper_grid_rpart$trainR2[i] <- sum((pred_train_rpart-mean(train_std[,y]))^2)/ 
                (sum((pred_train_rpart-mean(train_std[,y]))^2) + sum((pred_train_rpart-train_std[,y])^2))
              hyper_grid_rpart$testR2[i] <-  sum((pred_test_rpart-mean(test_std[,y]))^2)/ 
                (sum((pred_test_rpart-mean(test_std[,y]))^2) + sum((pred_test_rpart-test_std[,y])^2))
            } 
          }
          
          model <- rpart::rpart(formula, method="anova", data=feed_std,
                                control=rpart::rpart.control(minsplit  = 2, 
                                                             minbucket = 2, 
                                                             cp        = 0.008, 
                                                             maxdepth  = 30))
          
          pred <- predict(model, newdata = feed_std)
          #plot(feed_std[,y],pred)
          
          if(impt & !is.null(model$variable.importance)){
            varImp<- data.frame(imp=model$variable.importance)
            varImp <- varImp/max(varImp)*100
            
            varImp$var <- row.names(varImp) 
            
            varImp$mth <-mth 
            varImp$y <-y 
            varImp$scl <-scl
            varImp_all <- rbind(varImp_all,varImp)
          }
          
          output<-data.frame(sampleID=feed_std$sampleID,obs=feed[,y], obs_std=feed_std[,y],pred=pred)
          output$pred_rcl <- 10^(output$pred*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$Y <- y
          output$scale <- scl
          output$method <- mth
          
          ###OOB
          if(OOB){ 
            cl <- makeCluster(no_cores)  
            clusterExport(cl,c("wd_para",'mth',"feed_std",'idcols','formula'))
            
            parLapply(cl,1:nrow(feed_std),
                      function(ind){
                        #ind<-1
                        train_data <- feed_std[-ind,]
                        test_data <- feed_std[ind,]
                        
                        model_OOB <-rpart::rpart(formula, method="anova", data=train_data,
                                                 control=rpart::rpart.control(minsplit  = 2,
                                                                              minbucket = 2, 
                                                                              cp        = 0.008,
                                                                              maxdepth  = 30))
                        pred_OOB <- predict(model_OOB, newdata = test_data)
                        
                        output_OOB<-data.frame(sampleID=test_data[,c(idcols)],pred_OOB=pred_OOB)
                        write.csv(output_OOB,file=paste0(wd_para,"predOOB",ind,".csv"),row.names = FALSE)
                      })
            stopCluster(cl)
            
            fileList <- list.files(path=paste0(wd_para), pattern=paste0("predOOB"))
            pred_OOB <- combineFiles(paste0(wd_para,fileList),1)
            
            output <- merge(output,pred_OOB,by=c(idcols))
            output$pred_OOB_rcl <- 10^(output$pred_OOB*sd(feed_scl[,y])+mean(feed_scl[,y]))
          }
          
          output_all <- rbind(output_all,output)
        }
        
        if('svm'%in% mdls){
          mth <- 'svm'
          print(mth)
          
          #Tune
          if(FALSE){
            hyper_grid_svm <- expand.grid(
              kernel = c('sigmoid','linear' ,"radial",'polynomial'),#'linear' "radial",'polynomial',
              gamma = seq(0.1,1,0.1), 
              cost = seq(1,10,1), 
              type=c('eps-regression'),#'nu-regression',
              tolerance = c(0.00001,0.0001),#tolerance of termination criterion (default: 0.001)
              epsilon=seq(0.01,0.1,0.01), # epsilon in the insensitive-loss function (default: 0.1)
              trainR2 = NA,  
              testR2 = NA)  
            
            
            for(i in 1:nrow(hyper_grid_svm)){
              model_svm <-e1071::svm(formula, data=train_std,
                                     kernel = hyper_grid_svm$kernel[i],
                                     gamma = hyper_grid_svm$gamma[i], 
                                     cost = hyper_grid_svm$cost[i], 
                                     type = hyper_grid_svm$type[i], 
                                     tolerance=hyper_grid_svm$tolerance[i], 
                                     epsilon=hyper_grid_svm$epsilon[i])
              
              pred_train_svm <- predict(model_svm, newdata = train_std)
              pred_test_svm <- predict(model_svm, newdata = test_std)
              
              #hyper_grid_xgb$trainR2[i] <- 1-sum((pred_train_xgboost-train_std[,y])^2)/sum((train_std[,y]-mean(train_std[,y]))^2)
              #hyper_grid_xgb$testR2[i] <- 1-sum((pred_test_xgboost-test_std[,y])^2)/sum((test_std[,y]-mean(test_std[,y]))^2)
              hyper_grid_svm$trainR2[i] <- sum((pred_train_svm-mean(train_std[,y]))^2)/ 
                (sum((pred_train_svm-mean(train_std[,y]))^2) + sum((pred_train_svm-train_std[,y])^2))
              hyper_grid_svm$testR2[i] <-  sum((pred_test_svm-mean(test_std[,y]))^2)/ 
                (sum((pred_test_svm-mean(test_std[,y]))^2) + sum((pred_test_svm-test_std[,y])^2))
              
            }
          }
          
          model <- e1071::svm(formula, 
                              data      = feed_std,
                              kernel    = 'radial',
                              gamma     = 0.1, 
                              cost      = 1, 
                              type      = 'eps-regression', 
                              tolerance = 0.001, 
                              epsilon   = 0.1)
          pred <-predict(model,feed_std,na.action=na.omit)
          #plot(feed_std[,y],pred)
          
          output<-data.frame(sampleID=feed_std$sampleID,obs=feed[,y], obs_std=feed_std[,y],pred=pred)
          output$pred_rcl <- 10^(output$pred*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$Y <- y
          output$scale <- scl
          output$method <- mth
          
          ###OOB
          if(OOB){ 
            cl <- makeCluster(no_cores)  
            clusterExport(cl,c("wd_para",'mth',"feed_std",'idcols','formula'))
            
            parLapply(cl,1:nrow(feed_std),
                      function(ind){
                        #ind<-1
                        train_data <- feed_std[-ind,]
                        test_data <- feed_std[ind,]
                        
                        model_OOB <- e1071::svm(formula, 
                                                data      = train_data,
                                                kernel    = 'radial',
                                                gamma     = 0.1, 
                                                cost      = 1, 
                                                type      = 'eps-regression', 
                                                tolerance = 0.001, 
                                                epsilon   = 0.1)
                        pred_OOB <- predict(model_OOB,test_data,na.action=na.omit)
                        
                        output_OOB<-data.frame(sampleID=test_data[,c(idcols)],pred_OOB=pred_OOB)
                        write.csv(output_OOB,file=paste0(wd_para,"predOOB",ind,".csv"),row.names = FALSE)
                      })
            stopCluster(cl)
            
            fileList <- list.files(path=paste0(wd_para), pattern=paste0("predOOB"))
            pred_OOB <- combineFiles(paste0(wd_para,fileList),1)
            
            output <- merge(output,pred_OOB,by=c(idcols))
            output$pred_OOB_rcl <- 10^(output$pred_OOB*sd(feed_scl[,y])+mean(feed_scl[,y]))
          }
 
          output_all <- rbind(output_all,output)
        }
        
        if('xgboost' %in% mdls){
          mth <- 'xgboost'
          print(mth)
          
          #tune
          if(FALSE){  
            hyper_grid_xgb <- expand.grid(
              objective=c('reg:squarederror'), #'reg:squarederror','reg:pseudohubererror', "reg:logistic",'reg:pseudohubererror','reg:absoluteerror'
              booster=c('gbtree'), #'gbtree','gblinear','dart'
              eta = c(0.3), #default 0.3. the learning rate.~[0.01 - 0.3] Lower eta leads to slower computation. It must be supported by increase in nrounds.
              nrounds = c(500), 
              gamma = c(1),#regulation. ~[0,inf]. Start with 0 and check CV error rate. If you see train error >>> test error, bring gamma into action. Higher the gamma, lower the difference in train and test CV.                                
              alpha = c(0), #L1 regularization (equivalent to Lasso regression) on weights.
              lambda = c(0), #L2 regularization (equivalent to Ridge regression) on weights. It is used to avoid overfitting
              max_depth=c(10,11,12),#the depth of the tree.Larger the depth, more complex the model; higher chances of overfitting
              min_child_weight=c(6,7,8),
              subsample=c(0.6,0.8), #number of samples (observations) supplied to a tree. ~ (0.5-0.8)
              colsample_bytree=c(0.6,0.8), # the number of features (variables) supplied to a tree. [0,1] ~(0.5,0.9)
              colsample_bylevel=c(0.6,0.8), # the number of features (variables) supplied to each level. [0,1] ~(0.5,0.9)
              colsample_bynode=c(0.6,0.8), # the number of features (variables) supplied to each node. [0,1] ~(0.5,0.9)
              trainR2 = NA,  
              testR2 = NA  
            )
            
            for(i in 1:nrow(hyper_grid_xgb)) {
              #i <-1
              print(paste0(i,'/',nrow(hyper_grid_xgb)))
              #[1] "293/384"
              set.seed(sd)
              model_xgboost <- xgboost::xgboost(data = data.matrix(train_std[,allXs]), 
                                                label = train_std[,y], 
                                                #nthread = 1,
                                                objective = as.character(hyper_grid_xgb$objective[i]),
                                                #metrics = as.character(hyper_grid_xgb$metrics[i]),
                                                booster=as.character(hyper_grid_xgb$booster[i]),
                                                eta= hyper_grid_xgb$eta[i],
                                                nrounds=hyper_grid_xgb$nrounds[i],
                                                gamma=hyper_grid_xgb$gamma[i],
                                                alpha=hyper_grid_xgb$alpha[i],
                                                lambda=hyper_grid_xgb$lambda[i],
                                                max_depth=hyper_grid_xgb$max_depth[i],
                                                min_child_weight=hyper_grid_xgb$min_child_weight[i],
                                                subsample=hyper_grid_xgb$subsample[i],
                                                colsample_bytree=hyper_grid_xgb$colsample_bytree[i],
                                                colsample_bylevel=hyper_grid_xgb$colsample_bylevel[i],
                                                colsample_bynode=hyper_grid_xgb$colsample_bynode[i],
                                                verbose = TRUE)
              
              pred_train_xgboost <- predict(model_xgboost, newdata = data.matrix(train_std[,allXs]))
              pred_test_xgboost <- predict(model_xgboost, newdata = data.matrix(test_std[,allXs]))
              
              hyper_grid_xgb$trainR2[i] <- sum((pred_train_xgboost-mean(train_std[,y]))^2)/ 
                (sum((pred_train_xgboost-mean(train_std[,y]))^2) + sum((pred_train_xgboost-train_std[,y])^2))
              hyper_grid_xgb$testR2[i] <-  sum((pred_test_xgboost-mean(test_std[,y]))^2)/ 
                (sum((pred_test_xgboost-mean(test_std[,y]))^2) + sum((pred_test_xgboost-test_std[,y])^2))
              
              #xgb.importance (feature_names = colnames(train_std[,allXs]),model = model_xgboost)
            }
          }
          
          set.seed(sd)
          model <- xgboost::xgboost(data              = data.matrix(feed_std[,allXs]), 
                                    label             = feed_std[,y], 
                                    objective         = 'reg:squarederror',
                                    booster           = 'gbtree',
                                    eta               = 0.3,
                                    nrounds           = 500,
                                    gamma             = 1,
                                    alpha             = 1,
                                    lambda            = 1,
                                    max_depth         = 11,
                                    min_child_weight  = 7,
                                    subsample         = 0.6,
                                    colsample_bytree  = 0.8,
                                    colsample_bylevel = 0.6,
                                    colsample_bynode  = 0.6,
                                    verbose           = FALSE)
          
          pred <- predict(model, data.matrix(feed_std[,allXs]))
          #plot(feed_std[,y],pred)
          
          if(impt){
            varImp<-data.frame(imp=xgboost::xgb.importance(model=model)[,2])
            colnames(varImp) <- 'imp'
            #library(randomForest)
            #importance(model$finalModel)
            varImp$var <- xgboost::xgb.importance(model=model)$Feature
            varImp$imp <- varImp$imp/max(varImp$imp,na.rm=TRUE)*100
            varImp$mth <-mth 
            varImp$y <-y 
            varImp$scl <-scl
            colnames(varImp) <- c('imp','var','mth','y','scl')
            varImp_all <- rbind(varImp_all,varImp)
          }
          
          output<-data.frame(sampleID=feed_std$sampleID,obs=feed[,y], obs_std=feed_std[,y],pred=pred)
          output$pred_rcl <- 10^(output$pred*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$Y <- y
          output$scale <- scl
          output$method <- mth
          
          ###OOB
          if(OOB){ 
            cl <- makeCluster(no_cores)  
            clusterExport(cl,c("wd_para",'mth',"feed_std",'idcols','y','allXs','sd'))
            
            parLapply(cl,1:nrow(feed_std),
                      function(ind){
                        #ind<-1
                        train_data <- feed_std[-ind,]
                        test_data <- feed_std[ind,]
                        
                        set.seed(sd)
                        model_OOB <- xgboost::xgboost(data              = data.matrix(train_data[,allXs]), 
                                                      label             = train_data[,y], 
                                                      objective         = 'reg:squarederror',
                                                      booster           = 'gbtree',
                                                      eta               = 0.3,
                                                      nrounds           = 500,
                                                      gamma             = 1,
                                                      alpha             = 1,
                                                      lambda            = 1,
                                                      max_depth         = 11,
                                                      min_child_weight  = 7,
                                                      subsample         = 0.6,
                                                      colsample_bytree  = 0.8,
                                                      colsample_bylevel = 0.6,
                                                      colsample_bynode  = 0.6,
                                                      verbose           = FALSE)
                        
                        pred_OOB <- predict(model_OOB, data.matrix(test_data[,allXs]))
                        
                        output_OOB<-data.frame(sampleID=test_data[,c(idcols)],pred_OOB=pred_OOB)
                        write.csv(output_OOB,file=paste0(wd_para,"predOOB",ind,".csv"),row.names = FALSE)
                      })
            stopCluster(cl)
            
            fileList <- list.files(path=paste0(wd_para), pattern=paste0("predOOB"))
            pred_OOB <- combineFiles(paste0(wd_para,fileList),1)
            
            output <- merge(output,pred_OOB,by=c(idcols))
            output$pred_OOB_rcl <- 10^(output$pred_OOB*sd(feed_scl[,y])+mean(feed_scl[,y]))
          }
          output_all <- rbind(output_all,output)
        }
        }#loop Y
    }#scale  
    
end_time <- Sys.time()
print(end_time - start_time)

write.csv(varImp_all,paste0(wd,'/result/model/output_ml_variableImportance.csv'),row.names = FALSE)


#get r2
mths <- c("pred","pred_rcl", "pred_OOB","pred_OOB_rcl")
R2_list <- unique(output_all[,c('method','scale','Y')])

for (row in 1:nrow(R2_list)){
#row <- 1 
for (mth in mths){
#mth <- "pred"
data_sub <- output_all[which(output_all$scale==R2_list[row,'scale']  & output_all$Y==R2_list[row,'Y']
                               & output_all$method==R2_list[row,'method']),]
if(mth %in% c("pred", "pred_OOB")){
r2 <- sum((data_sub[,mth] - mean(data_sub[,'obs_std']))^2)/sum((data_sub[,'obs_std'] - mean(data_sub[,'obs_std']))^2)
}
if(mth %in% c("pred_rcl","pred_OOB_rcl")){
r2 <- sum((data_sub[,mth] - mean(data_sub[,'obs']))^2)/sum((data_sub[,'obs'] - mean(data_sub[,'obs']))^2)
}
R2_list[row,paste0('r2_',mth)] <- r2
}#loop row
}#loop mdl    

#output_all <- output_all[,c("method", 'scale',"Y",idcols,'obs',"obs_std","pred","pred_rcl","pred_OOB","pred_OOB_rcl")]
#write.csv(output_all2,paste0(wd,'/result/model/output_ml_',paste0(mdls,collapse = '_'),'.csv'),row.names = FALSE)


  





