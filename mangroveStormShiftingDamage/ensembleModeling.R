#####requirements#####
#R version 4.4.2
#caret_7.0-1 (for knn)
#e1071_1.7.16  (for svm)
#gbm_2.2.2 
#randomForest_4.7.1.2
#rpart_4.1.23
#xgboost_1.7.10.1 
######################

###emsemble 
if(TRUE){
  rm(list = ls())
  
  #####################  
  #self-defined function
  ####################  
  if(TRUE){
    ens <- function(mths0,train_data0,test_data0,y_col0,x_cols0,sd0){ 
      #mths0 <- mths
      #train_data0 <- train_std
      #test_data0 <- test_std
      #y_col0 <- y
      #x_cols0 <- allXs
      #sd0<-1234
      
      formula <- as.formula(paste(y_col0,"~", paste(x_cols0, collapse = " + ")))  
      
      train_data0 <- train_data0[,c(y_col0,x_cols0)]
      test_data0 <- test_data0[,c(x_cols0)]
      
      #gbm
      if('gbm' %in% mths0){
        set.seed(sd)
        model_gbm <-  gbm::gbm(formula           = formula,
                               data              = train_data0,
                               distribution      = "gaussian",
                               n.trees           = 500,
                               interaction.depth = 45,
                               shrinkage         = 0.14,
                               n.minobsinnode    = 5,
                               cv.folds          = 10,
                               verbose           = FALSE)
        #train(formula,data=train_data0,method='gbm',trControl=fitControl,tuneLength=3,verbos=FALSE)
        pred_gbm_train <- predict(object = model_gbm,train_data0)
        pred_gbm_test <- predict(object = model_gbm,test_data0)
      }
      
      #knn
      if('knn' %in% mths0){
        fitControl <- caret::trainControl(method = "cv", number = 10, savePredictions = 'final')
        
        model_knn <-caret::train(train_data0[,c(x_cols0)],train_data0[,y_col0],method='knn',trControl=fitControl, tuneGrid = data.frame(k=9),verbos=FALSE) 
        # train(formula,data=train_data0,method='knn',trControl=fitControl,tuneLength=3)
        pred_knn_train <- predict(object = model_knn,train_data0)
        pred_knn_test <- predict(object = model_knn,test_data0)  
      }
      
      #rf
      if('rf' %in% mths0){
        set.seed(sd0) 
        model_rf <- randomForest::randomForest(formula         = formula, 
                                               data            = train_data0, 
                                               ntree           = 500, 
                                               mtry            = 2,
                                               min.node.size   = 5,
                                               replace         = TRUE,
                                               sample.fraction = 0.5,
                                               verbose         = FALSE,
                                               importance      = FALSE)
        
        # model_rf <- train(formula,data=train_data0,method='rf',trControl=fitControl,tuneLength=3)
        pred_rf_train <- predict(object = model_rf,train_data0)
        pred_rf_test <- predict(object = model_rf,test_data0)
        
      }  
      
      #rpart
      if('rpart' %in% mths0){
        model_rpart <- rpart::rpart(formula, 
                                    method  = "anova", 
                                    data    = train_data0,
                                    control = rpart::rpart.control(minsplit  = 2, 
                                                                   minbucket = 2, 
                                                                   cp        = 0.008, 
                                                                   maxdepth  = 30))
        pred_rpart_train <- predict(model_rpart, newdata = train_data0)
        pred_rpart_test <- predict(model_rpart, newdata = test_data0)
      }
      
      #svm
      if('svm' %in% mths0){
        # library(e1071) 
        model_svm <- e1071::svm(formula, 
                                data      = train_data0,
                                kernel    = 'radial',
                                gamma     = 0.1, 
                                cost      = 1, 
                                type      = 'eps-regression', 
                                tolerance = 0.001, 
                                epsilon   = 0.1)
        #  svm(formula, data=train_data0)
        pred_svm_train <- predict(model_svm,train_data0,na.action=na.omit)
        pred_svm_test <- predict(model_svm,test_data0,na.action=na.omit)
      }
      
      #xgboost
      if('xgb' %in% mths0){
        model_xgboost <- xgboost::xgboost(data              = data.matrix(train_data0[,x_cols0]), 
                                          label             = train_data0[,y_col0], 
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
        # xgboost(data = data.matrix(train_data0[,x_cols0]), 
        #                        label = train_data0[,y_col0], 
        #                       objective = "reg:squarederror",
        #                       nrounds=15,
        #                       verbose = FALSE)
        pred_xgb_train <- predict(model_xgboost, data.matrix(train_data0[,x_cols0]))
        pred_xgb_test <- predict(model_xgboost, data.matrix(test_data0[,x_cols0]))
      }
      
      train_top<-train_data0[,y_col0]
      for(m in 1:length(mths0)){
        train_top<-cbind(train_top, eval(parse(text=paste0('pred_',mths0[m],'_train'))))  
      }
      train_top <- as.data.frame(train_top)
      colnames(train_top)<-c(y_col0,paste0('pred_',mths0))
      
      
      test_top<-eval(parse(text=paste0('pred_',mths0[1],'_test')))
      if(length(mths0)>1){
        for(m in 2:length(mths0)){
          test_top<-cbind(test_top, eval(parse(text=paste0('pred_',mths0[m],'_test'))))  
        }}
      test_top <- as.data.frame(test_top)
      colnames(test_top)<-c(paste0('pred_',mths0))
      
      
      formula_top <- as.formula(paste(y_col0,"~", paste(paste0('pred_',mths0), collapse = " + ")))  
      model_en <- lm(formula_top,data=train_top)
      pred_en <- predict(model_en,test_top,interval="predict")
      
      outputList <- list(model_en,pred_en) 
      
      return(outputList)
    }    
    
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
  library(tidyr)
  library(parallel)
  
  ##########
  #input
  ##########
  if(TRUE){  
    #wd <- paste0("C:/YM/CHES/TCMangroveModel/") #local
    wd <- paste0("Z:/TCMangroveModel") #server
    
    data <- read.csv(paste0(wd,'/result/model/input.csv'),stringsAsFactors = FALSE)
    
    #no_cores<-detectCores()-1
    no_cores<-8
    wd_para<-paste0(wd,"/result/model/OOBoutput/")
    
    #format input file 
    if(TRUE){ 
      idcols <- c('sampleID')
      sd<-1234
    }
    
    #set up variables
    if(TRUE){
      allYs <- c("damage")
      
      allXs <- c("windSpeed","windSpeed5day","travelSpeed","rainfall","coastalSlope",'coastDist')#
      
      
      #adjust 0
      for(var in c(allXs)){
        if(min(data[,var],na.rm = TRUE)<=0){
          data[,var] <- data[,var]+abs(min(data[,var],na.rm = TRUE))+1 
        }  
      }
    } 
  }
  ###########
  #modelling
  ###########
  if(TRUE){  
    mths <- c('rpart','gbm','xgb','rf','knn')#
    
    output_all <- data.frame(matrix(nrow=0,ncol=27))
    colnames(output_all) <-  c( 'scale',"Y",idcols,"obs_std",
                                'pred_en','pred_en_upr','pred_en_lwr',
                                'pred_en_OOB','pred_en_OOB_upr','pred_en_OOB_lwr',
                                'pred_en_OOB_event','pred_en_OOB_event_upr','pred_en_OOB_event_lwr',
                                'obs','pred_en_rcl','pred_en_upr_rcl','pred_en_lwr_rcl',
                                'pred_en_OOB_rcl','pred_en_OOB_upr_rcl','pred_en_OOB_lwr_rcl',
                                'pred_en_OOB_event_rcl','pred_en_OOB_event_upr_rcl','pred_en_OOB_event_lwr_rcl')
    
    start_time <- Sys.time()
    
    for(scl in c('log10')){
      #for(scl in c('log10some')){  
      #scl <- 'log10'  
      print(scl)  
      
      for (y in allYs){
        #y <- allYs[1]  
        print(y)
        
        feed <- data[,c(idcols,y, allXs)]
        
        ###############  
        #transform input
        ###############  
        feed_scl <- feed
        if(TRUE){
          if(scl=='log10'){
            for(x in c(y, allXs[!allXs%in%c('side')])){
              feed_scl[,x] <- log10(feed_scl[,x])  
              feed_scl[,x] [which(!is.finite(feed_scl[,x] ))]<- NA 
            }
          }
          
          feed_scl[,y] [which(!is.finite(feed_scl[,y] ))]<- NA 
          feed_scl <- feed_scl[complete.cases(feed_scl), ] 
        }
        
        ################
        #standardize input
        ################
        feed_std <- feed_scl
        for (col in c(y,allXs)){
          #col <- 3
          feed_std[,col] <- feed_std[,col]-mean(feed_std[,col])   
          feed_std[,col] <- feed_std[,col]/sd(feed_std[,col])   
        }
        
        ################
        #modelling
        ################
        
        #choose methods
        if(FALSE){
          set.seed(sd) 
          testRows<- sample(1:nrow(feed_std), round(nrow(feed_std)/10,digits = 0), replace=FALSE)
          test_std <- feed_std[testRows,]
          train_std <- feed_std[-testRows,]
          
          mths_all <- c('rpart','gbm','xgb') # ,'knn','svm','rf'
          output_en_model_all <- data.frame(mths=NA,trainR2=NA,testR2=NA,testR2_res=NA)
          
          for(nn in 1:length(mths_all)){
            #nn <- 5  
            mths_cb<-combn(length(mths_all),nn)
            
            for(cc in 1:ncol(mths_cb)){
              #cc<-1
              mths <- mths_all[mths_cb[,cc]]
              
              #mths <- c('rpart','gbm','xgb'),'rf','knn'
              model_en <- ens(mths,train_std,test_std,y,allXs,sd)
              trainR2<-summary(model_en[[1]])$adj.r
              pred_en <- as.data.frame(model_en[[2]])
              
              TSS <- sum((test_std[,y]-mean(test_std[,y]))^2) 
              RSS <- sum((test_std[,y]-pred_en[,'fit'])^2)
              r2_res <- 1-RSS/TSS
              
              RSS2 <- sum((pred_en[,'fit']-mean(pred_en[,'fit']))^2) 
              r2 <- RSS2/TSS
              
              
              #plot(test_std[,y],pred_en[,'fit'],main=paste0(c(mths,round(r2,digits = 2),round(r2_res,digits = 2)),collapse = '|'))
              
              output_en_model <- data.frame(mths=paste0(mths,collapse = '+'),trainR2=trainR2,testR2=r2,testR2_res=r2_res)       
              output_en_model_all <- rbind(output_en_model_all,output_en_model)
            }
          }  
          
          
        }
        
        
        
        pred_en <- as.data.frame(ens(mths,feed_std,feed_std,y,allXs,sd)[[2]])
        output<-data.frame(landing=feed_std$landing,sd=feed_std$sd,cD=feed_std$cD,tD=feed_std$tD,obs_std=feed_std[,y],
                           pred_en=pred_en$fit,pred_en_upr=pred_en$upr,pred_en_lwr=pred_en$lwr)
        
        output$pred_en_rcl <- 10^(output$pred_en*sd(feed_scl[,y])+mean(feed_scl[,y]))
        output$pred_en_upr_rcl <- 10^(output$pred_en_upr*sd(feed_scl[,y])+mean(feed_scl[,y]))
        output$pred_en_lwr_rcl <- 10^(output$pred_en_lwr*sd(feed_scl[,y])+mean(feed_scl[,y]))
        
        output <- merge(output,feed[,c(idcols,y)],by=c(idcols))
        colnames(output)[ncol(output)]<-'obs'
        
        output$Y <- y
        output$scale <- scl
        
        ###OOB
        if(TRUE){ 
          cl <- makeCluster(no_cores)  
          clusterExport(cl,c("wd_para",'mths',"feed_std",'idcols','y','allXs','ens','sd'))
          
          parLapply(cl,1:nrow(feed_std),
                    function(ind){
                      #ind<-2
                      train_data <- feed_std[-ind,]
                      test_data <- feed_std[ind,]
                      
                      pred_OOB<- ens(mths,train_data,test_data,y,allXs,sd)[[2]]
                      
                      output<-cbind(test_data[,idcols],pred_OOB)
                      write.csv(output,file=paste0(wd_para,"predOOB",ind,".csv"),row.names = FALSE)
                    })
          stopCluster(cl)
          
          fileList <- list.files(path=paste0(wd_para), pattern=paste0("predOOB"))
          pred_en_OOB<- combineFiles(paste0(wd_para,fileList),d=1)
          colnames(pred_en_OOB) <- c( idcols,"pred_en_OOB","pred_en_OOB_lwr","pred_en_OOB_upr" )
          
          output <- merge(output,pred_en_OOB,by=c(idcols))
          
          output$pred_en_OOB_rcl <- 10^(output$pred_en_OOB*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$pred_en_OOB_upr_rcl <- 10^(output$pred_en_OOB_upr*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$pred_en_OOB_lwr_rcl <- 10^(output$pred_en_OOB_lwr*sd(feed_scl[,y])+mean(feed_scl[,y]))
          
        }
        
        ###OOB event
        if(FALSE){
          eventList<-unique(feed_std$landing)
          cl <- makeCluster(no_cores)     
          clusterExport(cl,c('eventList',"wd_para",'mths',"feed_std",'idcols','y','allXs','ens','sd'))
          
          parLapply(cl,1:length(eventList),
                    function(id){
                      #id <-2
                      ind <- which(feed_std$landing==eventList[id])
                      train_data <- feed_std[-ind,]
                      test_data <- feed_std[ind,]
                      
                      pred_OOB_event<- ens(mths,train_data,test_data,y,allXs,sd)[[2]]
                      
                      output<-cbind(test_data[,idcols],pred_OOB_event)
                      write.csv(output,paste0(wd_para,"predOOBEvent",id,".csv"),row.names = FALSE)
                    })
          stopCluster(cl)
          
          fileList <- list.files(path=paste0(wd_para), pattern=paste0("predOOBEvent"))
          pred_en_OOB_event<- combineFiles(paste0(wd_para,fileList),d=1)
          colnames(pred_en_OOB_event) <- c( idcols,"pred_en_OOB_event","pred_en_OOB_event_lwr","pred_en_OOB_event_upr" )
          
          output <- merge(output,pred_en_OOB_event,by=c(idcols))
          
          output$pred_en_OOB_event_rcl <- 10^(output$pred_en_OOB_event*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$pred_en_OOB_event_upr_rcl <- 10^(output$pred_en_OOB_event_upr*sd(feed_scl[,y])+mean(feed_scl[,y]))
          output$pred_en_OOB_event_lwr_rcl <- 10^(output$pred_en_OOB_event_lwr*sd(feed_scl[,y])+mean(feed_scl[,y]))
        }  
        
        output_all <- rbind(output_all,output)
        
      }#loop Y
    }#scale  
    
    end_time <- Sys.time()
    print(end_time - start_time)
    
    output_all2 <- output_all[,c( 'scale',"Y",idcols,"obs_std",
                                  'pred_en','pred_en_upr','pred_en_lwr',
                                  'pred_en_OOB','pred_en_OOB_upr','pred_en_OOB_lwr',
                                  #'pred_en_OOB_event','pred_en_OOB_event_upr','pred_en_OOB_event_lwr',
                                  'obs','pred_en_rcl','pred_en_upr_rcl','pred_en_lwr_rcl',
                                  'pred_en_OOB_rcl','pred_en_OOB_upr_rcl','pred_en_OOB_lwr_rcl'#,
                                  #'pred_en_OOB_event_rcl','pred_en_OOB_event_upr_rcl','pred_en_OOB_event_lwr_rcl'
    )]
    
    write.csv(output_all,paste0(wd,'/result/model/en/output_en-',paste0(allXs,collapse = '+'),'-',paste0(mths,collapse = '+'),'.csv'),row.names = FALSE)
  }#model
}
