###model 
if(TRUE){
  rm(list = ls())
  
  #####################  
  #self-defined function
  ####################  
  if(TRUE){
    ens <- function(train_data0,test_data0,y_col0,x_cols0){ 
      #train_data0 <- feed_std
      #test_data0 <- feed_std
      #y_col0 <- y
      #x_cols0 <- allXs
      formula <- as.formula(paste(y_col0,"~", paste(x_cols0, collapse = " + ")))  
      
      train_data0 <- train_data0[,c(y_col0,x_cols0)]
      test_data0 <- test_data0[,c(y_col0,x_cols0)]
      
      library(caret) #for rf, gbm, knn
      fitControl <- trainControl(method = "cv", number = 5, savePredictions = 'final')
      
      if(FALSE){
        set.seed(1) 
        model_rf <- train(formula,data=train_data0,method='rf',trControl=fitControl,tuneLength=3)
        pred_rf_train <- predict(object = model_rf,train_data0)
        pred_rf_test <- predict(object = model_rf,test_data0)
      }  
      
      if(TRUE){
        set.seed(1)
        model_gbm <- train(formula,data=train_data0,method='gbm',trControl=fitControl,tuneLength=3,verbos=FALSE)
        pred_gbm_train <- predict(object = model_gbm,train_data0)
        pred_gbm_test <- predict(object = model_gbm,test_data0)
      }
      
      if(FALSE){
        library(e1071) 
        model_svm <- svm(formula, data=train_data0)
        pred_svm_train <- predict(model_svm,train_data0,na.action=na.omit)
        pred_svm_test <- predict(model_svm,test_data0,na.action=na.omit)
      }
      
      if(FALSE){
        model_knn <- train(formula,data=train_data0,method='knn',trControl=fitControl,tuneLength=3)
        pred_knn_train <- predict(object = model_knn,train_data0)
        pred_knn_test <- predict(object = model_knn,test_data0)  
      }
      
      library(xgboost) #for xgboost
      model_xgboost <- xgboost(data = data.matrix(train_data0[,x_cols0]), 
                               label = train_data0[,y_col0], 
                               objective = "reg:squarederror",
                               nrounds=15,
                               verbose = FALSE)
      pred_xgboost_train <- predict(model_xgboost, data.matrix(train_data0[,x_cols0]))
      pred_xgboost_test <- predict(model_xgboost, data.matrix(test_data0[,x_cols0]))
      
      library(rpart) #for rpart
      model_rpart <- rpart(formula, method="anova", data=train_data0,
                           control=rpart.control(minsplit=1, minbucket=1, cp=0.01, maxdepth=20))
      pred_rpart_train <- predict(model_rpart, newdata = train_data0)
      pred_rpart_test <- predict(model_rpart, newdata = test_data0)
      
      
      train_top<-as.data.frame(cbind(train_data0[,y_col0],
                                     # pred_svm_train,pred_rf_train,pred_knn_train,
                                     pred_gbm_train,pred_xgboost_train,pred_rpart_train))
      colnames(train_top)<-c(y_col0,
                             # "pred_svm","pred_rf","pred_knn",
                             "pred_gbm",'pred_xgboost','pred_rpart')
      test_top<-as.data.frame(cbind(test_data0[,y_col0],
                                    # pred_svm_test,pred_rf_test,pred_knn_test,
                                    pred_gbm_test, pred_xgboost_test,pred_rpart_test))
      colnames(test_top)<-c(y_col0,
                            # "pred_svm","pred_rf","pred_knn",
                            "pred_gbm",'pred_xgboost','pred_rpart')
      
      formula_top <- as.formula(paste(y_col0,"~", paste(c(
        #'pred_gbm','pred_svm','pred_rf','pred_knn',
        'pred_gbm', 'pred_xgboost','pred_rpart'), collapse = " + ")))  
      model_en <- lm(formula_top,data=train_top)
      pred_en <- predict(model_en,test_top,interval="predict")
      
      return(pred_en)
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
    #wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangroveModel/") #local
    wd <- paste0("Z:/TCMangroveModel") #server
    
    data <- read.csv(paste0(wd,'/result/model/input_fixedCNTRY_rmOutlier.csv'),stringsAsFactors = FALSE)
    
    no_cores<-detectCores()-1 
    wd_para<-paste0(wd,"/result/model/OOBoutput/")
    
    #format input file 
    if(TRUE){ 
      data$sd <- data$side
      data$tD <- data$trackDist
      data$cD <- data$coastDist
      idcols <- c('landing','sd','tD','cD')
    }
    
    
    #set up variables
    if(TRUE){
      allYs <- c("intensity")
      
      allXs <- c("bathymetry_mean",'coastDist'#,#"elevation_mean",
                 #"landingSpeed","landingWindMaxLocal2","totalPrec_total"
      )
      
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
        pred_en <- as.data.frame(ens(feed_std,feed_std,y,allXs))
        #plot(feed_std[,y],pred_en[,'fit'])
        
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
          clusterExport(cl,c("wd_para","feed_std",'idcols','y','allXs','ens'))
          
          parLapply(cl,1:nrow(feed_std),
                    function(ind){
                      #ind<-2
                      train_data <- feed_std[-ind,]
                      test_data <- feed_std[ind,]
                      
                      pred_OOB<- ens(train_data,test_data,y,allXs)
                      
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
        if(TRUE){
          eventList<-unique(feed_std$landing)
          cl <- makeCluster(no_cores)     
          clusterExport(cl,c('eventList',"wd_para","feed_std",'idcols','y','allXs','ens'))
          
          parLapply(cl,1:length(eventList),
                    function(id){
                      #id <-2
                      ind <- which(feed_std$landing==eventList[id])
                      train_data <- feed_std[-ind,]
                      test_data <- feed_std[ind,]
                      
                      pred_OOB_event<- ens(train_data,test_data,y,allXs)
                      
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
                                  'pred_en_OOB_event','pred_en_OOB_event_upr','pred_en_OOB_event_lwr',
                                  'obs','pred_en_rcl','pred_en_upr_rcl','pred_en_lwr_rcl',
                                  'pred_en_OOB_rcl','pred_en_OOB_upr_rcl','pred_en_OOB_lwr_rcl',
                                  'pred_en_OOB_event_rcl','pred_en_OOB_event_upr_rcl','pred_en_OOB_event_lwr_rcl')]
    
    write.csv(output_all2,paste0(wd,'/result/model/ml/output_en_3Method_gbm_xgboost_rpart-coast.csv'),row.names = FALSE)
  }#model
}#whold block


###plot
if(TRUE){
  rm(list = ls())
  library(dplyr)
  library(tidyr)
  
  #wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangroveModel/") #local
  wd <- paste0("Z:/TCMangroveModel") #server
  
  
  ff <-'en_3Method_gbm_xgboost_rpart-coast'
  
  file <- list.files(paste0(wd,'/result/model/ml/'),paste0('output_',ff,'.csv'))
  data <- read.csv(paste0(wd,'/result/model/ml/',file),stringsAsFactors = FALSE)
  
  mdls <- c("pred_en", "pred_en_OOB", "pred_en_OOB_event","pred_en_rcl", "pred_en_OOB_rcl", "pred_en_OOB_event_rcl")
  obList <- c("obs_std", "obs_std", "obs_std","obs", "obs", "obs")
  
  list <- unique(data[,c('scale','Y')])
  
  
  
  pdf(file = paste0(wd,'/result/figure/','pred_',ff,'.pdf'), width = 8.5, height = 11) 
  par(oma=c(3,1,3,1),mar=c(5,4,4,1),xpd=FALSE) 
  layout(matrix(seq(1,12,1), 4, 3, byrow = TRUE))
  
  for (row in 1:nrow(list)){
    #row <- 1 
    for (mm in 1:length(mdls)){
      mdl <- mdls[mm]
      ob <- obList[mm]
      
      data_sub <- data[which(data$scale==list[row,'scale'] 
                             & data$Y==list[row,'Y']),]
      
      r2 <- sum((data_sub[,mdl] - mean(data_sub[,ob]))^2)/sum((data_sub[,ob] - mean(data_sub[,ob]))^2)
      r2res <- 1 - sum((data_sub[,mdl] - data_sub[,ob])^2)/sum((data_sub[,ob] - mean(data_sub[,ob]))^2)
      mse <- sum((data_sub[,ob]-data_sub[,mdl])^2)/nrow(data_sub)
      mape <- sum(abs((data_sub[,ob]-data_sub[,mdl]))/data_sub[,ob])/nrow(data_sub)
      
      
      
      list[row,'n'] <- nrow(data_sub)
      list[row,paste0('r2_',mdl)] <- r2
      list[row,paste0('r2res_',mdl)] <- r2res
      list[row,paste0('mse_',mdl)] <- mse
      list[row,paste0('mape_',mdl)] <- mape
      
      plot(data_sub[,ob],data_sub[,mdl],
           xlab='obs',ylab=mdl,
           main=paste0(list[row,'Y'],', ',
                       '\nen, ',list[row,'scale'],', ','n=',nrow(data_sub),',',
                       '\nr2=',round(r2, digits = 2) ,', r2res=',round(r2res,digits = 2),
                       '\nMSE=',round(mse, digits = 2) ,', MAPE=',round(mape,digits = 2))) 
      abline(a=0,b=1)
      #list[row,paste0('mse_',mdl)] <- sum((data_sub[,mdl] - data_sub[,'obs'])^2/(data_sub[,'obs'])^2)/nrow(data_sub)
      #list[row,paste0('mape_',mdl)] <- 1-sum(abs((data_sub[,mdl] - data_sub[,'obs'])/data_sub[,'obs']))/nrow(data_sub)
    }#loop row
  }#loop mdl
  
  dev.off()
  
  #write.csv(list,paste0(wd,'/result/model/ml/r2_en_3Method_rf_xgboost_rpart-storm6var.csv'),row.names = FALSE)
}

#AUC
if(FALSE){
  
  library(ROCR)
  perf = prediction(pred1[,2], mydata$Creditability)
  # 1. Area under curve
  auc = performance(perf, "auc")
  auc
  # 2. True Positive and Negative Rate
  pred3 = performance(perf, "tpr","fpr")
  # 3. Plot the ROC curve
  plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
  abline(a=0,b=1,lwd=2,lty=2,col="gray")
}
