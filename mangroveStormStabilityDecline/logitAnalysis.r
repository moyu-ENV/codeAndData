  rm(list = ls())
  library(dplyr)
  library(randomForest)
  
  
  wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangroveRe/") #local

  data00 <- read.csv(paste0(wd,'/result/model/input2024_rmOutlier-newDi.csv'),stringsAsFactors = FALSE)
  data00[which(data00$resistance0>1),'resistance0'] <-1
  data00[which(data00$recoverRate>1),'recoverRate'] <-1
  
  data00<-data00[which(data00$diversityFilled==0),]
  
 
  allYs <- c( "resistance0",'reactRate','recoverRate')
  
  allXs <- c('wind','trackDist','sideTrack',"speed","totalPrec_value","stormFreq_mean",#"localWind",
             "bathymetry_mean",'coastDistM',
             "alphaDiversity_mean","canopyHeight_mean","preEVI","areaM2"  #,
             #           "population_mean","builtUpArea_mean",'agricultureLand_mean'
  )
  
  #input
  if(TRUE){
  feed <-data00
 
   ################
  #transform input
  ################
    if(TRUE){
      feed_scl <- feed
      #adjust 0 
      for(var in c(allXs)){
        #var <-"elevation_mean"
        if(min(feed[,var],na.rm = TRUE)<=0){
          print(var)
          feed_scl[,var] <- feed[,var]+abs(min(feed[,var],na.rm = TRUE))+1} 
      }
      
      for(x in c(allXs)){
        feed_scl[,x] <- log10(feed_scl[,x])  
        feed_scl[,x] [which(!is.finite(feed_scl[,x] ))]<- NA  
      }
      
      #for(y in allYs){
      #feed_scl[,y] [which(!is.finite(feed_scl[,y] ))]<- NA  
      #}
     # feed_scl <- feed_scl[complete.cases(feed_scl), ] 
    }
    
    ################
    #standardize input
    ################
    feed_std <- feed_scl
    for (col in c(allXs)){
      #col <- 1
      feed_std[,col] <- feed_std[,col]-mean(feed_std[,col],na.rm = TRUE)   
      feed_std[,col] <- feed_std[,col]/sd(feed_std[,col],na.rm = TRUE)   
    }  
    
  }
  
 
  formula <- as.formula(paste('prob',"~ ", paste(allXs, collapse = " + ")))  
  sd <- 1234
  
  error_all <- data.frame()
  varImp_all <- data.frame()
  vp0_all <- data.frame()
  vp_all <- data.frame()
  
  for(ttl in c("resistanceAboveMedianOrNotDamageData",
               "reactionOverInitialDamageAboveMedianOrNotReationData",
               "recoveryRateAboveMedianOrNotDamageData")){
    
    #ttl <-"resistanceAboveMedianOrNotReactionData"
   
    if(ttl=="resistanceAboveMedianOrNotDamageData"){input <- feed_std[which(feed_std$impacted==1),c('resistance0',allXs)]
    colnames(input)[1]<-'y'
    input$prob <- 0
    input[which(input$y > median(input$y,na.rm=TRUE)),'prob']<-1}
       
    if(ttl=="reactionRateAboveMedianOrNotReactionData"){input <- feed_std[which(feed_std$impacted==1 & feed_std$reactRate<0),c('reactRate',allXs)]
    colnames(input)[1]<-'y'
    input$prob <- 0
    input[which(input$y > median(input$y,na.rm=TRUE)),'prob']<-1}
    
   if(ttl=="recoveryRateAboveMedianOrNotDamageData"){
      input <- feed_std[which(feed_std$impacted==1),c('recoverRate',allXs)]
      colnames(input)[1]<-'y'
      input$prob <- 0
      input[which(input$y > median(input$y,na.rm=TRUE)),'prob']<-1}
     
    #model 
    if(TRUE){
      #logit OOB 
      if(TRUE){
        set.seed(sd) 
        testRows<- sample(1:nrow(input), round(nrow(input)/10,digits = 0), replace=FALSE)
        input_test <- input[testRows,]
        input_train <- input[-testRows,]
        
        
        logit <- glm(formula,family='binomial',data=input_train)
        
        pred_train <- predict(logit, newdata = input_train, type = "response")
        roc_obj_train<-pROC::roc(input_train$prob, pred_train)
        pred_test <- predict(logit, newdata = input_test, type = "response")
        roc_obj_test<-pROC::roc(input_test$prob, pred_test)
        
        error <- data.frame(train_error=pROC::auc(roc_obj_train),test_error=pROC::auc(roc_obj_test))
        error$y <- ttl
        error_all <- rbind(error_all,error)
      }
      
      #var imp
      if(TRUE){
        varImp<-as.data.frame(summary(logit)$coefficients)
        for(cc in 1:4){
          varImp[,cc] <- round(varImp[,cc],digits = 3)  
        }
        varImp$var <- rownames(varImp)
        varImp$y <-ttl
        
        varImp_all <- rbind(varImp_all,varImp)
      } 
      }
    

  
  write.csv(error_all,paste0(wd,'/result/model/lg01/error.csv'),row.names=FALSE)
  colnames(varImp_all) <- c( "Estimate","Std.Error", "z.value","Pr.z","var","y")  
  write.csv(varImp_all,paste0(wd,'/result/model/lg01/varImp.csv'),row.names=FALSE)
}#whole block
