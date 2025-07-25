#####requirements#####
#R version 4.4.2
#pROC_1.18.5  
######################

rm(list = ls())
 
data <- read.csv('/data/data.csv',stringsAsFactors = FALSE)

allYs <- c("initialResistance",'reactionRate','recoveryRate')
  
allXs <- c('windSpeed','trackDist','sideTrack',"travelSpeed","rainfall","stormFreq",
           "coastalSlope",'coastDist',
           "nSpecies","canopyHeight","preEVI","patchSize")
             
formula <- as.formula(paste('prob',"~ ", paste(allXs, collapse = " + ")))  
sd <- 1234

#format input
if(TRUE){
  feed <-data
 
    #transform input
    if(TRUE){
      feed_scl <- feed
      #adjust 0 
      for(var in c(allXs)){
        if(min(feed[,var],na.rm = TRUE)<=0){
            feed_scl[,var] <- feed[,var]+abs(min(feed[,var],na.rm = TRUE))+1} 
      }
      
      for(x in c(allXs)){
        feed_scl[,x] <- log10(feed_scl[,x])  
        feed_scl[,x] [which(!is.finite(feed_scl[,x] ))]<- NA  
      }
         
    }
    
    #standardize input
    feed_std <- feed_scl
    for (col in c(allXs)){
      feed_std[,col] <- feed_std[,col]-mean(feed_std[,col],na.rm = TRUE)   
      feed_std[,col] <- feed_std[,col]/sd(feed_std[,col],na.rm = TRUE)   
    }  
}

error_all <- data.frame()
varImp_all <- data.frame()

for(ttl in c("resistanceAboveMedianOrNot", 
             "reactionRateAboveMedianOrNot",
             "recoveryRateAboveMedianOrNot")){
               
    if(ttl=="resistanceAboveMedianOrNot"){
        input <- feed_std[,c('initialResistance',allXs)]
        colnames(input)[1]<-'y'
        input$prob <- 0
        input[which(input$y > median(input$y,na.rm=TRUE)),'prob']<-1}
  
    if(ttl=="reactionRateAboveMedianOrNot"){
        input <- feed_std[which(feed_std$reactionRate<0),c('reactionRate',allXs)]
        colnames(input)[1]<-'y'
        input$prob <- 0
        input[which(input$y > median(input$y,na.rm=TRUE)),'prob']<-1}
  
    if(ttl=="recoveryRateAboveMedianOrNot"){
        input <- feed_std[,c('recoveryRate',allXs)]
        colnames(input)[1]<-'y'
        input$prob <- 0
        input[which(input$y > median(input$y,na.rm=TRUE)),'prob']<-1}
   
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
      
    #variable importance
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

write.csv(error_all,'/results/lg01_error.csv',row.names=FALSE)

colnames(varImp_all) <- c("Estimate","Std.Error", "z.value","Pr.z","var","y")  
write.csv(varImp_all,'/results/lg01_varImp.csv',row.names=FALSE)
               
