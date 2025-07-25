rm(list = ls())

data_input <- read.csv('/data/data.csv',stringsAsFactors = FALSE)
data_varImp <- read.csv('/results/lg01_varImp.csv',stringsAsFactors = FALSE)
data_error <- read.csv('/results/lg01_error.csv',stringsAsFactors = FALSE)

#change distance to proximity 
data_varImp[which(data_varImp$var%in%c('coastDist','trackDist')),'Estimate']<-(-1)*data_varImp[which(data_varImp$var%in%c('coastDist','trackDist')),'Estimate']
  
#set labels
if(TRUE){
    lable <- data.frame(var=c('windSpeed','trackDist','sideTrack','travelSpeed','rainfall',
                              'stormFreq','coastalSlope','coastDist',
                              'preEVI','patchSize','canopyHeight','nSpecies'), 
                        lbl= c('Wind speed','Prox. to track','Side to track','Travel speed', 'Rainfall',
                               'Storm frequency','Coastal slope','Prox. to shore',
                               'Pre-storm EVI','Patch size','Canopy height','No. Species'))
    lable$order <- as.numeric(rownames(lable))
    data_varImp <- merge(data_varImp,lable)
}

#set color 
if(TRUE){
    #define color 
    if(TRUE){
      col_wind <- rgb(170,68,153,maxColorValue = 255)
      col_vege <- rgb(17,119,51,maxColorValue = 255)
      col_coast <- rgb(0,68,136,maxColorValue = 255)
    } 
    
    #by var
    if(TRUE){
        data_varImp$group <- NA
        for (row in 1:nrow(data_varImp)){
            if(data_varImp[row,'var']%in%c("windSpeed",'trackDist','sideTrack','travelSpeed','rainfall','stormFreq')){data_varImp[row,'group']<-'wind'}  
            if(data_varImp[row,'var']%in%c("coastalSlope","coastDist")){data_varImp[row,'group']<-'coast'} 
            if(data_varImp[row,'var']%in%c('preEVI','patchSize','canopyHeight','nSpecies')){data_varImp[row,'group']<-'vege'} 
        }  
      
        data_varImp$col <- NA
        data_varImp[which(data_varImp$group=='wind'),'col'] <- col_wind
        data_varImp[which(data_varImp$group=='vege'),'col'] <- col_vege
        data_varImp[which(data_varImp$group=='coast'),'col'] <-col_coast
            
        data_varImp$colFill <- data_varImp$col
        data_varImp[data_varImp$Pr.z>0.05,'colFill'] <- NA
    }
}

#plot format 
if(TRUE){
    cex_pnl <- 0.6
    cex_lbl <- 0.55   
    cex_mrk <- 0.7
    cex_lgd <- 0.6
    cex_note <- 0.5
    
    wth_tck_x <- -0.02
    ln_mrk_x <- -0.18
    ln_lbl_x <- 1.5
    
    wth_tck_y <- -0.02
    ln_mrk_y <- -0.8
    ln_lbl_y <- 1
    
    srt_note = 45
    srt_mrk_x = 45
    
    col_text <- 'black'
    col_axs <- col_text
    space <- 0.2
    
    col_all <- 'grey40'
    col_tc <- 'black'  
    lwd <- 3
    gap <- 0.4 

    hist_mn_x <- c(0,-0.6,0)
    hist_mx_x <- c(1,0,1) 
    hist_inr_x <- c(0.2,0.1,0.2)
    hist_mn_y <- c(0,0,0)
    hist_mx_y <- c(2500,2500,2500)
    hist_inr_y <- c(500,500,500)
}

hist_ys <- c('initialResistance','reactionRate','recoveryRate')
ys <- c('resistanceAboveMedianOrNot','reactionRateAboveMedianOrNot','recoveryRateAboveMedianOrNot') 
pnls <- c('Inital resistance','Reaction rate','Recovery rate')
 
tiff(file = '/results/Fig3_logit.tif', width =183, height = 120, units = 'mm', res=300) 
     layout(matrix(c(8,1,2,3,
                     4,5,6,7),2,4, byrow = TRUE),widths=c(0.5,1,1,1),heights = c(0.6,1))
     par(oma =c(0.3,0.2,0.5,0.3),mar =c(3,2,3,1.5),xpd=TRUE,bty="n",bg=NA,lwd=1) 
        
    #histogram
    for(nmb in 1:length(ys)){
        data_hist <- data_input[,c(hist_ys[nmb])]
        data_hist <- data_hist[!is.na(data_hist)]
        
        hist(data_hist,probability = FALSE,main='',xlim=c(hist_mn_x[nmb],hist_mx_x[nmb]),ylim=c(hist_mn_y[nmb],hist_mx_y[nmb]),
             xaxs = 'i', yaxs = 'i',  xaxt="n",xlab="", yaxt="n",ylab="") 
    
        arrows(quantile(data_hist,0.5,na.rm = TRUE),par('usr')[3],quantile(data_hist,0.5,na.rm = TRUE),par('usr')[4],lty=2,col='grey40',length = 0)
        arrows(quantile(data_hist,c(0.25,0.75),na.rm = TRUE),par('usr')[3],quantile(data_hist,c(0.25,0.75),na.rm = TRUE),par('usr')[4],lty=3,col='grey40',length = 0)
    
        axis(1,at=seq(hist_mn_x[nmb],hist_mx_x[nmb],hist_inr_x[nmb]),labels=FALSE,tck=wth_tck_x,pos=0)
        axis(1,at=seq(hist_mn_x[nmb],hist_mx_x[nmb],hist_inr_x[nmb]),labels=seq(hist_mn_x[nmb],hist_mx_x[nmb],hist_inr_x[nmb]),lwd=0,line=ln_mrk_x,cex.axis=cex_mrk)
    
        axis(2,at=seq(hist_mn_y[nmb],hist_mx_y[nmb],hist_inr_y[nmb]),labels=FALSE,tck=wth_tck_x,pos=hist_mn_x[nmb])
        if(nmb==1){
        axis(2,at=seq(hist_mn_y[nmb],hist_mx_y[nmb],hist_inr_y[nmb]),labels=seq(hist_mn_y[nmb],hist_mx_y[nmb],hist_inr_y[nmb]),lwd=0,line=ln_mrk_y,cex.axis=cex_mrk)
        mtext('Count', side=2, line=ln_lbl_y, cex=cex_lbl)}
        
        mtext(pnls[nmb], side=3, line=2, adj=0.5,cex=cex_pnl,font=2)
        mtext(letters[nmb], side=3, line=1, adj=0,cex=cex_pnl,font=2)
    }
    
    #variable names
    for(nmb in 1){
        yy <- ys[nmb]
        data_plot <- data_varImp[which(data_varImp$y==yy),]
        data_plot$prec100 <- data_plot$Estimate/max(abs(data_plot$Estimate))*100
        data_plot <- data_plot[order(-data_plot$order),]    
      
        bp <- barplot(data_plot$prec100,horiz = TRUE,col=NA,border = NA,
                      xlim=c(0,7.8),space = 0.3,xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n", xlab="", ylab="")
        text(rep(10,nrow(data_plot)),bp,data_plot$lbl,col=data_plot$col,adj=1,cex=0.85,font=2)
    }
   
    #bar plots
    for(nmb in 1:length(ys)){
        yy <- ys[nmb]

        r2 <- format(round(data_error[which(data_error$y==yy),'test_error'],digits = 2),nsmall = 2)
                
        data_plot <- data_varImp[which(data_varImp$y==yy),]
        data_plot$prec100 <- data_plot$Estimate/max(abs(data_plot$Estimate))*100
        data_plot <- data_plot[order(-data_plot$order),]    
      
        lmt_x <- c(-100,100);arrowx1 <- -15; arrowx2 <- 20
      
        bp <- barplot(data_plot$prec100,horiz = TRUE,col=adjustcolor(data_plot$colFill,1),border = adjustcolor(data_plot$col,1),
                    xlim=lmt_x,space = 0.3,
                    xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n", xlab="", ylab="")
          
        arrows(seq(-100,100,50),0,seq(-100,100,50),par('usr')[4],length=0,lty=3,col='grey40')
      
        axis(1,at=seq(-100,100,50),labels=FALSE,tck=wth_tck_x,pos=-0.5)
        axis(1,at=seq(-100,100,50),labels=seq(-100,100,50),lwd=0,line=ln_mrk_x,cex.axis=cex_mrk)
        mtext('Variable importance', side=1, line=ln_lbl_x+0.1, cex=cex_lbl)
        
        mtext(letters[nmb+3], side=3, line=1.7, adj=0,cex=cex_pnl,font=2)
        mtext(side=3,bquote("Accuracy "==~.(r2)),line=1.5,font=1,cex=cex_pnl)  
        mtext(side=3,paste0('Less stable'),adj=-0.1,line=0.3,font=3,cex=cex_note)
        mtext(side=3,paste0('More stable'),adj=1.1,line=0.3,font=3,cex=cex_note)
    }
   
dev.off()

  
