#####requirements#####
#R version 4.4.2
######################

rm(list = ls())
#library(dplyr)
#library(tidyr)
  
wd <- paste0("C:/YM/CHES/TCMangroveModel/") 

data <- read.csv(paste0(wd,'/result/model/output_en.csv'),stringsAsFactors = FALSE)
data_i <- read.csv(paste0(wd,'/result/model/data.csv'),stringsAsFactors = FALSE)
  
data2 <- merge(data[,c("sampleID",'obs',"pred_en_OOB_rcl")],
                 data_i[,c("sampleID",'continent')])
colnames(data2) <- c("sampleID","obs","model", "continent")
  
#col & symbol
if(TRUE){
    reg <- unique(data2$continent0)
    regList <-  c("North America", "Africa", "Asia", "Oceania")
    pchList <-c(1,2,3,4)
    cexList <- c(1,0.7,0.7,0.8)
    data2$pch <-NA
    data2$cex <- NA
    for(ii in 1:length(regList)){
      data2[which(data2$continent==regList[ii]),'pch']<-pchList[ii]
      data2[which(data2$continent==regList[ii]),'cex']<-cexList[ii]
    }  
  }
  
  
#format
if(TRUE){  
    cex_tck <- 0.6
    cex_lbl <- 0.65
    cex_pnl <- 0.7
    cex_note <- 0.6
    
    ln_ylbl <- 1.1
    ln_xlbl <- 0.45
   
    ln_ytck <- -0.7
    ln_xtck <- -1.1
   
    wth_ytck <- -0.025 
    wth_xtck <- -0.02
   
   
    lty_line <- 1
    lwd_line <- 2
    lty_line2 <- 3
    
    col_line <- rgb(0.8,0.1,0.1) 
    col_abline<- rgb(199,152,42,maxColorValue = 255)#rgb(193,57,61,maxColorValue = 255)
  }

#plot  
if(TRUE){
  #pdf(file = paste0(wd,'/result/figure1c.pdf'),width = 2.3622,height = 1.61417)   
  tiff(file = paste0(wd,'/result/figure/figure1c.tif'), width = 60, height = 41, units = 'mm', res=300) 
  par(oma=c(0,0,0,0),mar=c(1.5,2.1,0.7,2.7),xpd=TRUE,bg='NA') 
  layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))
  
  #points
  if(TRUE){    
    data_sub <- data2 
    plot(data_sub$obs, data_sub$model,pch=data2$pch,cex=data_sub$cex,
         col=NA,
         ylim=c(0,0.85),xlim=c(0,0.85),
         xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",frame=FALSE,
         xlab='',ylab='') 
    
    arrows(0,0,0.6,0.6, length=0, angle=90, code=3,lwd=1,col=col_abline)
    points(data_sub$obs, data_sub$model,pch=data2$pch,cex=data_sub$cex,lwd=0.5,col=adjustcolor('black',alpha.f = 1))
    
    r2All <- sum((data_sub[,"model"] - mean(data_sub[,'obs']))^2)/sum((data_sub[,'obs'] - mean(data_sub[,'obs']))^2)
    r2All2<- format(round(r2All,digits=2),nsmall = 2)
    
    text(0.2,0.85, bquote(italic('r')^2 ==~.(r2All2)), col=col_abline,cex=cex_note,font=3)
    
    axis(1,at=seq(0,0.85,0.05),labels=FALSE,tck=wth_xtck,pos=0)
    axis(1,at=seq(0,0.8,0.2),labels=seq(0,80,20),
         lwd=0,line=ln_xtck,cex.axis=cex_tck)
    mtext('Observation (% EVI)', side=1, line=ln_xlbl, cex=cex_lbl)
    
    axis(2,at=seq(0,0.85,0.05),labels=FALSE,tck=wth_ytck,pos=0)
    axis(2,at=seq(0,0.8,0.2),labels=seq(0,80,20),lwd=0,line=ln_ytck,cex.axis=cex_tck,las=1)
    mtext('Prediction (% EVI)', side=2, line=ln_ylbl, cex=cex_lbl)  
    
    mtext('c',3,line=0,adj=-0.3,cex=cex_pnl,font=2)
  }
  
  par(new=TRUE,fig = c(0.69, 1, 0.72,1), mar = c(0,0,0.5,1),lwd = 0.5)

  #RMSE
  if(TRUE){
    data_sub_af <- data_sub[which(data_sub$continent=='Africa'),]
    r2_af <- sqrt(sum((data_sub_af[,'obs']-data_sub_af[,"model"])^2)/nrow(data_sub_af))
    data_sub_as <- data_sub[which(data_sub$continent=='Asia'),]
    r2_as <- sqrt(sum((data_sub_as[,'obs']-data_sub_as[,"model"])^2)/nrow(data_sub_as))
    data_sub_na <- data_sub[which(data_sub$continent=='North America'),]
    r2_na <- sqrt(sum((data_sub_na[,'obs']-data_sub_na[,"model"])^2)/nrow(data_sub_na))
    data_sub_oc <- data_sub[which(data_sub$continent=='Oceania'),]
    r2_oc <- sqrt(sum((data_sub_oc[,'obs']-data_sub_oc[,"model"])^2)/nrow(data_sub_oc))
    br <- barplot(c(r2_oc,r2_na,r2_as,r2_af),horiz = TRUE,xlim=c(0,0.13),space = 0.25,border=NA,
                  col=adjustcolor(col_abline,alpha.f = 0.5),
                  xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n", xlab='',ylab='')
    axis(1,at=seq(0,0.12,0.02),labels=FALSE,tck=-0.07,pos=0,lwd=1)
    axis(1,at=seq(0,0.12,0.12),labels=seq(0,12,12),lwd=0,line=-1.1,cex.axis=cex_tck)
    mtext('RMSE', side=1, line=0.30, cex=cex_lbl,las=1)
    text(-0.02,br,c("Oceania","America",'Asia','Africa'),cex=0.6,pos=4)
  }
  
  dev.off()
}



