  rm(list = ls())
  library(dplyr)
  
  wd <- "C:/YM/co/TCNL/repository" #local
  
  data <- read.csv(paste0(wd,'/data_NL.csv'),stringsAsFactors = FALSE)
  
  #test distribution 
  if(TRUE){
    hist(data$durationBetaMode/max(data$durationBetaMode),freq = F)
    #fitdistrplus::descdist(dataAll$duration, discrete = FALSE)
    set.seed(1234)
    a<-rbeta(100,1.7,8) 
    hist(a,border='red4',col=NA,add=TRUE,freq = F)
    ks.test(data$durationBetaMode/max(data$durationBetaMode),a) #p=0.66
  }
  
  #col
  if(TRUE){
    colHDC <- 'brown4'#rgb(25,100,176, maxColorValue = 255)
    colLDC <- 'orange3'#rgb(81,138,198, maxColorValue = 255)
    colRUR <- 'yellow3'#rgb(123,176,223, maxColorValue = 255)
    
   }
  
  #format
  if(TRUE){  
    cex_tck <- 0.6
    cex_lbl <- 0.65
    cex_note <- 0.6
   
    ln_ylbl <- 1.3
    ln_xlbl <- 0.5
    
    ln_ytck <- -0.4
    ln_xtck <- -1
    
    wth_ytck <- -0.02 
    }
  
  #fig 3 NL hist
  if(TRUE){
   tiff(file = paste0(wd,'/Fig3_NL_hist-2.tiff'), width = 89, height = 80, units = 'mm', res=300) 
    par(oma=c(2,0,0,0),mar=c(0.5,3,0.1,1.5),xpd=TRUE,bg='NA') 
    layout(matrix(seq(1,2,1), 2, 1, byrow = TRUE),heights = c(0.7,1))
    
    xlm <- c(0,32)
    
    #by urban
    if(TRUE){
      #prepare data
      if(TRUE){
        output_agg <- data %>%
          group_by(urbanLevel) %>%
          summarise(mean = mean(durationBetaMode,na.rm = TRUE),
                    SD = sd(durationBetaMode,na.rm = TRUE),
                    q50 = quantile(durationBetaMode,0.50,na.rm = TRUE),
                    q25 = quantile(durationBetaMode,0.25,na.rm = TRUE),
                    q75 = quantile(durationBetaMode,0.75,na.rm = TRUE),
                    q05 = quantile(durationBetaMode,0.05,na.rm = TRUE),
                    q95 = quantile(durationBetaMode,0.95,na.rm = TRUE),
                    area=mean(pxl),.groups = 'drop')
        output_agg_count <- data %>%
          group_by(urbanLevel) %>%
          tally()
        output_agg <- merge(output_agg,output_agg_count)
        output_agg$SE <- output_agg$SD/sqrt(output_agg$n) 
        output_agg$order <- as.numeric(rev(rownames(output_agg)))
        output_agg$col <- c(colHDC,colLDC,colRUR)
        
        fit <- aov(durationBetaMode ~ urbanLevel, data = data)
        summary(fit)
        ltts<-agricolae::HSD.test(fit, trt = c("urbanLevel"), console = TRUE)$groups
        output_agg$ltt <- ltts$groups
      }
    
      bp <- barplot(output_agg$mean,col=NA,border=NA,ylim=c(0.5,3.5),space=0.5,horiz = TRUE,xlim=xlm,
                    xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",xlab='',ylab='')
      
      arrows(output_agg$q05,output_agg$order,output_agg$q95,output_agg$order,col='black',lwd=1,code=3,angle=90,length=0.05)
      wth<-0.3
      for(oo in output_agg$order){
        x <-c(output_agg[which(output_agg$order==oo),'q25'],output_agg[which(output_agg$order==oo),'q25'],
              output_agg[which(output_agg$order==oo),'q75'],output_agg[which(output_agg$order==oo),'q75'])
        
        y <- c(oo-wth,oo+wth,oo+wth,oo-wth)  
        polygon(x, y,border = "black",col=output_agg[which(output_agg$order==oo),'col'])
      }
      points(output_agg$q50,output_agg$order,pch=20,col='black',cex=1.5)
      text(20,output_agg$order,output_agg$ltt,cex=cex_note+0.2,font=3)
      text(21,output_agg$order,paste0('(n=',output_agg$n,')'),cex=cex_note+0.2,font=3,pos=4)
      
      axis(2,at=output_agg$order,labels=output_agg$urbanLevel,lwd=0,las=2,line=ln_xtck+0.5,cex.axis=0.8)
     }
    
    #all
    if(TRUE){
      hist(data$durationBetaMode, breaks=20,ylim=c(0,7000),xlim=xlm,col='grey80',border='white',xaxs = 'i', yaxs = 'i', main='',yaxt="n",xaxt="n",xlab='',ylab='') 
     rug(quantile(data$durationBetaMode,c(0.25,0.5,0.75)), ticksize = 0.05, side = 1, lwd = 2,col='black')  
      rug(quantile(data$durationBetaMode,c(0.05,0.95)), ticksize = 0.05, side = 1, lwd = 2,col='grey50')  
      
      axis(1,at=seq(min(xlm),max(xlm),1),labels=FALSE,tck=wth_ytck,pos=0.2)  
      axis(1,at=seq(min(xlm),max(xlm),5),labels=seq(min(xlm),max(xlm),5),lwd=0,line=ln_xtck+0.2,cex.axis=cex_tck+0.3)
      
      axis(2,at=seq(0,7000,1000),labels=FALSE,tck=wth_ytck,pos=0)
      axis(2,at=seq(0,6000,3000),labels=seq(0,6000,3000),lwd=0,line=ln_ytck-0.2,cex.axis=cex_tck+0.3)
      mtext('Count', side=2, line=ln_ylbl, cex=cex_lbl+0.2)  
     mtext('NTL blackout (day)', side=1, line=ln_xlbl+0.7, cex=cex_lbl+0.2)   
    }
    
    dev.off()
  }  
  
 