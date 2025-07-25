#####requirements#####
#R version 4.4.2
######################

rm(list = ls())

data <- read.csv('/data/data.csv',stringsAsFactors = FALSE)

#format 
  if(TRUE){
    cex_pnl <- 0.7
    cex_lbl <- 0.65  
    cex_mrk <- 0.7
    cex_lgd <- 0.6
    cex_note <- 0.9
    
    wth_tck_x <- -0.03
    ln_mrk_x <- -0.6
    ln_lbl_x <- 0
    
    wth_tck_y <- -0.03
    ln_mrk_y <- -0.5
    ln_lbl_y <- 1.7
        
    col <- 'grey'
  }


tiff(file = '/results/Fig2_damageType.tif', width =210, height = 55, units = 'mm', res=300) 
    layout(matrix(seq(1,2,1),1,2, byrow = FALSE),widths=c(1,1),heights = c(1))
    par(oma =c(1,0.2,0,0.2),xpd=TRUE,bty="n",bg=NA,lwd=1) 
    
    par(mar =c(1,1,1,3))
    #pie
    if(TRUE){
      nIDOnly <- sum((data$initialDamage>0 & data$subsequentDamage==0)) #2080
      prctIDOnly <- round(sum((data$initialDamage>0 & data$subsequentDamage==0))/nrow(data),digits=2)*100 #39%
      
      nIDSD <-  sum((data$initialDamage>0 & data$subsequentDamage>0)) #2053
      prctIDSD <- round(sum((data$initialDamage>0 & data$subsequentDamage>0))/nrow(data),digits=2)*100 #38
     
      nSDOnly <- sum((data$initialDamage==0 & data$subsequentDamage > 0 )) #1228
      prctSDOnly <- round(sum((data$initialDamage== 0 & data$subsequentDamage > 0))/nrow(data),digits=2)*100 #0.23
        
      pie(c(nIDOnly,nIDSD,nSDOnly),col=col,border='black',labels = c(' ',' ',' '),radius=0.7,init.angle = 75)
      par(new=TRUE)
      mtext('a', side=3, line=-0.5, adj=-0.07,cex=cex_pnl,font=2)
      mtext('Occurrence', side=3, line=-0.5, adj=0.5,cex=cex_pnl,font=2)
      
      text(-0.6,0.6,paste0('Initial damage only\n(n = ',nIDOnly,'; ',prctIDOnly,'%)'),pos=2,cex=cex_note-0.2)
      text(-0.8,-1,paste0('Both initial damage & subsequent damage\n(n = ',nIDSD,'; ',prctIDSD,'%)'),pos=4,cex=cex_note-0.2)
      text(0.6,0.6,paste0('Subsequent damage only\n(n = ',nSDOnly,'; ',prctSDOnly,'%)'),pos=4,cex=cex_note-0.2)
    }
    
    par(mar =c(1,3.5,1,0.5))
    #bar
    if(TRUE){
      IDQs<-as.data.frame(t(quantile(data[which(data$initialDamage>0),'initialDamage'],c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE))) 
      IDQs$var <- 'initialDamage'
      IDQs$mean <- mean(data[which(data$initialDamage>0),'initialDamage'],na.rm=TRUE)
      SDQs<- as.data.frame(t(quantile(data[which(data$subsequentDamage > 0),'subsequentDamage'],c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)))      
      SDQs$var <- 'subsequentDamage'  
      SDQs$mean<- mean(data[which(data$subsequentDamage>0),'subsequentDamage'],na.rm=TRUE)   
      agg <- rbind(IDQs,SDQs)
      agg$order <- c(2,1)
      
      plot(agg$mean,agg$order,col=NA,frame=F,xaxs = 'i', yaxs = 'i',  xaxt="n",xlab="", yaxt="n",ylab="",
                xlim=c(-0.15,0.5),ylim=c(0,3))
    
      arrows(agg$`5%`,agg$order,agg$`95%`,agg$order,code=3,angle = 90,length = 0.05,col='black',lwd=1) 
    
      for(oo in agg$order){
      x <-c(agg[which(agg$order==oo),'25%'],agg[which(agg$order==oo),'25%'],
            agg[which(agg$order==oo),'75%'],agg[which(agg$order==oo),'75%'])
      wth <- (par('usr')[4]-par('usr')[3])/10
      y <- c(oo-wth,oo+wth,oo+wth,oo-wth)  
      polygon(x, y,border = "black",col='grey')
      }
      points(agg$'50%',agg$order,col='black',cex=1.5,pch='|')#pch=20
    
      axis(1,at=seq(0,0.5,0.05),labels=FALSE,tck=wth_tck_x+0.01,pos=0.45)
      axis(1,at=seq(0,0.5,0.1),labels=seq(0,50,10),lwd=0,cex.axis=cex_mrk,pos=0.8)
    
      axis(2,at=agg$order,labels=c(paste0('Initial damage\n(n = ',sum(nIDOnly,nIDSD),')'),paste0('Subsequent damage\n(n = ',sum(nSDOnly,nIDSD),')')),lwd=0,tck=0,las=1,pos=0.02,cex.axis=cex_mrk)
    
      mtext('b', side=3, line=-0.5, adj=-0.23,cex=cex_pnl,font=2)
      mtext('Magnitude', side=3, line=-0.5, adj=0.5,cex=cex_pnl,font=2)
      mtext('Damage (% EVI)       ', side=1, line=ln_lbl_x, adj=0.8,cex=cex_lbl)
    }
    
  dev.off()
  
