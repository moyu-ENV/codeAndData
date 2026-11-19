#For guidance, Nature's standard figure sizes are 89 mm wide (single column) and 183 mm wide (double column). 
#The full depth of a Nature page is 247 mm. Figures can also be a column-and-a-half where necessary (120-136 mm).

if(TRUE){
  rm(list = ls())
  #library(rworldmap)
  #library(rgdal)
  library(dplyr)
  #library(chisq.posthoc.test) 
  
  wd <- paste0("C:/YM/CHES/TCMangroveRe/github/")
  #wd <- paste0("C:/YM/TCMangroveRe/")
  
  data <- read.csv(paste0(wd,'input_GMW.csv'),stringsAsFactors = FALSE)
  
  #format 
  if(TRUE){
    cex_pnl <- 0.7
    cex_lbl <- 0.65  
    cex_mrk <- 0.9
    cex_lgd <- 0.6
    cex_note <- 0.9
    
    axis_tck_x <- -0.03
    axis_mrkLine_x <- -0.6
    axis_lblLine_x <- 1.5
    
    axis_tck_y <- -0.03
    axis_mrkLine_y <- -0.5
    axis_lblLine_y <- 1.7
    
    srt_note = 45
    srt_mrk_x = 45
    
    col_text <- 'black'
    col_axs <- col_text
  }
  
  #fig 2 damage form 
  if(TRUE){
    col <- 'grey'
    tiff(file = paste0(wd,'Fig2damageForm.tif'), width =183, height = 55, units = 'mm', res=300) 
    layout(matrix(seq(1,2,1),1,2, byrow = FALSE),widths=c(1,1),heights = c(1))
    par(oma =c(0,0.2,0,0.2),xpd=TRUE,bty="n",bg=NA,lwd=1) 
    
    par(mar =c(1,1,1,3))
    
    #get input
    if(TRUE){
      iniN<- sum((data$resistance0<1 & data$reaction==0),na.rm=TRUE) 
      iniP<- round(sum((data$resistance0<1 & data$reaction==0),na.rm=TRUE)/nrow(data)*100,digits = 0) 

      botN<-sum((data$resistance0<1 & data$reaction<0),na.rm=TRUE) 
      botP<-round(sum((data$resistance0<1 & data$reaction<0),na.rm=TRUE)/nrow(data)*100,digits = 0) 
      
      subN<-sum((data$resistance0>=1& data$reaction<0 ),na.rm=TRUE) 
      subP<-round(sum((data$resistance0>=1& data$reaction<0),na.rm=TRUE)/nrow(data)*100,digits = 0) 
      }
    
    
    #pie
    if(TRUE){
      pie(c(iniN,botN,subN),col=col,border='black',labels = c(' ',' ',' '),radius=0.7,init.angle = 75)
      par(new=TRUE)
      mtext('a', side=3, line=-0.5, adj=-0.07,cex=cex_pnl,font=2)
      mtext('Occurrence', side=3, line=-0.5, adj=0.5,cex=cex_pnl,font=2)
       
      text(-0.6, 0.6, bquote(atop("Initial damage only","(" * italic(n) * " = " * .(iniN) * "; " * .(iniP) * "%)")),pos = 2, cex = cex_note - 0.2)
      text(-0.8, -1, bquote(atop("Both initial damage & subsequent damage", "(" * italic(n) * " = " * .(botN) * "; " * .(botP) * "%)")), pos = 4, cex = cex_note - 0.2)
      text(0.6, 0.6, bquote(atop("Subsequent damage only", "(" * italic(n) * " = " * .(subN) * "; " * .(subP) * "%)")), pos = 4, cex = cex_note - 0.2)
    }
    
    par(mar =c(1,3.5,1,0.5))
    #bar
    if(TRUE){
    dist0Qs<-as.data.frame(t(quantile(data[which(data$resistance0<1),'disturbance0'],c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE))) 
    dist0Qs$var <- 'disturbance0'
    dist0Qs$mean <- mean(data[which(data$resistance0<1),'disturbance0'],na.rm=TRUE)
    reactionQs<- as.data.frame(t(quantile(-data[which(data$reaction<0),'reaction'],c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)))      
    reactionQs$var <- 'reaction'  
    reactionQs$mean<- -mean(data[which(data$reaction<0),'reaction'],na.rm=TRUE)   
    agg <- rbind(dist0Qs,reactionQs)
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
     
    axis(1,at=seq(0,0.5,0.05),labels=FALSE,tck=axis_tck_x+0.01,pos=0.45)
    axis(1,at=seq(0,0.5,0.1),labels=seq(0,50,10),lwd=0,cex.axis=cex_mrk-0.2,pos=0.8)
    
     axis(2, at = agg$order,
         labels = as.expression(c(bquote(atop("Initial damage","(" * italic(n) * " = " * .(sum(iniN, botN)) * ")")),
           bquote(atop("Subsequent damage","(" * italic(n) * " = " * .(sum(subN, botN)) * ")")))),
         lwd = 0, tck = 0, las = 1, pos = 0.02,cex.axis = cex_mrk - 0.2)
    mtext('b', side=3, line=-0.5, adj=-0.23,cex=cex_pnl,font=2)
    mtext('Magnitude', side=3, line=-0.5, adj=0.5,cex=cex_pnl,font=2)
    mtext('Damage (% EVI)       ', side=1, line=axis_lblLine_x-2.2, adj=0.8,cex=cex_lbl)
    }
    
     dev.off()
  }
  
  
  #by region and type
  if(TRUE){
   typList <- c("Coast", "Delta","Estuary","Lagoon" )
   cntList <- c("Africa", "Americas","Asia", "Oceania")
   col <- 'grey'
  
   for(grpN in 1:8){
   if(grpN <=4){input <- data[which(data$typologyNearest==typList[grpN]),];grp <- typList[grpN];ltt <- letters[grpN]}
   if(grpN >4){input <- data[which(data$continent==cntList[grpN-4]),];grp <-cntList[grpN-4];ltt <- letters[grpN-4]}     
    
   #get input
   if(TRUE){
         iniN<- sum((input$resistance0<1 & input$reaction==0),na.rm=TRUE) 
         iniP<- round(sum((input$resistance0<1 & input$reaction==0),na.rm=TRUE)/nrow(input)*100,digits = 0) 
          
         botN<-sum((input$resistance0<1 & input$reaction<0),na.rm=TRUE) 
         botP<-round(sum((input$resistance0<1 & input$reaction<0),na.rm=TRUE)/nrow(input)*100,digits = 0) 
          
         subN<-sum((input$resistance0>=1& input$reaction<0 ),na.rm=TRUE) 
         subP<-round(sum((input$resistance0>=1& input$reaction<0),na.rm=TRUE)/nrow(input)*100,digits = 0) 
    } 
       
       
    tiff(file = paste0(wd,'Fig2damageType_',grp,'.tif'), width =183, height = 55, units = 'mm', res=300) 
    layout(matrix(seq(1,2,1),1,2, byrow = FALSE),widths=c(1,1),heights = c(1))
    par(oma =c(0,0.2,0,0.2),xpd=TRUE,bty="n",bg=NA,lwd=1) 
    
    par(mar =c(0.5,1,1.5,3))
    #pie
    if(TRUE){
      pie(c(iniN,botN,subN),col=col,border='black',labels = c(' ',' ',' '),radius=0.7,init.angle = 75)
      par(new=TRUE)
      mtext(paste0(ltt,' ',grp), side=3, line=0, adj=-0.07,cex=cex_pnl,font=2)
      mtext('Occurrence', side=3, line=-1, adj=0.5,cex=cex_pnl,font=2)
      
      text(-0.6, 0.6, bquote(atop("Initial damage only","(" * italic(n) * " = " * .(iniN) * "; " * .(iniP) * "%)")),pos = 2, cex = cex_note - 0.2)
      text(-0.8, -1, bquote(atop("Both initial damage & subsequent damage", "(" * italic(n) * " = " * .(botN) * "; " * .(botP) * "%)")), pos = 4, cex = cex_note - 0.2)
      text(0.6, 0.6, bquote(atop("Subsequent damage only", "(" * italic(n) * " = " * .(subN) * "; " * .(subP) * "%)")), pos = 4, cex = cex_note - 0.2)
    }
    
    par(mar =c(0.5,3.5,1.5,0.5))
    #bar
    if(TRUE){
      dist0Qs<-as.data.frame(t(quantile(input[which(input$resistance0<1),'disturbance0'],c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE))) 
      dist0Qs$var <- 'disturbance0'
      dist0Qs$mean <- mean(input[which(input$resistance0<1),'disturbance0'],na.rm=TRUE)
      reactionQs<- as.data.frame(t(quantile(-input[which(input$reaction<0),'reaction'],c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)))      
      reactionQs$var <- 'reaction'  
      reactionQs$mean<- -mean(input[which(input$reaction<0),'reaction'],na.rm=TRUE)   
      agg <- rbind(dist0Qs,reactionQs)
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
      
      axis(1,at=seq(0,0.5,0.05),labels=FALSE,tck=axis_tck_x+0.01,pos=0.45)
      axis(1,at=seq(0,0.5,0.1),labels=seq(0,50,10),lwd=0,cex.axis=cex_mrk-0.2,pos=0.8)
      
      axis(2, at = agg$order,
           labels = as.expression(c(bquote(atop("Initial damage","(" * italic(n) * " = " * .(sum(iniN, botN)) * ")")),
                                    bquote(atop("Subsequent damage","(" * italic(n) * " = " * .(sum(subN, botN)) * ")")))),
           lwd = 0, tck = 0, las = 1, pos = 0.02,cex.axis = cex_mrk - 0.2)
      
      mtext('Magnitude', side=3, line=-1, adj=0.5,cex=cex_pnl,font=2)
      mtext('Damage (% EVI)       ', side=1, line=axis_lblLine_x-2.2, adj=0.8,cex=cex_lbl)
    }
    
    dev.off()
  }#loop grp
  }
  
   }


  