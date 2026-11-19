#####requirements#####
#R version 4.4.2
######################
rm(list = ls())
library(dplyr)

wd <- "C:/YM/CHES/TCMangroveRe/github/"

data <- read.csv(paste0(wd,'input_GMW.csv'),stringsAsFactors = FALSE) #
data<-data[which(data$year<=2021),]

#set col
if(FALSE){
    cntList <- c("Africa",   
                 "Americas",
                 "Asia",
                 "Oceania")
    pchList <- c(21,22,23,24)
    colList <- c( rgb(221,204,119,maxColorValue = 255),
                 rgb(204,102,119,maxColorValue = 255),
                  rgb(68, 170, 153,maxColorValue = 255),
                 rgb(136, 204,238,maxColorValue = 255))
  
    lvl <- data.frame(order=c(1:4) ,region=cntList,pch=pchList,col=colList,colbdr=colList)
   
    typList <- c("OpenCoast", "Lagoon","Estuary", "Delta")
    lvlTyp <- data.frame(order=c(1:4) ,typ=typList,pch=pchList[1:4],col=colList[1:4],colbdr=colList[2:5])

    
    }

#Figure 4 plot format 
if(TRUE){
  cex_pnl <- 0.6
  cex_lbl <- 0.6   
  cex_mrk <- 0.8
  cex_lgd <- 0.6
  cex_note <- 0.5
  
  wth_tck_x <- -0.03
  ln_mrk_x <- -0.6
  ln_lbl_x <- 1.4
  
  wth_tck_y <- -0.03
  ln_mrk_y <- -0.6
  ln_lbl_y <- 1.5
  
  srt_note = 45
  srt_mrk_x = 45
  
  col_text <- 'black'
  col_axs <- col_text
  space <- 0.2
  
  col_all <- 'grey40'
  col_tc <- 'black'  
  lwd <- 3
  gap <- 0.4 
  
  col_up <-rgb(68,170,153,maxColorValue = 255)
  col_down <- rgb(204,102,119,maxColorValue = 255)
  
}

#fig 4 by continent and typology
if(TRUE){
yys <- c('initialResistance','reactionRate','recoveryRate')
ylbs <- c('Resistance (%)','Rate (%/month)','Rate (%/month)')
ypnls <- c('Inital resistance','Reaction rate','Recovery rate')
typList <- c("Coast", "Delta","Estuary","Lagoon")
cntList <- c("Africa", "Americas","Asia", "Oceania")
cntNote <- c("Af", "Am","As", "Oc")

slope_all <- data.frame() 

tiff(file = paste0(wd,'/FigS7.tif'), width =183, height = 140, units = 'mm', res=300) 
layout(matrix(seq(1,12,1),4,3, byrow = FALSE),widths=c(1),heights = c(1))
par(oma =c(2,4,2,0),mar =c(2,4,0.5,1),xpd=TRUE,bty="n",bg=NA,lwd=1) 

for (yyN in c(1:3)){
#yyN <-1  
for (typN in c(1:4)){
#typN <-2
  
if(yys[yyN]=='initialResistance'){  
if(typList[typN]=='Coast'){ylms <- c(0.73,0.86);ytcks<- seq(0.73,0.86,0.01);ymrks<-seq(0.75,0.85,0.05);noteX<-c(2020.5,2020.5,2020.5,2020.5);noteY<-c(0.84,0.805,0.745,0.76)}  
if(typList[typN]=='Delta'){ylms <- c(0.74,1);ytcks<- seq(0.74,1,0.02);ymrks<-seq(0.8,1,0.1);noteX<-c(2020.5,2012,2020.5,2012);noteY<-c(0.83,0.865,0.76,0.92)}  
if(typList[typN]=='Estuary'){ylms <- c(0.68,0.92);ytcks<- seq(0.68,0.92,0.02);ymrks<-seq(0.7,0.9,0.1);noteX<-c(2020.5,2020.5,2020.5,2020.5);noteY<-c(0.73,0.89,0.78,0.82)}    
if(typList[typN]=='Lagoon'){ylms <- c(0.68,0.9);ytcks<- seq(0.68,0.9,0.02);ymrks<-seq(0.7,0.9,0.1);noteX<-c(2020.5,2020.5,2020.5,2020.5);noteY<-c(0.845,0.78,0.7,0.825)}     
}

if(yys[yyN]=='reactionRate'){    
if(typList[typN]=='Coast'){ylms <- c(-0.2,-0.08);ytcks<- seq(-0.2,-0.08,0.01);ymrks<-seq(-0.2,-0.08,0.05);noteX<-c(2016,2016,2020.5,2020.5);noteY<-c(-0.12,-0.091,-0.12,-0.165)}  
if(typList[typN]=='Delta'){ylms <-c(-0.36,0);ytcks<- seq(-0.36,0,0.02);ymrks<-seq(-0.3,0,0.1);noteX<-c(2020.5,2020.5,2020.5,2020.5);noteY<-c(-0.155,-0.05,-0.185,-0.08)}  
if(typList[typN]=='Estuary'){ylms <- c(-0.22,-0.06);ytcks<- seq(-0.22,-0.06,0.02);ymrks<-seq(-0.2,-0.1,0.1);noteX<-c(2020.5,2020.5,2020.5,2020.5);noteY<-c(-0.19,-0.16,-0.145,-0.13)}    
if(typList[typN]=='Lagoon'){ylms <-c(-0.17,-0.07);ytcks<- seq(-0.17,-0.07,0.01);ymrks<-seq(-0.15,-0.1,0.05);noteX<-c(2020.5,2020.5,2020.5,2020.5);noteY<-c(-0.135,-0.105,-0.115,-0.148)}       
}
  
if(yys[yyN]=='recoveryRate'){
if(typList[typN]=='Coast'){ylms <- c(0.04,0.22);ytcks<- seq(0.04,0.22,0.02);ymrks<-seq(0.1,0.2,0.1);noteX<-c(2020.5,2020.5,2012,2012);noteY<-c(0.145,0.125,0.187,0.162)}  
if(typList[typN]=='Delta'){ylms <-c(0.06,0.2);ytcks<- seq(0.06,0.2,0.02);ymrks<-seq(0.1,0.2,0.1);noteX<-c(2016,2020.5,2016,2020.5);noteY<-c(0.16,0.12,0.195,0.15)}  
if(typList[typN]=='Estuary'){ylms <- c(0.06,0.2);ytcks<- seq(0.06,0.2,0.02);ymrks<-seq(0.1,0.2,0.1);noteX<-c(2020.5,2020.5,2020.5,2020.5);noteY<-c(0.1,0.14,0.16,0.125)}    
if(typList[typN]=='Lagoon'){ylms <-c(0.06,0.2);ytcks<- seq(0.06,0.2,0.02);ymrks<-seq(0.1,0.2,0.1);noteX<-c(2016,2020.5,2016,2020.5);noteY<-c(0.155,0.08,0.18,0.125)}       
}
  
plot(0,0,xlim=c(2001,2022),ylim=ylms,xaxs = 'i', yaxs = 'i', xaxt="n", xlab="", ylab='',yaxt="n",frame = FALSE)
  
#axis 
if(TRUE){
    axis(1,at=seq(2001,2021,1),labels=FALSE,tck=wth_tck_x)
    if(typN==4){axis(1,at=seq(2001,2021,5),labels=seq(2001,2021,5),lwd=0,line=ln_mrk_x,cex.axis=cex_mrk)
                mtext('Year', side=1, line=ln_lbl_x, cex=cex_lbl)}
    axis(2,at=range(ylms),labels=FALSE,tck=0)
    axis(2,at=ytcks,labels=FALSE,tck=wth_tck_y)
    axis(2,at=ymrks,labels=ymrks,lwd=0,line=ln_mrk_y,cex.axis=cex_mrk)
    mtext(ylbs[yyN], side=2, line=ln_lbl_y, cex=cex_lbl)
    #mtext(letters[1+(yy-1)*2], side=3, line=0.5,adj=-0.15, cex=cex_pnl,font=2)
    if(typN==1){mtext(ypnls[yyN], side=3, line=1,font=2, cex=cex_pnl)}
    
    
    #stable note
    if(yyN==1){text(1995.5,par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.95,paste0('More\nstable'),font=3,cex=cex_note+0.2,col='grey')
               text(1995.5,par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.05,paste0('Less\nstable'),font=3,cex=cex_note+0.2,col='grey')
               arrows(1995.5,par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.8,1995.5,par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.6,col='grey',lwd=1,code=1,angle=35,length=0.04)
               arrows(1995.5,par('usr')[4]-(par('usr')[4]-par('usr')[3])*0.8,1995.5,par('usr')[4]-(par('usr')[4]-par('usr')[3])*0.6,col='grey',lwd=1,code=1,angle=35,length=0.04)}
  }

for(cntN in c(1:4)){
  #cntN <-1   
  
  #prepare data
  if(TRUE){
    input <- data[which(data$typology==typList[typN] & data$continent==cntList[cntN]),c(yys[yyN],'year')]
    colnames(input) <- c('y','year')
    input <- input[complete.cases(input),]
    
    
    data_plot <- data.frame(year=seq(2001,2021))
    
    #log transform
    if(TRUE){
      input$y2 <- input$y
      if(min(input$y)<=0){input$y2 <- input$y+abs(min(input$y))+1}
      fit <- glm(log10(y2)~year,data=input)  
     
      slope <- data.frame('yy'=yys[yyN],'typ' = typList[typN],'cnt'=cntList[cntN])
      slope$estimate<- summary(fit)$coefficients[2,1]
      slope$se<- summary(fit)$coefficients[2,2]
      slope$p<- summary(fit)$coefficients[2,4]
      slope$r2 <- summary(fit)$r.squared
     
      data_plot$prd0 <-  predict(fit,data_plot, type = "response",se.fit=TRUE)$fit
      data_plot$lwr0 <-  predict(fit,data_plot, type = "response",se.fit=TRUE)$fit-predict(fit,   data_plot, type = "response",se.fit=TRUE)$se.fit
      data_plot$upr0 <-  predict(fit,data_plot, type = "response",se.fit=TRUE)$fit+predict(fit,data_plot, type = "response",se.fit=TRUE)$se.fit
      
      data_plot$prd <-  10^data_plot$prd0
      data_plot$lwr <-  10^data_plot$lwr0
      data_plot$upr <-  10^data_plot$upr0
      
      if(min(input$y)<=0){
        data_plot$prd <- 10^data_plot$prd0-1-abs(min(input$y))
        data_plot$lwr <- 10^data_plot$lwr0-1-abs(min(input$y))
        data_plot$upr <- 10^data_plot$upr0-1-abs(min(input$y))
      }
      
      slope$estRlt <- (10^slope$estimate-1)*100
      slope$seRlt <- (10^slope$se-1)*100
    
      slope_all <- rbind(slope_all,slope)
    } 
  }

  #plot
  if(TRUE){
    if(slope$estimate>0){col <- col_up} else {col <- col_down}
    lty <- 3;lwd <- 0.8;f <- 0.1
    if(slope$p<0.05){lty<-1;lwd<-1.5;f <- 0.6}
    
    if(yys[yyN]=='initialResistance' & typList[typN]=='Delta' & cntList[cntN]=='Oceania'){par(xpd=FALSE)}#outside drawing off
    polygon(c(data_plot$year,rev(data_plot$year)),c(data_plot$upr,rev(data_plot$lwr)),border = NA,col = adjustcolor(col,alpha.f = f))
    
    if(yys[yyN]=='initialResistance' & typList[typN]=='Delta'& cntList[cntN]=='Oceania'){par(xpd=TRUE)} #outside drawing back
    
    lines(data_plot$year,data_plot$prd,col=col,lwd=lwd,lty=lty)
    
    
    text(noteX[cntN],noteY[cntN],cntNote[cntN],cex=cex_note+0.3,pos=4)
  }
  
}  
}
}
dev.off()
}


#fig 4 slope bar
for(cntN in c(1:4)){
tiff(file = paste0(wd,'/fig4estRlt_',cntList[cntN],'.tif'), width =70, height = 25, units = 'mm', res=300)

layout(matrix(c(1,2,3),1,3, byrow = TRUE),widths=c(1),heights = c(1))
par(oma =c(0,0,0,0),mar =c(1,2,2.2,0.1),xpd=TRUE,bty="n",bg=NA,lwd=1) 

for(yyN in c(1:3)){  
  
if(cntList[cntN]=='Africa' & yys[yyN]=='initialResistance'){ylms2 <- c(-0.8,0.4);ytcks2<- seq(-0.8,0.4,0.2);ymrks2<- seq(-0.8,0.4,0.4)}  
if(cntList[cntN]=='Africa' & yys[yyN]=='reactionRate'){ylms2 <- c(-0.5,0.3);ytcks2<- seq(-0.5,0.3,0.1);ymrks2<- seq(-0.4,0.2,0.2)}  
if(cntList[cntN]=='Africa' & yys[yyN]=='recoveryRate'){ylms2 <- c(-3,4);ytcks2<- seq(-3,4,1);ymrks2<- seq(-2,4,2)}  

if(cntList[cntN]=='Americas'& yys[yyN]=='initialResistance'){ylms2 <- c(-0.3,1.5);ytcks2<- seq(-0.3,1.5,0.3);ymrks2<- seq(0,1.5,0.3)}  
if(cntList[cntN]=='Americas'& yys[yyN]=='reactionRate'){ylms2 <- c(-0.3,0.3);ytcks2<- seq(-0.3,0.3,0.1);ymrks2<- seq(-0.3,0.3,0.3)}  
if(cntList[cntN]=='Americas'& yys[yyN]=='recoveryRate'){ylms2 <- c(-1,4);ytcks2<- seq(-1,4,1);ymrks2<- seq(0,4,2)}  
  
if(cntList[cntN]=='Asia'& yys[yyN]=='initialResistance'){ylms2 <- c(-1.2,0);ytcks2<- seq(-1.2,0,0.2);ymrks2<- seq(-1.2,0,0.4)}  
if(cntList[cntN]=='Asia'& yys[yyN]=='reactionRate'){ylms2 <- c(-0.4,0.2);ytcks2<- seq(-0.4,0.2,0.1);ymrks2<- seq(-0.4,0.2,0.2)}  
if(cntList[cntN]=='Asia'& yys[yyN]=='recoveryRate'){ylms2 <- c(-2,1.5);ytcks2<- seq(-2,1.5,0.5);ymrks2<- seq(-2,1,1)}  
  
if(cntList[cntN]=='Oceania'& yys[yyN]=='initialResistance'){ylms2 <- c(-1.5,0.5);ytcks2<- seq(-1.5,0.5,0.5);ymrks2<- seq(-1.5,0.5,0.5)}  
if(cntList[cntN]=='Oceania'& yys[yyN]=='reactionRate'){ylms2 <- c(-0.5,1.5);ytcks2<- seq(-0.5,1.5,0.5);ymrks2<- seq(-0.5,1.5,0.5)}  
if(cntList[cntN]=='Oceania'& yys[yyN]=='recoveryRate'){ylms2 <- c(-3,6);ytcks2<- seq(-3,6,3);ymrks2<- seq(-3,6,3)}  
    
data_plot<- slope_all[which(slope_all$cnt==cntList[cntN] & slope_all$yy==yys[yyN]),]
data_plot$colbdr <- col_up
data_plot[which(data_plot$estRlt<0),'colbdr']<- col_down  
data_plot$col <- data_plot$colbdr
data_plot[which(data_plot$p>=0.05),'col']<-'white'  
data_plot<-data_plot[order(data_plot$typ),] 
 
#cc <-1
bp <- barplot(data_plot$estRlt,horiz = FALSE,col=adjustcolor(data_plot$col,1),border = adjustcolor(data_plot$colbdr,1),
                space = 0.2,xlim=c(-0.1,5),ylim=ylms2,#width = rank(data_plot$prec100),
                xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n", xlab="", ylab="")#
arrows(bp,data_plot$estRlt-data_plot$seRlt,bp,data_plot$estRlt+data_plot$seRlt,col='black',lwd=1,code=3,angle=90,length=0.03)
arrows(par('usr')[1],0,par('usr')[2],0,col='black',lty=1,lwd=1,code=3,angle=90,length=0)
  
#axis 
if(TRUE){
axis(1,at=bp,labels=c('C','D','E','L'),font=2,lwd=0,line=ln_mrk_x-0.5,cex.axis=cex_mrk)
  
axis(2,at=ytcks2,labels=FALSE,tck=wth_tck_y-0.01)
axis(2,at=ymrks2,labels=ymrks2,lwd=0,line=ln_mrk_y-0.2,cex.axis=cex_mrk-0.1)
if(yys[yyN]=='initialResistance'){mtext(paste0('Rate (%/year)'), side=2,line=ln_lbl_y-0.5, cex=cex_lbl-0.1)}
mtext(ypnls[yyN], side=3, line=0.5,adj=0.05, cex=cex_pnl-0.1,font=2)
if(yys[yyN]=='reactionRate'){mtext(cntList[cntN], side=3, line=1.4,adj=0.05, cex=cex_pnl,font=2)}
}

}
dev.off()  
}

















