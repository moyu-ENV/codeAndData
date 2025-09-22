rm(list = ls())
library(dplyr)
wd <- "C:/YM/co/TCNL/repository" #local

data <- read.csv(paste0(wd,'/data_NL.csv'),stringsAsFactors = FALSE)
tracks <- read.csv(paste0(wd,'/data_ibtracs.csv'),stringsAsFactors = FALSE)

#format
if(TRUE){  
  cex_tck <- 0.8
  cex_lbl <- 0.6
  cex_pnl <- 0.7
  cex_note <- 0.85
  
  ln_ylbl <- 1.5
  ln_xlbl <- 1.1
  
  ln_ytck <- -0.4
  ln_xtck <- -0.8
  
  wth_ytck <- -0.02 
  wth_xtck <- -0.02
  
  colHDC <- 'brown4'#rgb(25,100,176, maxColorValue = 255)
  colLDC <- 'orange3'#rgb(81,138,198, maxColorValue = 255)
  colRUR <- 'yellow3'#rgb(123,176,223, maxColorValue = 255)
  
  col_line <- rgb(0.8,0.1,0.1) 
  col_abline<- 'orange2'
}

yyList <- c('stormN','stormBlN','blD')
ypnlList <- c('Recorded storms','Blackout storms','Blackout duration')
ylblList <- c('Count','Count','Duration (day)')
urbList <- c('HDC','LDC','RUR')
colList <- c(colHDC,colLDC,colRUR)
cexList <- c(1.2,1.2,1.4)
pchList <- c(16,17,18)
ymax <- c(120,50,7)
ymin <- c(0,10,3.5)
ytck <- c(10,5,0.5)
ymrk <- c(30,10,1)

#plot
if(TRUE){
  tiff(file = paste0(wd,'/Fig4_overTime2.tiff'), width = 89, height = 150, units = 'mm', res=300) 
  par(oma=c(1,1,1,1),mar=c(2,3,2,0.5),xpd=TRUE,bg='NA',lend=1) 
  layout(matrix(seq(1,3,1), 3, 1, byrow = FALSE))
  
  for(pp in 1:3){
  #pp<-1  
  yy <- yyList[pp]  
  if(yy=='stormN'){
    stms <- unique(tracks[,c('SID','SEASON')])
    input <- stms %>%filter(SEASON>=2012 & SEASON<=2021)%>%group_by(SEASON)%>%tally()  
    colnames(input)<-c('xx','yy')
    fit <- glm(yy~xx, data= input,family =poisson())
   
    table<-summary(fit)$coe
    p<-table[2,4]
   
    pred <- predict(fit,type = "response",se.fit=TRUE)$fit
    se <- predict(fit,type = "response",se.fit=TRUE)$se.fit
    
    plot(input$xx,input$yy,col=NA,xlim=c(2011.5,2021.5),ylim=c(ymin[pp],ymax[pp]),
         main='',xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",xlab='',ylab='',frame=FALSE)
    segments(input$xx,0,input$xx,input$yy,col=adjustcolor('grey',alpha.f = 0.6),lwd=10)
    
    lty<-2
    if(p<=0.05){lty <- 1}
    lines(input$xx,pred,col=adjustcolor('grey30',alpha.f = 1),lty=lty,lwd=1)
    #lines(input$xx,pred+se,col=adjustcolor('grey30',alpha.f = 1),lty=3,lwd=1)
    #lines(input$xx,pred-se,col=adjustcolor('grey30',alpha.f = 1),lty=3,lwd=1)
    
    #mtext(,3,line=-1.5,adj=1,font=1,cex=cex_note,col=col_abline)
    pPlot<- paste0('= ',format(round(p,digits = 2),nsmall = 2))
    text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
         par("usr")[3]+(par("usr")[4]-par("usr")[3])*0.98,cex=cex_note,pos=4,
         bquote(italic("Yr: P")~'= 0.07')) 
    }  
    
  if(yy=='stormBlN'){
    stms_NL <- unique(data[which(data$year<=2021),c('sid','year','urbanLevel')])
    input <-  stms_NL %>% group_by(year,urbanLevel)%>%tally()  
    fit <- glm(n~year+urbanLevel, data=input,family =poisson())
    
    table<-summary(fit)$coe
    p<-table[2,4]
    xx <- seq(2012,2021,1)
    prd <- data.frame(urbanLevel=rep(c('HDC','LDC','RUR'),each=length(xx)),year=rep(xx,3))
    prd$pred <- predict(fit,prd,type = "response",se.fit=TRUE)$fit
    prd$se <- predict(fit,prd,type = "response",se.fit=TRUE)$se.fit
    
    plot(0,0,col=NA,xlim=c(2011.5,2021.5),ylim=c(ymin[pp],ymax[pp]),
         main='',xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",xlab='',ylab='',frame=FALSE)
  
    legend(2012, 53, legend=c('','',''), horiz=TRUE,border=NA,
           col=colList, cex =cexList, bty='n',pch=pchList,x.intersp=2.3)
    legend(2011.9, 51, legend=urbList, horiz=TRUE,border=NA,
           col=colList, cex =0.8, bty='n',pch=NA,x.intersp=1.7)
           
    pPlot<- paste0('= ',format(round(p,digits = 2),nsmall = 2))
    text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
         par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.22),cex=cex_note,pos=4,
         bquote(italic("U: P")~"= 0.006"))
    text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
         par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.15),cex=cex_note,pos=4,
         bquote(italic("Yr: P")~"= 0.001"))
    text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
         par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.08),cex=cex_note,pos=4,
         bquote(italic("U x Yr")~~~"N.S."))
    for(uu in 1:3){
    #uu<- 1
    urb <- urbList[uu]  
    #plotData <- data%>%filter(urbanLevel==urb & year<=2021)%>%group_by(year)%>%
    #    summarise(mean=mean(durationBetaMode,na.rm=TRUE),
    #              SD=sd(durationHalf,na.rm=TRUE)) 
    plotData <- input[which(input$urbanLevel==urb),]
    
    points(plotData$year,plotData$n,type='b',pch=pchList[uu],lty=3,col=adjustcolor(colList[uu],alpha.f = 0.8),cex=cexList[uu])
    #arrows(plotData$year,plotData$mean-plotData$SE,plotData$year,plotData$mean+plotData$SE,col = adjustcolor(colList[uu],,alpha.f = 1),length=0.03, angle=90, code=3,lwd=0.8)
      
    plotData_prd <- prd[which(prd$urbanLevel==urb),]
      
    lines(plotData_prd$year,plotData_prd$pred,col=adjustcolor(colList[uu],alpha.f = 1),lty=1,lwd=1.5)
    #lines(plotData_prd$year,plotData_prd$pred+plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1.5)
    #lines(plotData_prd$year,plotData_prd$pred-plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1.5)
    }
    }

  if(yy=='blD'){
  input <- data%>%filter(year<=2021)%>%group_by(year,urbanLevel)%>%
      summarise(mean=mean(durationBetaMode,na.rm=TRUE),
                SD=sd(durationBetaMode,na.rm=TRUE)) 
  input_count <- data%>%filter(year<=2021)%>%group_by(year,urbanLevel)%>%tally()
  input <- merge(input,input_count)
  input$SE <- input$SD/sqrt(input$n)
  
  
  fit <- lm(durationBetaMode~year+urbanLevel,data=data) 
    table<-summary(fit)$coe
    p<-table[2,4]
    
    xx <- seq(2012,2021,1)
    prd <- data.frame(urbanLevel=rep(c('HDC','LDC','RUR'),each=length(xx)),year=rep(xx,3))
    prd$pred <- predict(fit,prd,type = "response",se.fit=TRUE)$fit
    prd$se <- predict(fit,prd,type = "response",se.fit=TRUE)$se.fit
  
    
  plot(0,0,col=NA,xlim=c(2011.5,2021.5),ylim=c(ymin[pp],ymax[pp]),
       main='',xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",xlab='',ylab='',frame=FALSE)
  
  text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
       par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.22),cex=cex_note,pos=4,
       bquote(italic("U: P")~"< 0.001"))
  text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
       par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.15),cex=cex_note,pos=4,
       bquote(italic("Yr: P")~"< 0.001"))
  text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
       par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.08),cex=cex_note,pos=4,
       bquote(italic("U x Yr")~~~"N.S."))
  
  for(uu in 1:3){
    #uu<- 1
    urb <- urbList[uu]  
    #plotData <- data%>%filter(urbanLevel==urb & year<=2021)%>%group_by(year)%>%
    #    summarise(mean=mean(durationBetaMode,na.rm=TRUE),
    #              SD=sd(durationHalf,na.rm=TRUE)) 
    plotData <- input[which(input$urbanLevel==urb),]
    
    points(plotData$year,plotData$mean,type = 'b',lty=3,pch=pchList[uu],col=adjustcolor(colList[uu],alpha.f = 0.8),cex=cexList[uu])
    arrows(plotData$year,plotData$mean-plotData$SE,plotData$year,plotData$mean+plotData$SE,col = adjustcolor(colList[uu],alpha.f = 1),length=0.03, angle=90, code=3,lwd=0.8)
    
    plotData_prd <- prd[which(prd$urbanLevel==urb),]
    
    lines(plotData_prd$year,plotData_prd$pred,col=adjustcolor(colList[uu],alpha.f = 1),lty=1,lwd=1.5)
    #lines(plotData_prd$year,plotData_prd$pred+plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1)
    #lines(plotData_prd$year,plotData_prd$pred-plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1)
  } 
  }
  
  #axis
  if(TRUE){
  axis(1,at=seq(2012,2021,1),labels=FALSE,tck=wth_xtck)
  axis(1,at=seq(2012,2021,1),labels=seq(2012,2021,1),lwd=0,line=ln_xtck,cex.axis=cex_tck)
  
  if(pp==3){mtext('Year', side=1, line=ln_xlbl, cex=cex_lbl)}
  
  axis(2,at=seq(ymin[pp],ymax[pp],ytck[pp]),labels=FALSE,tck=wth_ytck)
  axis(2,at=seq(ymin[pp],ymax[pp],ymrk[pp]),labels=seq(ymin[pp],ymax[pp],ymrk[pp]),lwd=0,line=ln_ytck-0.3,cex.axis=cex_tck)
  mtext(ylblList[pp], side=2, line=ln_ylbl, cex=cex_lbl) 
  mtext(letters[pp],3,line=0.5, adj=-0.15,cex=cex_pnl,font=4)
  mtext(ypnlList[pp],3,line=0.5, adj=0.5,cex=cex_pnl,font=4)
  
  #pPlot<- paste0('= ',format(round(p,digits = 2),nsmall = 2))
  #if(pPlot=='= 0.00'){pPlot<-'< 0.01'}
  #mtext(bquote(italic("P")~.(pPlot)),3,line=-1.5,adj=1,font=1,cex=cex_note,col=col_abline)
  
  #text((par("usr")[1]-par("usr")[2]-par("usr")[1])*0.17,par("usr")[3]+(par("usr")[4]-par("usr")[3])*1.05,
  }
  }
  
dev.off()
}  

#2015
data_plot<-data[which(data$year==2015),]
agg2015 <- data%>%group_by(sid,storm)%>%
  summarise(mean=mean(durationBetaMode),
            sd=sd(durationBetaMode))

agg2015_count <- data[which(data$year==2015),]%>%group_by(sid,storm)%>%tally()
agg2015 <- merge(agg2015,agg2015_count)

(69+143)/sum(agg2015$n)


#2020
data$durationHalf <- (data$duration_lwr+data$duration_upr)/2


if(TRUE){
  tiff(file = paste0(wd,'/2020.tiff'), width = 89, height = 170, units = 'mm', res=300) 
  par(oma=c(1,1,1,1),mar=c(2,2,1,0.5),xpd=TRUE,bg='NA',lend=1) 
  layout(matrix(seq(1,3,1), 3, 1, byrow = FALSE))
  pp<-3
  
  #mode
  if(TRUE){
 
  input <- data%>%filter(year<=2021)%>%group_by(year,urbanLevel)%>%
    summarise(mean=mean(durationBetaMode,na.rm=TRUE),
              SD=sd(durationBetaMode,na.rm=TRUE)) 
  input_count <- data%>%filter(year<=2021)%>%group_by(year,urbanLevel)%>%tally()
  input <- merge(input,input_count)
  input$SE <- input$SD/sqrt(input$n)
  
  
  fit <- lm(durationBetaMode~year+urbanLevel,data=data) 
  table<-summary(fit)$coe
  p<-table[2,4]
  
  xx <- seq(2012,2021,1)
  prd <- data.frame(urbanLevel=rep(c('HDC','LDC','RUR'),each=length(xx)),year=rep(xx,3))
  prd$pred <- predict(fit,prd,type = "response",se.fit=TRUE)$fit
  prd$se <- predict(fit,prd,type = "response",se.fit=TRUE)$se.fit
  
  
  plot(0,0,col=NA,xlim=c(2011.5,2021.5),ylim=c(3,10),
       main='',xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",xlab='',ylab='',frame=FALSE)
  
#  text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
#      par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.22),cex=cex_note,pos=4,
#     bquote(italic("U: P")~"< 0.001"))
#  text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
#      par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.15),cex=cex_note,pos=4,
#     bquote(italic("Yr: P")~"< 0.001"))
#text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
#    par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.08),cex=cex_note,pos=4,
#     bquote(italic("U x Yr")~~~"N.S."))
  
  for(uu in 1:3){
    #uu<- 1
    urb <- urbList[uu]  
    #plotData <- data%>%filter(urbanLevel==urb & year<=2021)%>%group_by(year)%>%
    #    summarise(mean=mean(durationBetaMode,na.rm=TRUE),
    #              SD=sd(durationHalf,na.rm=TRUE)) 
    plotData <- input[which(input$urbanLevel==urb),]
    
    points(plotData$year,plotData$mean,type = 'b',lty=3,pch=pchList[uu],col=adjustcolor(colList[uu],alpha.f = 0.8),cex=cexList[uu])
    arrows(plotData$year,plotData$mean-plotData$SE,plotData$year,plotData$mean+plotData$SE,col = adjustcolor(colList[uu],alpha.f = 1),length=0.03, angle=90, code=3,lwd=0.8)
    
    plotData_prd <- prd[which(prd$urbanLevel==urb),]
    
#    lines(plotData_prd$year,plotData_prd$pred,col=adjustcolor(colList[uu],alpha.f = 1),lty=1,lwd=1.5)
    #lines(plotData_prd$year,plotData_prd$pred+plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1)
    #lines(plotData_prd$year,plotData_prd$pred-plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1)
  } 
}

  #axis
  if(TRUE){
    axis(1,at=seq(2012,2021,1),labels=FALSE,tck=wth_xtck)
    axis(1,at=seq(2012,2021,1),labels=seq(2012,2021,1),lwd=0,line=ln_xtck,cex.axis=cex_tck)
    
    mtext('Year', side=1, line=ln_xlbl, cex=cex_lbl)
    
    axis(2,at=seq(3,10,1),labels=FALSE,tck=wth_ytck)
    axis(2,at=seq(3,10,1),labels=seq(3,10,1),lwd=0,line=ln_ytck-0.3,cex.axis=cex_tck)
    mtext(ylblList[pp], side=2, line=ln_ylbl, cex=cex_lbl+0.2) 
   # mtext(letters[pp],3,line=0.5, adj=-0.15,cex=cex_pnl,font=4)
    #mtext(ypnlList[pp],3,line=0.5, adj=0.5,cex=cex_pnl,font=4)
    
    #pPlot<- paste0('= ',format(round(p,digits = 2),nsmall = 2))
    #if(pPlot=='= 0.00'){pPlot<-'< 0.01'}
    #mtext(bquote(italic("P")~.(pPlot)),3,line=-1.5,adj=1,font=1,cex=cex_note,col=col_abline)
    
    #text((par("usr")[1]-par("usr")[2]-par("usr")[1])*0.17,par("usr")[3]+(par("usr")[4]-par("usr")[3])*1.05,
  }
  
  #half
  if(TRUE){
    input <- data%>%filter(year<=2021)%>%group_by(year,urbanLevel)%>%
      summarise(mean=mean(durationHalf,na.rm=TRUE),
                lwr=mean(duration_lwr,na.rm=TRUE),
                upr=mean(duration_upr,na.rm=TRUE)) 
    input_count <- data%>%filter(year<=2021)%>%group_by(year,urbanLevel)%>%tally()
    input <- merge(input,input_count)
   
    fit <- lm(durationHalf~year*urbanLevel,data=data) 
    table<-summary(fit)$coe
    p<-table[2,4]
    
    xx <- seq(2012,2021,1)
    prd <- data.frame(urbanLevel=rep(c('HDC','LDC','RUR'),each=length(xx)),year=rep(xx,3))
    prd$pred <- predict(fit,prd,type = "response",se.fit=TRUE)$fit
    prd$se <- predict(fit,prd,type = "response",se.fit=TRUE)$se.fit
    
    
    plot(0,0,col=NA,xlim=c(2011.5,2021.5),ylim=c(3,10),
         main='',xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",xlab='',ylab='',frame=FALSE)
    
#    text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
#        par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.22),cex=cex_note,pos=4,
#       bquote(italic("U: P")~"= 0.03"))
#      text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
#          par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.15),cex=cex_note,pos=4,
#         bquote(italic("Yr: P")~"= 0.01"))
#    text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
#        par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.08),cex=cex_note,pos=4,
#       bquote(italic("U x Yr")~~~"P = 0.04"))
    
    for(uu in 1:3){
      #uu<- 1
      urb <- urbList[uu]  
      #plotData <- data%>%filter(urbanLevel==urb & year<=2021)%>%group_by(year)%>%
      #    summarise(mean=mean(durationBetaMode,na.rm=TRUE),
      #              SD=sd(durationHalf,na.rm=TRUE)) 
      plotData <- input[which(input$urbanLevel==urb),]
      
      points(plotData$year,plotData$mean,type = 'b',lty=3,pch=pchList[uu],col=adjustcolor(colList[uu],alpha.f = 0.8),cex=cexList[uu])
      arrows(plotData$year,plotData$lwr,plotData$year,plotData$upr,col = adjustcolor(colList[uu],alpha.f = 1),length=0.03, angle=90, code=3,lwd=0.8)
      
      plotData_prd <- prd[which(prd$urbanLevel==urb),]
      
      #lines(plotData_prd$year,plotData_prd$pred,col=adjustcolor(colList[uu],alpha.f = 1),lty=1,lwd=1.5)
      #lines(plotData_prd$year,plotData_prd$pred+plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1)
      #lines(plotData_prd$year,plotData_prd$pred-plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1)
    } 
  }
  
  #axis
  if(TRUE){
    axis(1,at=seq(2012,2021,1),labels=FALSE,tck=wth_xtck)
    axis(1,at=seq(2012,2021,1),labels=seq(2012,2021,1),lwd=0,line=ln_xtck,cex.axis=cex_tck)
    
    mtext('Year', side=1, line=ln_xlbl, cex=cex_lbl)
    
    axis(2,at=seq(3,10,1),labels=FALSE,tck=wth_ytck)
    axis(2,at=seq(3,10,1),labels=seq(3,10,1),lwd=0,line=ln_ytck-0.3,cex.axis=cex_tck)
    mtext(ylblList[pp], side=2, line=ln_ylbl, cex=cex_lbl+0.2) 
  #  mtext(letters[pp],3,line=0.5, adj=-0.15,cex=cex_pnl,font=4)
   # mtext(ypnlList[pp],3,line=0.5, adj=0.5,cex=cex_pnl,font=4)
    
    #pPlot<- paste0('= ',format(round(p,digits = 2),nsmall = 2))
    #if(pPlot=='= 0.00'){pPlot<-'< 0.01'}
    #mtext(bquote(italic("P")~.(pPlot)),3,line=-1.5,adj=1,font=1,cex=cex_note,col=col_abline)
    
    #text((par("usr")[1]-par("usr")[2]-par("usr")[1])*0.17,par("usr")[3]+(par("usr")[4]-par("usr")[3])*1.05,
  }
  
  
  #half two group
  if(TRUE){
    data$grp <- 3
    data[which(data$urbanLevel%in%c('HDC','LDC')),'grp']<-1
    input <- data%>%filter(year<=2021)%>%group_by(year,grp)%>%
      summarise(mean=mean(durationHalf,na.rm=TRUE),
                lwr=mean(duration_lwr,na.rm=TRUE),
                upr=mean(duration_upr,na.rm=TRUE)) 
    input_count <- data%>%filter(year<=2021)%>%group_by(year,urbanLevel)%>%tally()
    input <- merge(input,input_count)
    
    fit <- lm(durationHalf~year*grp,data=data) 
    table<-summary(fit)$coe
    p<-table[2,4]
    
    xx <- seq(2012,2021,1)
    prd <- data.frame(grp=rep(c(1,3),each=length(xx)),year=rep(xx,2))
    prd$pred <- predict(fit,prd,type = "response",se.fit=TRUE)$fit
    prd$se <- predict(fit,prd,type = "response",se.fit=TRUE)$se.fit
    
    
    plot(0,0,col=NA,xlim=c(2011.5,2021.5),ylim=c(3,10),
         main='',xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",xlab='',ylab='',frame=FALSE)
    
    #text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
     #    par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.22),cex=cex_note,pos=4,
      #   bquote(italic("U: P")~"= 0.03"))
    #text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
     #    par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.15),cex=cex_note,pos=4,
      #   bquote(italic("Yr: P")~"= 0.01"))
    #text(par("usr")[1]+(par("usr")[2]-par("usr")[1])*0.75,
     #    par("usr")[3]+(par("usr")[4]-par("usr")[3])*c(0.08),cex=cex_note,pos=4,
      #   bquote(italic("U x Yr")~~~"P = 0.04"))
    
    for(uu in c(1:3)){
      #uu<- 1
      urb <- uu
      #plotData <- data%>%filter(urbanLevel==urb & year<=2021)%>%group_by(year)%>%
      #    summarise(mean=mean(durationBetaMode,na.rm=TRUE),
      #              SD=sd(durationHalf,na.rm=TRUE)) 
      plotData <- input[which(input$grp==urb),]
      
      points(plotData$year,plotData$mean,type = 'b',lty=3,pch=pchList[uu],col=adjustcolor(colList[uu],alpha.f = 0.8),cex=cexList[uu])
      arrows(plotData$year,plotData$lwr,plotData$year,plotData$upr,col = adjustcolor(colList[uu],alpha.f = 1),length=0.03, angle=90, code=3,lwd=0.8)
      
      plotData_prd <- prd[which(prd$urbanLevel==urb),]
      
      #lines(plotData_prd$year,plotData_prd$pred,col=adjustcolor(colList[uu],alpha.f = 1),lty=1,lwd=1.5)
      #lines(plotData_prd$year,plotData_prd$pred+plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1)
      #lines(plotData_prd$year,plotData_prd$pred-plotData_prd$se,col=adjustcolor(colList[uu],alpha.f = 1),lty=2,lwd=1)
    } 
  }
  
  #axis
  if(TRUE){
    axis(1,at=seq(2012,2021,1),labels=FALSE,tck=wth_xtck)
    axis(1,at=seq(2012,2021,1),labels=seq(2012,2021,1),lwd=0,line=ln_xtck,cex.axis=cex_tck)
    
    mtext('Year', side=1, line=ln_xlbl, cex=cex_lbl)
    
    axis(2,at=seq(3,10,1),labels=FALSE,tck=wth_ytck)
    axis(2,at=seq(3,10,1),labels=seq(3,10,1),lwd=0,line=ln_ytck-0.3,cex.axis=cex_tck)
    mtext(ylblList[pp], side=2, line=ln_ylbl, cex=cex_lbl+0.2) 
    #  mtext(letters[pp],3,line=0.5, adj=-0.15,cex=cex_pnl,font=4)
    # mtext(ypnlList[pp],3,line=0.5, adj=0.5,cex=cex_pnl,font=4)
    
    #pPlot<- paste0('= ',format(round(p,digits = 2),nsmall = 2))
    #if(pPlot=='= 0.00'){pPlot<-'< 0.01'}
    #mtext(bquote(italic("P")~.(pPlot)),3,line=-1.5,adj=1,font=1,cex=cex_note,col=col_abline)
    
    #text((par("usr")[1]-par("usr")[2]-par("usr")[1])*0.17,par("usr")[3]+(par("usr")[4]-par("usr")[3])*1.05,
  }
  
dev.off()  
}
 
 


data2020<-data[which(data$year==2020),]
agg2020 <- data2020%>%
           group_by(sid,storm,country)%>%
           #group_by(year,urbanLevel)%>%
           summarise(meanBeta=mean(durationBetaMode),
            SD=sd(durationBetaMode,na.rm=TRUE),
            meanHalf=mean(durationHalf),
            lwr=mean(duration_lwr),
            upr=mean(duration_upr)
            )

agg2020_count <- data2020%>%group_by(sid,storm)%>%tally()
agg2020 <- merge(agg2020,agg2020_count)
agg2020$SE <- agg2020$SD/sqrt(agg2020$n)

(69+143)/sum(agg2015$n)