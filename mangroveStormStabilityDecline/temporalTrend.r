rm(list = ls())

data <- read.csv('/data/data.csv',stringsAsFactors = FALSE)

#set col
if(TRUE){
    cntList <- c("Global", 
                 "Africa",   
                 "Asia",
                 "America", 
                 "Oceania")
    pchList <- c(4,21,22,23,24)
    colList <- c('grey30',
                 rgb(221,204,119,maxColorValue = 255),
                 rgb(68, 170, 153,maxColorValue = 255),
                 rgb(204,102,119,maxColorValue = 255),
                 rgb(136, 204,238,maxColorValue = 255))
  
    lvl <- data.frame(order=c(1:5) ,region=cntList,pch=pchList,col=colList,colbdr=colList)
}


#plot format 
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
  
    #col_react <- rgb(244,124,32,maxColorValue = 255)
    #col_noReact<- rgb(180,36,121,maxColorValue = 255)
}

yys <- c('initialResistance','reactionRate','recoveryRate')
ylbs <- c('Inital resistance','Reaction rate (/month)','Recovery rate (/month)')
ylms <- list(c(0.7,0.91),c(-0.1,-0.02),c(0.06,0.21))
ytcks<- list(seq(0.7,0.91,0.03),seq(-0.1,-0.02,0.02),seq(0.06,0.21,0.03))
ymrks <- list(seq(0.7,0.91,0.03),seq(-0.1,-0.02,0.02),seq(0.06,0.21,0.03))

ylms2 <- list(c(-1,0.4),c(3,-4),c(-5,1))#c(-0.4,0.1),
ytcks2<- list(seq(-1,0.4,0.2),seq(3,-4,-1),seq(-5,1,1))#seq(-0.4,0.1,0.1),
ymrks2<- list(seq(-1,0.4,0.4),seq(3,-4,-3),seq(-5,1,2))#seq(-0.4,0.1,0.2),

tiff(file = '/results/Fig4_overTime.tif', width =150, height = 155, units = 'mm', res=300) 
    layout(matrix(seq(1,6,1),3,2, byrow = TRUE),widths=c(1.7,1),heights = c(1))
    par(oma =c(1,1,0.3,0.3),mar =c(2,6,2,1),xpd=TRUE,bty="n",bg=NA,lwd=1) 
  
p_all <- data.frame()

for (yy in c(1:3)){
    #lines
    if(TRUE){
        plot(0,0,xlim=c(2001,2021),ylim=ylms[[yy]],xaxs = 'i', yaxs = 'i', xaxt="n", xlab="", ylab='',yaxt="n",frame = FALSE)

        #axis 
        if(TRUE){
            axis(1,at=seq(2001,2021,1),labels=FALSE,tck=wth_tck_x)
            if(yy==3){axis(1,at=seq(2001,2021,5),labels=seq(2001,2021,5),lwd=0,line=ln_mrk_x,cex.axis=cex_mrk)
                      mtext('Year', side=1, line=ln_lbl_x, cex=cex_lbl)}
            axis(2,at=ytcks[[yy]],labels=FALSE,tck=wth_tck_y,pos=2001)
            axis(2,at=ymrks[[yy]],labels=ymrks[[yy]],lwd=0,line=ln_mrk_y,cex.axis=cex_mrk)
            mtext(ylbs[yy], side=2, line=ln_lbl_y, cex=cex_lbl)
            mtext(letters[1+(yy-1)*2], side=3, line=0.5,adj=-0.15, cex=cex_pnl,font=2)
        
            mtext(side=3,paste0('More\nstable'),adj=-0.25,line=-2,font=3,cex=cex_note,col='grey')
            mtext(side=3,paste0('Less\nstable'),adj=-0.25,line=-11,font=3,cex=cex_note,col='grey')
            arrows(1996,par('usr')[4],1996,par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.8,col='grey',lwd=1,code=1,angle=35,length=0.04)
            arrows(1996,par('usr')[3],1996,par('usr')[4]-(par('usr')[4]-par('usr')[3])*0.8,col='grey',lwd=1,code=1,angle=35,length=0.04)
        }
      
        slope_all <- data.frame()
         
        for(nmb in length(cntList):2){
            cnt <- cntList[nmb]
            #prepare data
            if(TRUE){
                input <-data[,c(yys[yy],'year')]
                if(cnt != 'Global'){input <- data[which(data$continent==cnt),c(yys[yy],'year')]}
          
                colnames(input)[1] <- c('y')
                input <- input[complete.cases(input[,'y']),]

                slope <- data.frame('yy'=yys[yy],'region' = cnt)
                data_plot <- data.frame(year=seq(2001,2021))
                  
                #log transform
                if(TRUE){
                input$y2 <- input$y
                if(min(input$y)<0){input$y2 <- -input$y}
                fit <- glm(log10(y2)~year,data=input)   
                slope <- data.frame('yy'=yys[yy],'region' = cnt)
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
           
                if(min(input$y)<0){
                    data_plot$prd <- -data_plot$prd
                    data_plot$lwr <- -data_plot$lwr
                    data_plot$upr <- -data_plot$upr
                }
           
                slope$estRlt <- (10^slope$estimate-1)*100
                slope$seRlt <- (10^slope$se-1)*100
          
                slope_all <- rbind(slope_all,slope)
                } 
            }
            
            #plot
            if(TRUE){
            cl<-colList[nmb];lty <- 3;lwd <- 0.8 
            if(slope$p<=0.05){cl<-colList[nmb];lty<-1;lwd<-1.5}
          
            polygon(c(data_plot$year,rev(data_plot$year)),c(data_plot$upr,rev(data_plot$lwr)),border = NA,col = adjustcolor(cl,alpha.f = 0.6))
            lines(data_plot$year,data_plot$prd,col=cl,lwd=lwd,lty=lty)
            }
        }
    }
    
    #slope bar
    if(TRUE){
        data_plot2 <- merge(slope_all,lvl)
        data_plot2[which(data_plot2$p>0.05),'col']<-'white'  
        data_plot2 <- data_plot2[order(data_plot2$order),]
       
        bp <- barplot(data_plot2$estRlt,horiz = FALSE,col=adjustcolor(data_plot2$col,1),border = adjustcolor(data_plot2$colbdr,1),
                    space = 0.7,xlim=c(0,7),ylim=ylms2[[yy]],#width = rank(data_plot$prec100),
                    xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n", xlab="", ylab="")#
        arrows(bp,data_plot2$estRlt-data_plot2$seRlt,bp,data_plot2$estRlt+data_plot2$seRlt,col='black',lwd=1,code=3,angle=90,length=0.03)
        arrows(par('usr')[1],0,par('usr')[2],0,col='grey40',lty=3,lwd=1,code=3,angle=90,length=0)
     
        #axis 
        if(TRUE){
            axis(2,at=ytcks2[[yy]],labels=FALSE,tck=wth_tck_y)
            axis(2,at=ymrks2[[yy]],labels=ymrks2[[yy]],lwd=0,line=ln_mrk_y,cex.axis=cex_mrk)
            mtext('Relative rate of change (%/year)', side=2, line=ln_lbl_y, cex=cex_lbl)
            mtext(letters[2+(yy-1)*2], side=3, line=0.5,adj=-0.33, cex=cex_pnl,font=2)

            if(yy==3){text(x = bp+0.5, y = par("usr")[3]-(par("usr")[4]-par("usr")[3])*0.05,labels = data_plot2$region, xpd = NA,srt = 45, cex = cex_mrk+0.1,pos=2)}
        }
    }
      
}
dev.off()

