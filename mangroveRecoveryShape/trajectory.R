#For guidance, Nature's standard figure sizes are 89 mm wide (single column) and 183 mm wide (double column). 
#The full depth of a Nature page is 247 mm. Figures can also be a column-and-a-half where necessary (120-136 mm).

  rm(list = ls())
  library(dplyr)
  
  wd <- paste0("C:/YM/CHES/TCMangroveRecovery/")
  
  dataAll <- read.csv(paste0(wd,'/gitHub/trajectory.csv'),stringsAsFactors = FALSE)

  #color 
  if(TRUE){
     col_lg <- rgb(238,119,51,maxColorValue = 255)
     col_ln <- rgb(0,119,187,maxColorValue = 255)
     col_xp <- rgb(238,51,119,maxColorValue = 255)
     col_lgt <- rgb(68,170,153,maxColorValue = 255)
     
     data_curve <- data.frame(lvl=c('lm','lg','xp','lgt'),txt=c('Linear','Logarithmic','Exponential','Logistic'),col=c(col_ln,col_lg,col_xp,col_lgt))  
     data_curve$order <- nrow(data_curve):1  
     
     
     data_N <- data.frame(lvl=c('Nall','N3','event','damage'),txt=c('Storm recorded', 'Damaing storm','Damageing event','Storm damage'),
                          col=c('purple','purple4','yellow2','orange'))  
     data_N$order <- nrow(data_curve):1  
  }
  
  
  #plot
  regList <- c("Africa","America","Asia", "Oceania")
  shpList <- c('lm','lg','xp','lgt')
  nList <- c('Nall','N3','event','damage')
  typList <- c("Coast","Delta","Estuary","Lagoon")
  
  #format 
  if(TRUE){
    cex_pnl <- 0.7
    cex_lbl <- 0.6   
    cex_mrk <- 0.6
    cex_lgd <- 0.6
    cex_note <- 0.7
    
    wdth_tck_x <- -0.015
    line_mrk_x <- -1
    line_lbl_x <- 0.6
    
    wdth_tck_y <- -0.02
    line_mrk_y <- -0.7
    line_lbl_y <- 1

    lmt_x <- c(0,1)
    lmt_y <- c(0,1)
    
  }
 
  #regional bar 
  if(TRUE){    
    txtYadj01<-matrix(c(-0.05,-0.05,-0.05,-0.05,
                        0,0,0,0,
                        0,0,0,0,
                        0,0,0,0),nrow=4,byrow = TRUE)
    
    for(rgN in 1:length(regList)){
      #rgN <- 1
      
      #prepare data
      if(TRUE){
        rg <- regList[rgN]
        print(rg)
        input <- dataAll[which(dataAll$AICDiff<=-2 & dataAll$bestAIC_P<=0.05),]
        if(rg!='Global'){input <- input[which(input$continent==rg),]}
        
         
        agg <- input%>%group_by(bestAIC)%>%tally()
        agg <- merge(agg,data_curve,by.x='bestAIC',by.y='lvl')  
        agg <- agg[order(agg$order),]
        agg$prct <- agg$n/sum(agg$n)
        }
      
      #plot
      if(TRUE){
        tiff(file = paste0(wd,'gitHub/figure/fig4CurveTypeBarCS_',rg,'.tif'), width =30, height = 23, units = 'mm', res=300) 
         
        layout(matrix(c(1),1,1, byrow = TRUE),widths=c(1),heights = c(1))
        par(oma =c(0,0,0,0),mar =c(0.5,0.5,0.5,0.5),xpd=TRUE,bty="n",bg=NA,lwd=2) 
        
        
        bp <- barplot(height = as.matrix(agg$prct), beside = FALSE,space=0.7,
                      col= adjustcolor(agg$col,alpha.f = 1),border=c(agg$col),lwd=0.1,
                      ylim=c(0,1.02),xlim=c(0,2.4),xaxs = 'i', yaxs = 'i', yaxt="n",main='', xaxt="n", xlab="", ylab="")
        
        mtext(bquote(.(rg)), side=3, adj=0.5,line=-0.1, cex=cex_note-0.2,font = 2)
        
        
        for(rr in 1:4){
          #rr<-2
          text(par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.32, sum(agg$prct[1:(rr-1)])*(rr>1)+0.5*agg$prct[rr]+txtYadj01[rr,rgN],paste0(round(agg$prct[rr],digits = 2)*100,'%'),cex=cex_note-0.2,pos=2)  
         }
        
        dev.off()
      }
    }#loop region
  }
   
  #type bar 
  if(TRUE){    
    txtYadj01<-matrix(c(-0.05,-0.05,-0.05,-0.05,
                        0,0,0,0,
                        0,0,0,0,
                        0,0,0,0),nrow=4,byrow = TRUE)
    
    for(typN in 1:length(typList)){
      #typN  <- 1
      
      #prepare data
      if(TRUE){
        typ <- typList[typN]
        print(typ)
        input <- dataAll[which(dataAll$AICDiff<=-2 & dataAll$bestAIC_P<=0.05 & dataAll$typology==typ),]
       
        agg <- input%>%group_by(bestAIC)%>%tally()
        agg <- merge(agg,data_curve,by.x='bestAIC',by.y='lvl')  
        agg <- agg[order(agg$order),]
        agg$prct <- agg$n/sum(agg$n)
      }
      
      #plot
      if(TRUE){
        tiff(file = paste0(wd,'gitHub/figure/fig4CurveTypeBarCS_',typ,'.tif'), width =31, height = 24, units = 'mm', res=300)
            
        layout(matrix(c(1),1,1, byrow = TRUE),widths=c(1),heights = c(1))
        par(oma =c(0,0,0,0),mar =c(0.5,1,0.5,0),xpd=TRUE,bty="n",bg=NA,lwd=2) 
        
        
        bp <- barplot(height = as.matrix(agg$prct), beside = FALSE,space=0.7,
                      col= adjustcolor(agg$col,alpha.f = 1),border=c(agg$col),lwd=0.1,
                      ylim=c(0,1.02),xlim=c(0,2.4),xaxs = 'i', yaxs = 'i', yaxt="n",main='', xaxt="n", xlab="", ylab="")
        
        mtext(bquote(.(typ)), side=3, adj=-0.2,line=-0.1, cex=cex_note-0.2,font = 2)

        for(rr in 1:4){
          #rr<-2
          text(par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.32, sum(agg$prct[1:(rr-1)])*(rr>1)+0.5*agg$prct[rr]+txtYadj01[rr,typN],paste0(round(agg$prct[rr],digits = 2)*100,'%'),cex=cex_note-0.2,pos=2)  
         }
        
        dev.off()
      }
      
    }#loop type
  }
  
  #Fig 4 by shape over time line
  if(TRUE){
    tiff(file = paste0(wd,'result_',mgData,'/figure/overTime_shape.tif'), width =183, height = 50, units = 'mm', res=400) 
    layout(matrix(c(1,2),1,2, byrow = TRUE),widths=c(1.8,1),heights = c(1))
    par(oma =c(0.5,1,1.5,0),mar =c(1.5,1.5,0,2),xpd=TRUE,bty="n",bg=NA) 
    
     #get data by year
    if(TRUE){
      inputAll <- dataAll
      input <- dataAll[which(dataAll$AICDiff<=-2 & dataAll$bestAIC_P<=0.05),]
      
      dataByYear <- data.frame(year=seq(2001,2021,1))
      for(rr in 1:nrow(dataByYear)){
        #rr <- 1
        dataByYear[rr,'Nall']<- nrow(inputAll[which(inputAll$year==dataByYear[rr,'year']),])
        dataByYear[rr,'N3']<- nrow(input[which(input$year==dataByYear[rr,'year']),])  
        
        for(shp in shpList){
          #shp <- 'lm'  
          dataByYear[rr,shp]<- nrow(input[which(input$bestAIC==shp & input$year==dataByYear[rr,'year']),])
        }
        
        }
   } 
      
    plot(0,0,xlim=c(2001,2021),ylim=c(0,100),xaxs = 'i', yaxs = 'i', xaxt="n", xlab="", ylab='',yaxt="n",frame = FALSE)
    
    slope_all <- data.frame()
    
    for(shpN in 1:length(shpList)){
    #shpN <-1  
    shp <- shpList[shpN]
    input <- dataByYear[,c('year',shp)]
    colnames(input) <- c('year','yy')
      
    fit <- glm(yy~year,data=input,family=poisson())   
    input$prd <-  predict(fit,input, type = "response",se.fit=TRUE)$fit
    input$lwr <-  predict(fit,input, type = "response",se.fit=TRUE)$fit-predict(fit,input, type = "response",se.fit=TRUE)$se.fit
    input$upr <-  predict(fit,input, type = "response",se.fit=TRUE)$fit+predict(fit,input, type = "response",se.fit=TRUE)$se.fit
    
    slope <- data.frame('lvl'=shp)
    slope$estimate<- summary(fit)$coefficients[2,1]
    slope$se<- summary(fit)$coefficients[2,2]
    slope$p<- summary(fit)$coefficients[2,4]
    slope$r2 <- summary(fit)$r.squared
  
    slope_all <- rbind(slope_all,slope)
    
    #plot
    if(TRUE){
      cl<-data_curve[which(data_curve$lvl==shp),'col'];lty <- 3;lwd <- 0.8;f <- 0.1
      if(slope$p<=0.05){lty<-1;lwd<-1.5;f <- 0.6}
      
      polygon(c(input$year,rev(input$year)),c(input$upr,rev(input$lwr)),border = NA,col = adjustcolor(cl,alpha.f = f))
      lines(input$year,input$prd,col=cl,lwd=lwd,lty=lty)
    }
    
    }
    
    #axis
    if(TRUE){
      axis(1,at=seq(2001,2021,1),labels=FALSE,tck=wdth_tck_x)
      axis(1,at=seq(2001,2021,5),labels=seq(2001,2021,5),lwd=0,line=line_mrk_x,cex.axis=cex_mrk)
      mtext('Year', side=1, line=line_lbl_x, cex=cex_lbl)
      
      axis(2,at=seq(0,100,10),labels=FALSE,tck=wdth_tck_y,pos=2001)
      axis(2,at=seq(0,100,10),labels=seq(0,100,10),lwd=0,line=line_mrk_y,cex.axis=cex_mrk)
      mtext('Number of trajectories', side=2, line=line_lbl_y, cex=cex_lbl)
      }
   
    par(mar =c(1.5,2,0,0.9)) 
    #slope bar
    if(TRUE){
      data_plot2 <- merge(slope_all,data_curve)
      data_plot2$colFill <- data_plot2$col
      data_plot2[which(data_plot2$p>0.05),'colFill']<-'white'  
      data_plot2 <- data_plot2[order(-data_plot2$order),]
      
      bp <- barplot(data_plot2$estimate,horiz = FALSE,col=adjustcolor(data_plot2$colFill,1),border = adjustcolor(data_plot2$col,1),
                    space = 1,xlim=c(0.5,8.5),ylim=c(-0.02,0.08),#width = rank(data_plot$prec100),
                    xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n", xlab="", ylab="")#
      arrows(bp,data_plot2$estimate-data_plot2$se,bp,data_plot2$estimate+data_plot2$se,col='black',lwd=1,code=3,angle=90,length=0.03)
      arrows(par('usr')[1],0,par('usr')[2],0,col='grey40',lty=3,lwd=1,code=3,angle=90,length=0)
      
      #axis 
      if(TRUE){
        axis(1,at=bp,labels=c('Lin','Log','Exp','Lgt'),lwd=0,line=line_mrk_x+0.6,cex.axis=cex_mrk)
        
        axis(2,at=seq(-0.02,0.1,0.02),labels=FALSE,tck=wdth_tck_y)
        axis(2,at=seq(0,0.08,0.04),labels=seq(0,8,4),lwd=0,line=line_mrk_y,cex.axis=cex_mrk)
        mtext('Slope (%/yr)', side=2, line=line_lbl_y, cex=cex_lbl)
      }
    } 
   
    dev.off()
  }
  
 
  
 


  