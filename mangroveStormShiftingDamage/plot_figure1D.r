#####requirements#####
#R version 4.4.2
######################
rm(list = ls())
#library(dplyr)
wd <- paste0("C:/YM/CHES/TCMangroveModel")
    
#read data 
if(TRUE){
data <- read.csv(paste0(wd,'/result/model/output_ml_variableImportance.csv'),stringsAsFactors = FALSE)
      
data$group <- NA
for (row in 1:nrow(data)){
if(data[row,'var']%in%c("windSpeed","windSpeed5day","travelSpeed","rainfall")){data[row,'group']<-'wind'}  
if(data[row,'var']%in%c("coastalSlope","coastDist")){data[row,'group']<-'coast'} 
}
  
varList <-c("windSpeed","windSpeed5day","travelSpeed","rainfall",
            "coastalSlope","coastDist")
lblList <- c('Wind speed','5-d wind speed','Travel speed', 'Rainfall',  
            'Coastal slope', 'Dist. to shore')

data_plot <- data                
for(vv in 1:length(varList)){
data_plot[which(data_plot$var==varList[vv]),'lbl'] <- lblList[vv]
} 
}
    
#color 
if(TRUE){
wind_col <- rgb(157,55,126,maxColorValue = 255)
coast_col <- rgb(0,103,179, maxColorValue = 255)

data_plot$col <- NA
data_plot[which(data_plot$group=='wind'),'col'] <- wind_col
data_plot[which(data_plot$group=='coast'),'col'] <-  coast_col
}
    
#format 
if(TRUE){
cex_lbl <- 0.65   
cex_mrk <- 0.6
cex_pnl <- 0.7

ln_xlbl <- 0.45
ln_xmrk <- -1.1

wth_xtck <- -0.02
}

#plot
if(TRUE){
      #pdf(file = paste0(wd,'/result/figure/','figure1d-6var.pdf'),width = 2.3622,height = 1.61417)   
      tiff(file = paste0(wd,'/result/figure/','figure1d.tif'), width =60, height = 41, units = 'mm', res=300) 
      layout(matrix(c(1),1,1, byrow = TRUE),heights=c(1),widths=c(1))
      par(oma =c(0,0,0,0),mar =c(1.5,4.05,0.62,0.5), lend=2,xpd=TRUE,bty="n",bg=NA,lwd=0.7) 
      
      data_plot2 <- data_plot %>% 
                   filter(mth%in%c('rpart','gbm','xgboost')) %>%
                   group_by(var,group,col,lbl) %>%
                   summarise(mean=mean(imp,na.rm=TRUE),
                             sd=sd(imp,na.rm=TRUE),
                             se=sd(imp,na.rm=TRUE)/sqrt(4))  
      
      data_plot2 <- data_plot2[order(-data_plot2$mean),]
      br <- barplot(data_plot2$mean,beside=TRUE,horiz = TRUE,
                    col=adjustcolor(data_plot2$col,alpha.f = 0.8),
                    border = 'white',
                    xlim=c(0,100),ylim=c(7.5,0.2),width = rep(1,6),#space = 0.2,
                    xaxs = 'i', yaxs = 'i', yaxt="n",main='',
                    xaxt="n", xlab="", ylab="")
      
      arrows(data_plot2$mean-data_plot2$se,br,data_plot2$mean+data_plot2$se,br,angle = 90,code=3,length = 0.03)
      
      text(rep(-50,5),br,data_plot2$lbl,adj=0,cex=cex_mrk)
      axis(1,at=seq(0,100,50),labels=FALSE,tck=wth_xtck,pos=7.5)
      axis(1,at=seq(0,100,50),labels=seq(0,100,50),
           lwd=0,line=ln_xmrk,cex.axis=cex_mrk)
      mtext('Variable importance', side=1, line=ln_xlbl, cex=cex_lbl)
      mtext('d',3,line=-0.1,adj=-0.58,cex=cex_pnl,font=2)
      
      dev.off()
    }



