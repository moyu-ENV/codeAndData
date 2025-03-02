rm(list = ls())

#Set up input
if(TRUE){
  wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangroveModel/") #local
  #wd <- paste0("C:/Users/cenv0794/Desktop/ym/CHES/TCMangroveModel") #server
  
#longley example 
if(FALSE){
sample.no<-10
variable.no<-6
input<-longley[1:sample.no,1:variable.no]
}

data <- read.csv(paste0(wd,'/result/model/input_fixedCNTRY_rmOutlier.csv'),stringsAsFactors = FALSE)
#data <- data[which(data$coastDist==5),]

allXs <- c('preEVI',"landingSpeed","landingWindMaxLocal2","totalPrec_total","bathymetry_mean","coastDist") #"elevation_mean",
input <- data[,c(allXs)]
input <- input[complete.cases(input),]

sample.no<-nrow(input)
variable.no<-length(allXs)

pc.keep.no <- variable.no
stoc.per<-35
}  

#standardize input
if(TRUE){
input.mean <- colMeans(input)  
input.sd <- apply(input,2,sd)
input.std <- input
for(cc in 1:ncol(input)){
#cc <-6  
input.std[,cc] <- input[,cc]-input.mean[cc]  
input.std[,cc] <- input.std[,cc]/input.sd[cc]  
}
#colMeans(input.std)
#apply(input.std,2,sd)  
}

#get PCA  
if(TRUE){
pca<-prcomp(input.std,retx=TRUE,center=FALSE,scale=FALSE) 
#summary(pca)

pc<-pca$x
#hist(pc[,1])
write.csv(pc,paste0(wd,'/result/model/stoc/pca.csv'),row.names = TRUE)
loading<-pca$rotation 
#write.csv(loading,paste0(wd,'result/model/stoc/pca-loading.csv'),row.names = TRUE)
reverse<-pc%*%t(loading)

reverse.rscl <- reverse
for(cc in 1:ncol(input)){
reverse.rscl[,cc] <- reverse[,cc]*input.sd[cc]  
reverse.rscl[,cc] <- reverse.rscl[,cc]+input.mean[cc]  
}
}
  
#perturb
if(TRUE){
SD <-2 #how varied every stochastic event from the seed event; larger, closer   #50 is too much
stochastic.no <- sample.no*stoc.per
pc.perturb<-matrix(0,stochastic.no,ncol(pc))
for(seed in 1:sample.no){
set.seed(1)
for(pcs in 1:pc.keep.no){
sd <- sd(pc[,pcs])
#set.seed(pcs)
pc.perturb[((seed-1)*stoc.per+1):(seed*stoc.per),pcs] <- rnorm(stoc.per,pc[seed,pcs],sd/SD) #perturb around each sample
}
}
  
reverse.perturb<-pc.perturb%*%t(loading)

#re-scale
reverse.perturb.rscl <- reverse.perturb
for(cc in 1:ncol(input)){
reverse.perturb.rscl[,cc] <- reverse.perturb[,cc]*input.sd[cc]  
reverse.perturb.rscl[,cc] <- reverse.perturb.rscl[,cc]+input.mean[cc]  
}
} 



#remove too small
for(var in allXs){
#var <-   "landingSpeed"
reverse.perturb.rscl <- reverse.perturb.rscl[which(reverse.perturb.rscl[,var] >= min(data[,var],na.rm = TRUE)),]  
}

#remove too big
for(var in allXs){
reverse.perturb.rscl <- reverse.perturb.rscl[which(reverse.perturb.rscl[,var] <= max(data[,var],na.rm = TRUE)),]  
}

#plot pca
if(FALSE){
plot(pc[,1],pc[,2])
arrows(0,0,loading[,1]*5,loading[,2]*5, col='red',lwd=3)
text(loading[,1]*8,loading[,2]*8,row.names(loading),col='red')
}

#plot to check 
if(FALSE){  
pdf(file = paste0(wd,'/result/figure/','stochasticEvents-10per.pdf'), width = 8.5, height = 11) 
par(oma=c(1,1,1,1),mar=c(5,5,1,1),xpd=TRUE) 
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = FALSE))  
  
for(xaxis in 1:(variable.no-1)){
for (col in 1:(variable.no-xaxis)){
plot(reverse.perturb.rscl[,xaxis],reverse.perturb.rscl[,(xaxis+col)],col="green",pch=19,
         xlim=c(min(input[,xaxis],reverse.rscl[,xaxis],reverse.perturb.rscl[,xaxis]),
                  max(input[,xaxis],reverse.rscl[,xaxis],reverse.perturb.rscl[,xaxis])),
           ylim=c(min(input[,(xaxis+col)],reverse.rscl[,(xaxis+col)],reverse.perturb.rscl[,(xaxis+col)]),
                  max(input[,(xaxis+col)],reverse.rscl[,(xaxis+col)],reverse.perturb.rscl[,(xaxis+col)])),
           main="Inputs",xlab=colnames(input)[xaxis],ylab=colnames(input)[xaxis+col])
points(reverse.rscl[,xaxis],reverse.rscl[,(xaxis+col)],col="blue",bg=NA,pch=1)
points(input[,xaxis],input[,(xaxis+col)],col="red",pch=4)}}
dev.off()
} 

write.csv(reverse.perturb.rscl,paste0(wd,'/result/model/stoc/stochasticEvents-35per.csv'),row.names = FALSE)  
   








