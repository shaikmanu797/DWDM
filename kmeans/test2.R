######Assignment 5 by Mansoor Baba Shaik
euclidDist<-function(XY,A,K){
  ecd=vector("list")
  euclidMatrix=vector("list")
  for(i in 1:K){
    for(j in 1:nrow(XY)){
      sqDist<-sum((XY[j,1]-A[i,1])^2,(XY[j,2]-A[i,2])^2)
      ecd[[j]]<-round(sqrt(sqDist),digits=3)
    }
    euclid=do.call(rbind,ecd)
    euclidMatrix[[i]]<-euclid
  }
  result=do.call(cbind,euclidMatrix)
  return(result)
}

assignClusterToPoints <- function(XY,B,K){
  clusterAssign=vector("list")
  meanDistance = euclidDist(XY,B,K)
  for(n in 1:nrow(meanDistance)){
    clusterAssign[[n]]<-which.min(meanDistance[n,])
  }
  clusterNums=do.call(rbind,clusterAssign)
  return(clusterNums)
}

calcNewMean<-function(XY,C,K){
  newMean=vector("list")
  for(m in 1:K){
    a<-which(C[,1]==m)
    newMeanX=0
    newMeanY=0
    for(n in 1:length(a)){
      index<-a[n]
      newMeanX<-newMeanX+XY[index,1]
      newMeanY<-newMeanY+XY[index,2]
    }
    nmX<-round(newMeanX/length(a),digits=3)
    nmY<-round(newMeanY/length(a),digits=3)
    newMean[[m]]<-cbind(nmX,nmY)
  }
  meansNew=do.call(rbind,newMean)
  return(meansNew)
}

k_means<-function(XY,means,K,maxItter){
  initClust=assignClusterToPoints(XY,means,K)
  #print(initClust)
  r<-cbind(XY,initClust)
  colnames(r)<-c("X","Y","Cluster")
  initResult<-as.matrix(r)
  #print(initResult)

  result=matrix(NA,ncol=(maxItter-1),nrow=13)
  for(p in 2:maxItter){
    if(p==2){
      new=calcNewMean(XY,initClust,K)
      
    }
    else
    {
      new=calcNewMean(XY,newClust,K)

    }
      newClust<-assignClusterToPoints(XY,new,K)
      m<-p-1
      result[,m]<-newClust
  }
    
  finalz=cbind(XY,initClust,result)
  colnames(finalz)<-c("X","Y",rep("Cluster",maxItter))
  finalz=as.data.frame(finalz)
  return(finalz)
}


#############################################
###Main Program

Xcoord <- c(1,3,1,1,4,5,7,8,9,4,5,1,2)
Ycoord <- c(2,4,5,9,6,5,7,9,7,5,6,1,1)
K <- 3
N<-5
data<-data.frame(Xcoord,Ycoord)
XY<-as.matrix(data)
means<-XY[sample(XY,size=3,replace=FALSE),]
cat("\n")
jpeg("0_initial_nocluster.jpg")
plot(Xcoord, Ycoord)
dev.off()

final=k_means(XY,means,K,N)

for(t in 2:N)
{
  prev=t+1
  curr=t+2
  if(identical(final[,prev],final[,curr])==TRUE){
    cat("Algorithm Converged at",t,"iteration")
    cat("\n")
    disp=t+2
    break
    
  }
}
cat("\n")

results<-as.matrix(final[,1:disp])
colnames(results)<-c("X","Y",rep("Cluster",disp-2))

for(pt in 3:disp){
  jpeg(paste(pt-2,".jpg"))
  plot(XY[,1],XY[,2],col=final[,pt])
  dev.off()
}


print(results)