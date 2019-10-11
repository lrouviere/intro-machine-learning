library(reshape2)
trace_front_gg <- function(donnees,model=NULL,grille=0.01,methode="logistic",k=1,points=FALSE){
  noms <- names(donnees)
  names(donnees)[1:2] <- c("X1","X2")
  px1 <- seq(min(donnees[,1]),max(donnees[,1]),by=grille)
  px2 <- seq(min(donnees[,2]),max(donnees[,2]),by=grille)
  dtest <- expand.grid(X1=px1,X2=px2)
  if (methode=="LDA"){
#    model <- lda(Y~.,data=donnees)
    prob <- as.numeric(predict(model,newdata=dtest)$class)
  }
#    mod <- lda(donnees,grouping=label)}
#  else {mod <- qda(donnees,grouping=label)}
  if (methode=="logistic"){
    prob <- as.numeric(round(predict(model,newdata=dtest,type="response")))
  }
  if (length(k)==0){k <- "all"}
  if (methode=="KNN"){
    if (k=="all"){
      prob <- rep(round(mean(as.numeric(donnees$Y)-1)),nrow(dtest))
#      prob <- 1
    } else {
      model <- knn(donnees[,1:2],test=dtest,cl=donnees[,3],k=k,prob=TRUE)
      prob <- attr(model, "prob")
      prob <- as.numeric(round(ifelse(model=="1", prob, 1-prob)))
    }
    }
  if (methode=="arbre"){
    prob <- as.numeric(predict(model,newdata=dtest,type="class"))-1
  }
  #  prob <- as.numeric(predict(mod,newdata=dtest)$class)
  #  prob <- attr(mod, "prob")
  #  prob <- ifelse(mod=="1", prob, 1-prob)
  prob3 <- matrix(prob, nrow = length(px1), ncol = length(px2))
  rownames(prob3) <- px1
  colnames(prob3) <- px2
  #  par(mar=rep(2,4))
  a <- melt(prob3)
  a$label <- factor(rbinom(nrow(a),1,0.5))
  a$label <- as.factor(a$value)
#  levels(a$label) <- levels(donnees$Y)
  names(donnees) <- noms
  names(a)[1:2] <- noms[1:2]
  p1 <-  ggplot(a)+geom_contour(aes_(x =as.name(noms[1]), y =as.name(noms[2]), z = as.name("value")),color="black")+
    geom_tile(data = a, aes_(x=as.name(noms[1]),y=as.name(noms[2]),color=as.name("label")),alpha=0.05)+
    scale_colour_brewer(palette="Set1")+theme_minimal()+scale_color_manual(values = c("1"="red","0"="blue"))
  if (points){
    noms[3] <- "label"
    names(donnees)[3] <- "label"
  p1 <- p1 + geom_point(data=donnees,aes_(x=as.name(noms[1]),y=as.name(noms[2]),color=as.name(noms[3]),shape=as.name(noms[3])),size=2,alpha=1)
  }
  return(p1)
}
