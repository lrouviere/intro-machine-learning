generation <- function(n,napp=round(n/2)){
  seuil <- 0.25
  set.seed(1234)
  X1 <- runif(n)
  set.seed(5678)
  X2 <- runif(n)
  set.seed(9012)
  U <- runif(n)
  Y <- rep(0,n)
  Y[X1<=0.25 & U<=seuil] <- 1
  Y[X1>0.25 & X2>=0.75 & U<=seuil] <- 1
  Y[X1>0.25 & X2<0.75 & U>seuil] <- 1
  #plot(X1,X2,col=Y+1)
  donnees <- data.frame(X1,X2,Y)
  donnees$Y <- as.factor(donnees$Y)
  indapp <- 1:napp
  dapp <- donnees[indapp,]
  dtest <- donnees[-indapp,]
  return(list(donnees=donnees,train=dapp,test=dtest))
}

