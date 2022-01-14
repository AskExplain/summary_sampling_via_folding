library(irlba)
library(mlbench)
data("Satellite")

if (is.null(permutation_test_number)){
  permutation_test_number <- 10
}

total_mae.svd <- c()
total_time.svd <- c()

total_mae.lreg.1 <- c()
total_time.lreg.1 <- c()

total_cor.svd <- c()

for (i in 1:dim(Satellite[,-37])[2]){
  
  x <- as.matrix(Satellite[,-c(i,37)])
  y <- Satellite[,i,drop=F]
  
  u_svd <- t(irlba(x,nv = 15)$u)
  
  folded_x <- u_svd%*%x
  to_unfold_y <- t(u_svd)
  
  for (j in 1:permutation_test_number){
    set.seed(j)
    train_ids <- sample(c(1:dim(Satellite)[1]),ceiling(dim(Satellite)[1]*0.8))
    
    transform.a.s <- u_svd[,train_ids]
    
    a <- Sys.time()
    svd.ranger <- ranger::ranger( y = transform.a.s%*%y[train_ids,] , x = transform.a.s%*%x[train_ids,], importance = "impurity", num.trees = 1000)
    b <- Sys.time()
    
    yx_hat <- predict(svd.ranger, data = folded_x)$predictions
    
    c <- Sys.time()
    standard.ranger <- ranger::ranger( y[train_ids,] ~ ., data = data.frame(x = x[train_ids,]), importance = "impurity", num.trees = 1000)
    d <- Sys.time()
    
    y_hat <- predict(standard.ranger, data = data.frame(x = x[-train_ids,]))$predictions
    
    total_time.lreg.1 <- c(total_time.lreg.1,d-c)
    total_time.svd <-  c(total_time.svd,b-a)
    
    total_mae.lreg.1 <- c(total_mae.lreg.1,mean(abs(y[-train_ids,] - y_hat)))
    total_mae.svd <-  c(total_mae.svd,mean(abs(y[-train_ids,] - (to_unfold_y%*%yx_hat)[-train_ids,])))
    
    total_cor.svd <- c(total_cor.svd,cor(ranger::importance(svd.ranger),ranger::importance(standard.ranger),method = "pearson"))
  }
  
}
