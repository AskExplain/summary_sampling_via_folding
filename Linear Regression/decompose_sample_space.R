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
  
  u_svd <- t(irlba(x%*%t(x),50)$u)
  
  
  factor_yx <- t(u_svd)
  
  for (j in 1:permutation_test_number){
    set.seed(j)
    train_ids <- sample(c(1:6435),ceiling(6435*0.8))
    
    a <- Sys.time()
    lm_yx_hat <- lm( u_svd[,train_ids]%*%y[train_ids,] ~ ., data = data.frame(x = u_svd[,train_ids]%*%x[train_ids,]))
    yx_hat <- predict(lm_yx_hat, newdata = data.frame(x = u_svd%*%x))
    b <- Sys.time()
    
    c <- Sys.time()
    lm_y_hat <- lm( y[train_ids,] ~ ., data = data.frame(x = x[train_ids,]))
    y_hat <- predict(lm_y_hat, newdata = data.frame(x = x[-train_ids,]))
    d <- Sys.time()
    
    total_time.lreg.1 <- c(total_time.lreg.1,d-c)
    total_time.svd <-  c(total_time.svd,b-a)
    
    total_mae.lreg.1 <- c(total_mae.lreg.1,mean(abs(y[-train_ids,] - y_hat)))
    total_mae.svd <- c(total_mae.svd,mean(abs(y[-train_ids,] - (factor_yx%*%yx_hat)[-train_ids,])))
    
    total_cor.svd <- c(total_cor.svd,cor(lm_y_hat$coefficients[-1],lm_yx_hat$coefficients[-1],method = "pearson"))
    
  }
  
}
