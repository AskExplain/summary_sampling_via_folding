remove.packages("gcode")
devtools::install_github("AskExplain/gcode@alpha_test_v2022.1",force = T)

library(gcode)
library(mlbench)
data("Satellite")

categorical_label <- 37

if (is.null(permutation_test_number)){
  permutation_test_number <- 10
}

total_mae.gcode <- c()
total_time.gcode <- c()

total_mae.lreg.2 <- c()
total_time.lreg.2 <- c()

for (i in 1:dim(Satellite[,-categorical_label])[2]){
  
  x <- as.matrix(Satellite[,-c(i,categorical_label)])
  y <- Satellite[,i,drop=F]
  
  config <- gcode::extract_config(F)
  config$init <- c("irlba","irlba")
  config$i_dim <- 30
  config$j_dim <- 5
  config$tol <- 1e-3
  
  gcode.model <- gcode::gcode(data_list = list((x)), config = config, join = list(data_list =1, alpha=1,beta=1,code=1))
  folded_x <- gcode.model$main.parameters$alpha[[1]]%*%x
  to_unfold_y <- t(gcode.model$main.parameters$alpha[[1]])%*%MASS::ginv((gcode.model$main.parameters$alpha[[1]])%*%t(gcode.model$main.parameters$alpha[[1]]))
  
  
  for (j in 1:permutation_test_number){
    set.seed(j)
    train_ids <- sample(c(1:dim(Satellite)[1]),ceiling(dim(Satellite)[1]*0.9))
    
    a.s <- gcode.model$main.parameters$alpha[[1]][,train_ids]
    
    a <- Sys.time()
    gcode.mlp <- RSNNS::mlp(y=a.s%*%y[train_ids,], x=a.s%*%(x)[train_ids,], linOut = T, maxit = 500, size = 15)
    b <- Sys.time()
                                    
    yx_hat <- predict(gcode.mlp,  folded_x)
    
    c <- Sys.time()
    standard.mlp <- RSNNS::mlp( y= y[train_ids,], x = x[train_ids,], linOut = T, maxit = 500, size = 15)
    d <- Sys.time()
    
    y_hat <- predict(standard.mlp, newdata = as.data.frame(x = x[-train_ids,]))
    
    total_time.lreg.2 <- c(total_time.lreg.2,d-c)
    total_time.gcode <-  c(total_time.gcode,b-a)
    
    total_mae.lreg.2 <- c(total_mae.lreg.2,mean(abs(y[-train_ids,] - y_hat)))
    total_mae.gcode <-  c(total_mae.gcode,mean(abs(y[-train_ids,] - (to_unfold_y%*%yx_hat)[-train_ids,])))
  
  }
  
}
