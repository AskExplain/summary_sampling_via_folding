devtools::install_github("AskExplain/gcode@alpha_test_v2022.1",force = T)

library(gcode)
library(mlbench)
data("Satellite")

if (is.null(permutation_test_number)){
  permutation_test_number <- 10
}

total_mae.gcode <- c()
total_time.gcode <- c()

total_mae.lreg.2 <- c()
total_time.lreg.2 <- c()

for (i in 1:dim(Satellite[,-37])[2]){
  
  x <- as.matrix(Satellite[,-c(i,37)])
  y <- Satellite[,i,drop=F]
  
  config <- gcode::extract_config(F)
  config$init <- c("rnorm","rnorm")
  config$i_dim <- 40
  config$j_dim <- 40
  config$tol <- 1e-3
  
  gcode.model <- gcode::gcode(data_list = list(x), config = config, join = list(alpha=1,beta=1,code=1) )
  folded_x <- gcode.model$main.parameters$alpha[[1]]%*%x
  to_unfold_y <- t(gcode.model$main.parameters$alpha[[1]])%*%MASS::ginv((gcode.model$main.parameters$alpha[[1]])%*%t(gcode.model$main.parameters$alpha[[1]]))

  for (j in 1:permutation_test_number){
    set.seed(j + 1000 )
    train_ids <- sample(c(1:6435),ceiling(6435*0.8))
    
    a <- Sys.time()
    yx_hat <- predict(lm( gcode.model$main.parameters$alpha[[1]][,train_ids]%*%y[train_ids,] ~ ., data = data.frame(x = gcode.model$main.parameters$alpha[[1]][,train_ids]%*%x[train_ids,])), newdata = data.frame(x = folded_x))
    b <- Sys.time()
    
    c <- Sys.time()
    y_hat <- predict(lm( y[train_ids,] ~ ., data = data.frame(x = x[train_ids,])), newdata = data.frame(x = x[-train_ids,]))
    d <- Sys.time()
    
    total_time.lreg.2 <- c(total_time.lreg.2,d-c)
    total_time.gcode <-  c(total_time.gcode,b-a)
    
    total_mae.lreg.2 <- c(total_mae.lreg.2,mean(abs(y[-train_ids,] - y_hat)))
    total_mae.gcode <-  c(total_mae.gcode,mean(abs(y[-train_ids,] - (to_unfold_y%*%yx_hat)[-train_ids,])))
  }
}
