# devtools::document("~/Documents/main_files/AskExplain/generative_encoder/package/generative_encoder_v2022.1/")
# remove.packages("gcode")
# devtools::install_local("~/Documents/main_files/AskExplain/generative_encoder/package/generative_encoder_v2022.1//",force = T)
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

total_cor.gcode <- c()

for (i in 1:dim(Satellite[,-categorical_label])[2]){
  
  x <- as.matrix(Satellite[,-c(i,categorical_label)])
  y <- Satellite[,i,drop=F]
  
  config <- gcode::extract_config(F)
  config$init <- c("rnorm","rnorm")
  config$i_dim <- 50
  config$j_dim <- 50
  config$tol <- 1
  
  gcode.model <- gcode::gcode(data_list = list(x), config = config, join = list(alpha=1,beta=1,code=1) )
  folded_x <- gcode.model$main.parameters$alpha[[1]]%*%x
  to_unfold_y <- t(gcode.model$main.parameters$alpha[[1]])%*%MASS::ginv((gcode.model$main.parameters$alpha[[1]])%*%t(gcode.model$main.parameters$alpha[[1]]))
  
  
  for (j in 1:permutation_test_number){
    set.seed(j)
    train_ids <- sample(c(1:dim(Satellite)[1]),ceiling(dim(Satellite)[1]*0.8))
    
    a.s <- gcode.model$main.parameters$alpha[[1]][,train_ids]
    transform.a.s <- a.s
      
    a <- Sys.time()
    gcode.ranger <- ranger::ranger( y = transform.a.s%*%y[train_ids,] , x = transform.a.s%*%x[train_ids,], importance = "impurity")
    b <- Sys.time()
                                    
    yx_hat <- predict(gcode.ranger, data = folded_x)$predictions
    
    c <- Sys.time()
    standard.ranger <- ranger::ranger( y[train_ids,] ~ ., data = data.frame(x = x[train_ids,]), importance = "impurity")
    d <- Sys.time()
    
    y_hat <- predict(standard.ranger, data = data.frame(x = x[-train_ids,]))$predictions
    
    total_time.lreg.2 <- c(total_time.lreg.2,d-c)
    total_time.gcode <-  c(total_time.gcode,b-a)
    
    total_mae.lreg.2 <- c(total_mae.lreg.2,mean(abs(y[-train_ids,] - y_hat)))
    total_mae.gcode <-  c(total_mae.gcode,mean(abs(y[-train_ids,] - (to_unfold_y%*%yx_hat)[-train_ids,])))
  
    total_cor.gcode <- c(total_cor.gcode,cor(ranger::importance(gcode.ranger),ranger::importance(standard.ranger),method = "pearson"))
  }
  
}
