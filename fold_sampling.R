setwd("~/Documents/main_files/AskExplain/variant_spark/Rscript/0_explore/")

permutation_test_number <- 1

# Run SVD decomposition of samples to a reduced sample space
source("./decompose_sample_space.R")

# Run gcode encoding of samples to a reduced sample space
source("./encode_sample_space.R")


########################### Total MAE


# Linear Regression 1
print(c("Linear Regression (run in svd script) Mean Absolute Error: "))
sum(total_mae.lreg.1)

# Linear Regression 2
print(c("Linear Regression (run in gcode script) Mean Absolute Error: "))
sum(total_mae.lreg.2)

# Singular Value Decomposition 
print(c("SVD Mean Absolute Error: "))
sum(total_mae.svd)

# Generative Encoding
print(c("gcode Mean Absolute Error: "))
sum(total_mae.gcode)



boxplot(data.frame(gcode = total_mae.gcode,
                   svd = total_mae.svd,
                   linreg1 = total_mae.lreg.1,
                   linreg2 = total_mae.lreg.2), 
        outline = F,
        horizontal = T, 
        main = "Total Mean Absolute Error")



########################### Total Run time


# Linear Regression 1
print(c("Linear Regression (run in svd script) runtime: "))
sum(total_time.lreg.1)

# Linear Regression 2
print(c("Linear Regression (run in gcode script) runtime: "))
sum(total_time.lreg.2)

# Singular Value Decomposition 
print(c("SVD runtime: "))
sum(total_time.svd)

# Generative Encoding
print(c("gcode runtime: "))
sum(total_time.gcode)

boxplot(data.frame(gcode = total_time.gcode,
                   svd = total_time.svd,
                   linreg1 = total_time.lreg.1,
                   linreg2 = total_time.lreg.2), 
        outline = F,
        horizontal = T, 
        main = "Total Runtime")






