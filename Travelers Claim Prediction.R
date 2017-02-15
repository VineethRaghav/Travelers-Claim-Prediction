library(MASS)
library(DMwR)
library(e1071)
tdata <- read.csv("C:\\Users\\vineeth raghav\\Downloads\\Kaggle\\Travelers\\Kangaroo_train.csv")
tdata1 <- tdata
# Standardizing the continuous values
tdata1$veh_value <- (tdata1$veh_value - min(tdata1$veh_value))/ (max(tdata1$veh_value)-min(tdata1$veh_value))
tdata1$veh_age <- (tdata1$veh_age - min(tdata1$veh_age))/ (max(tdata1$veh_age)-min(tdata1$veh_age))
tdata1$agecat <- (tdata1$agecat - min(tdata1$agecat))/ (max(tdata1$agecat)-min(tdata1$agecat))
# Creating dummy variables for categorical data
tdata1 <- cbind(tdata1,model.matrix(~tdata1$gender-1,data = tdata1))
tdata1 <- cbind(tdata1,model.matrix(~tdata1$veh_body-1,data = tdata1,))
tdata1 <- cbind(tdata1,model.matrix(~tdata1$area-1,data = tdata1))
# Removing the id and original variables
tdata1 <- tdata1[,-c(1,5,7,8)]
# Renaming the columns
colnames(tdata1) <- c("claimcst0","veh_value","exposure","veh_age","agecat","clm","numclaims","female","male","bus","convert","coupe","hback","hdtop","mcara","mibus","panvn","rdstr","sedan","stnwg","truck","ute","area_A","area_B","area_C","area_D","area_E","area_F")
tdata1[] <- lapply(tdata1,factor)
td <- mca(tdata1[,c(8:28)],5)
tdata1<-cbind(tdata1,td$rs)
tdata2 <- tdata1[,c(1:7,29:32)]
colnames(tdata2) <- c("claimcst0", "veh_value", "exposure", "veh_age", "agecat", 
                      +                       "clm", "numclaims", "var1", "var2", "var3", "var4")
tdata2 <- SMOTE(clm~.,tdata2,perc.over = 100,perc.under = 200)
model <- svm(clm ~ veh_value+exposure+veh_age+agecat+var1+var2+var3+var4,tdata2)
predicted <- predict(model,tdata2)
final <- cbind(predicted,tdata2$clm)
confusionMatrix(predicted,tdata2$clm)
