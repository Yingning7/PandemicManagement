library(pROC)
iris2 <- iris
iris2$label[iris2$Species == 'setosa'] <- 0
iris2$label[iris2$Species == 'versicolor'] <- 1
iris2 <- iris2[-which(iris2$Species == 'virginica'), ]  
iris2$Species <- NULL 
roc1<- roc(label~Sepal.Length, data = iris2)
roc2 <- roc(label~Sepal.Width, data=iris2)
roc3 <- roc(label~Petal.Length, data=iris2)
roc4 <- roc(label~Petal.Width, data=iris2)
plot(roc1, print.thres = TRUE, print.auc = TRUE, col = "blue", main = "sepal length as the classifier")
plot.roc(roc2, add = TRUE, print.thres = TRUE, col = "red")
plot.roc(roc3, add = TRUE, print.thres = TRUE, col = "green")

