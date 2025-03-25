library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(class)

wine <- read_csv("wine.data", col_names = FALSE)
names(wine) <- c("Type","Alcohol","MalicAcid","Ash","Alcalinity","Magnesium","TotalPhenols",
                 "Flavanoids","NonflavanoidPhenols","Proanthocyanins","ColorIntensity",
                 "Hue","OD280","Proline")
wine$Type <- as.factor(wine$Type)

set.seed(123)
train.indexes <- sample(nrow(wine), 0.75*nrow(wine))
train <- wine[train.indexes, ]
test <- wine[-train.indexes, ]

tune.lin <- tune.svm(Type ~ Alcohol + Ash + Alcalinity + Magnesium,
                     data=train, kernel="linear", cost=c(0.1,1,10))
tune.rad <- tune.svm(Type ~ Alcohol + Ash + Alcalinity + Magnesium,
                     data=train, kernel="radial", cost=c(0.1,1,10), gamma=c(0.01,0.1,1))

best.lin <- tune.lin$best.model
best.rad <- tune.rad$best.model

pred.lin <- predict(best.lin, test)
pred.rad <- predict(best.rad, test)

cm.lin <- confusionMatrix(pred.lin, test$Type)
cm.rad <- confusionMatrix(pred.rad, test$Type)

knn.pred <- knn(train[, c("Alcohol","Ash","Alcalinity","Magnesium")],
                test[, c("Alcohol","Ash","Alcalinity","Magnesium")],
                train$Type, k=3)
cm.knn <- confusionMatrix(knn.pred, test$Type)

lin.precision <- cm.lin$byClass[,"Pos Pred Value"]
lin.recall <- cm.lin$byClass[,"Sensitivity"]
lin.f1 <- 2 * lin.precision * lin.recall / (lin.precision + lin.recall)

rad.precision <- cm.rad$byClass[,"Pos Pred Value"]
rad.recall <- cm.rad$byClass[,"Sensitivity"]
rad.f1 <- 2 * rad.precision * rad.recall / (rad.precision + rad.recall)

knn.precision <- cm.knn$byClass[,"Pos Pred Value"]
knn.recall <- cm.knn$byClass[,"Sensitivity"]
knn.f1 <- 2 * knn.precision * knn.recall / (knn.precision + knn.recall)

data.frame(
  Model = c("SVM Linear","SVM Radial","kNN"),
  Precision = c(mean(lin.precision, na.rm=TRUE),
                mean(rad.precision, na.rm=TRUE),
                mean(knn.precision, na.rm=TRUE)),
  Recall = c(mean(lin.recall, na.rm=TRUE),
             mean(rad.recall, na.rm=TRUE),
             mean(knn.recall, na.rm=TRUE)),
  F1 = c(mean(lin.f1, na.rm=TRUE),
         mean(rad.f1, na.rm=TRUE),
         mean(knn.f1, na.rm=TRUE))
)

NY_House_Dataset <- read_csv("NY-House-Dataset.csv")

set.seed(123)
ny.indexes <- sample(nrow(NY_House_Dataset), 0.75*nrow(NY_House_Dataset))
ny.train <- NY_House_Dataset[ny.indexes, ]
ny.test <- NY_House_Dataset[-ny.indexes, ]

svm.reg <- svm(PRICE ~ PROPERTYSQFT, data=ny.train)
svm.pred <- predict(svm.reg, ny.test)
svm.df <- data.frame(real=ny.test$PRICE, pred=svm.pred)

ggplot(svm.df, aes(x=real, y=pred)) +
  geom_point() +
  stat_smooth(method="lm")

lin.mod <- lm(PRICE ~ PROPERTYSQFT, data=ny.train)
lin.pred <- predict(lin.mod, ny.test)
lin.df <- data.frame(real=ny.test$PRICE, pred=lin.pred)

ggplot(lin.df, aes(x=real, y=pred)) +
  geom_point() +
  stat_smooth(method="lm")

res.svm <- svm.pred - ny.test$PRICE
res.lin <- lin.pred - ny.test$PRICE

res.svm.df <- data.frame(index=1:length(res.svm), residual=res.svm)
res.lin.df <- data.frame(index=1:length(res.lin), residual=res.lin)

ggplot(res.svm.df, aes(x=index, y=residual)) +
  geom_point() +
  geom_hline(yintercept=0)

ggplot(res.lin.df, aes(x=index, y=residual)) +
  geom_point() +
  geom_hline(yintercept=0)
