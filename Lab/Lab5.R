library("factoextra")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(MASS)
library(ggplot2)
###############################PCA#############################################
mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)
summary(mtcars.pca)
str(mtcars.pca)

ggbiplot(mtcars.pca)
screeplot(mtcars.pca, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
cumpro <- cumsum(mtcars.pca$sdev^2 / sum(mtcars.pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
plot(mtcars.pca$x[,1],mtcars.pca$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")
#################################LDA############################################
require(MASS)
# Load data
data(iris)
head(iris, 3)
r <- lda(formula = Species ~ ., 
         data = iris, 
         prior = c(1,1,1)/3)
r$prior
r$counts
r$means
r$scaling
r$svd
prop = r$svd^2/sum(r$svd^2)
prop
r2 <- lda(formula = Species ~ ., 
          data = iris, 
          prior = c(1,1,1)/3,
          CV = TRUE)
head(r2$class)
head(r2$posterior, 3)
train <- sample(1:150, 75)
r3 <- lda(Species ~ ., # training model
          iris, 
          prior = c(1,1,1)/3, 
          subset = train)
plda = predict(object = r, # predictions
               newdata = iris[-train, ])
head(plda$class) # classification result
head(plda$posterior, 3)
head(plda$x, 3) # LD projections

###########################Q1.##################################################
breast_cancer<-read.csv('E:/Datasets/breast_cancer.csv',header=T)
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(breast_cancer) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))
bc.pca <- prcomp(breast_cancer[c(3:32)], center = TRUE, scale = TRUE)
summary(bc.pca)
screeplot(bc.pca , type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(bc.pca $sdev^2 / sum(bc.pca $sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)
plot(bc.pca$x[,1],bc.pca$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")

fviz_pca_ind(bc.pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = breast_cancer$diagnosis, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))
###########################Q2.##################################################
################################LDA#############################################
#1)load necessary libraries

#library(MASS)
#library(ggplot2)

#2)load the data

attach(iris)

#3)scale the data

#view structure of iris
str(iris)
#scale each predictor variable(i.e the first 4 columns)
iris[1:4]<-scale(iris[1:4])
#find mean of each predictor variable
apply(iris[1:4],2,mean)
#find standard deviation of each predictor variable
apply(iris[1:4],2,sd)

#4)create training sets and test samples

#make it reproducible
set.seed(1)
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(iris), replace=TRUE, prob=c(0.7,0.3))
train <- iris[sample, ]
test <- iris[!sample, ] 

#5)Fit the LDA method

#fit LDA model
model <- lda(Species~., data=iris)
#view model output
model

#6) use LDA model to make predictions on test data

predicted <- predict(model, test)
names(predicted)

#view predicted class for first six observations in test set
head(predicted$class)

#view posterior probabilities for first six observations in test set
head(predicted$posterior)

#view linear discriminant for first six observations in test set
head(predicted$x)

#find accuracy of model
mean(predicted$class==test$Species)

#7)Visualize the results

#define data to plot
lda_plot <- cbind(iris, predict(model)$x)
#create plot
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = Species))

##############################################PCA 2-D Projection of Iris dataset
iris.pca<-prcomp(iris[1:4])
summary(iris.pca)
str(iris.pca)
ggbiplot
plot(iris.pca$x[,1], iris.pca$x[,2],xlab = "PC1",ylab="PC2",col=iris$Species,pch=19)
#or
iris_pca<-as.data.frame(iris.pca$x)
iris_pca$group<-sapply(strsplit(as.character(row.names(df)), "_"), "[[", 1)
head(iris_pca)
p<-ggplot(iris_pca,aes(x=PC1,y=PC2,color=Species ))
p<-p+geom_point()
p
