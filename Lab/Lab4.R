#importing the libraries
library("ggplot2") 
library("ggExtra")
library("ggcorrplot")
#****************Construction of Marginal Histogram/Box Plot********************
#Importing the inbuilt data set ORANGE
Orange
o<-Orange
theme_set(theme_bw())
#people whose age is between 400 and 1500
o_select<-o[o$age>=400 & o$age<1500, ]
o_select
g<- ggplot(o,aes(age,circumference))+geom_count()+geom_smooth(method="lm",se=F)
ggMarginal(g, type = "histogram", fill="red")
ggMarginal(g, type = "boxplot", fill="orange")
#***********************Correlogram*********************************************
orange_corr<-o[,c(1)]
corr<-round(cor(orange_corr),1)
ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size=3,
           method = "circle",
           colors = c("tomato2","white","springgreen3"),
           title = "Correlogram of Orange")