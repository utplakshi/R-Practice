library(lattice)
library(scatterplot3d)
hsb2<-read.table('E:/Datasets/hsb2.csv',header=T,sep=",")
attach(hsb2)
hsb2
hsb2$ses.f = factor(hsb2$ses, labels=c("low", "middle", "high")) 
#histogram
histogram(~write, hsb2)
histogram(~write|factor(read),hsb2)
#conditional plot
histogram(~write | ses.f, hsb2)
#Check it yourself
histogram(~write | ses.f,plot.points=FALSE,auto.key = TRUE, hsb2)

#density plot
densityplot(~socst,hsb2)
#conditional plot
densityplot(~socst|ses.f,hsb2)
#Check it yourself
densityplot(~socst|ses.f,plot.points=FALSE,auto.key = TRUE,hsb2)

#quantile-quantile plots
qqmath(~write,hsb2)
#conditional plot
qqmath(~write|ses.f,hsb2)
#check it yourself
qqmath(~write|ses.f,plot.points=FALSE,auto.key = TRUE,hsb2)

#box and whiskers plot
bwplot(~math, hsb2)
#conditional plots
bwplot(ses.f~math, hsb2)
#check for yourself
bwplot(ses.f~math,plot.points=FALSE,auto.key = TRUE,hsb2)

#Multivariate Plots
#scatter plots
xyplot(write~math,hsb2)
#conditional plot
xyplot(write~math|factor(ses.f),hsb2)
#check for yourself
xyplot(write~math|ses.f,plot.points=FALSE,auto.key = TRUE,hsb2)

#strip plot
stripplot(~write,hsb2)
#conditional plot
stripplot(~write|ses.f,hsb2)
#check for yourself
stripplot(write~math|ses.f,plot.points=FALSE,auto.key = TRUE,hsb2)

#bar plot
barchart(~write,hsb2)
#conditional
barchart(~write|ses.f,hsb2)
#check for yourself
barchart(~write|ses.f,plot.points=FALSE,auto.key = TRUE,hsb2)

#3d graph
scatterplot3d(hsb2[,1:3])
#conditional
scatterplot3d(hsb2[,1:3],pch=16,highlight.3d=TRUE,type="h",main="3D Scatter Plot with Vertical Lines")







#Try it yourself
dia<-diamonds
dia$cut.f = factor(dia$cut, labels=c("Ideal", "Premium
", "Good","Very Good","Fair")) 
#histogram
histogram(~depth, dia)
#conditional plot
histogram(~depth | table, dia)

#density plot
densityplot(~depth,dia)
#conditional plot
densityplot(~depth|cut.f,dia)

#quantile-quantile plots
qqmath(~depth, dia)
#conditional plot
qqmath(~depth|cut.f,dia)

#box and whiskers plot
bwplot(~depth, dia)
#conditional plots
bwplot(cut.f~depth, dia)

#Multivariate Plots
#scatter plots
xyplot(depth~x,dia)
#conditional plot
xyplot(depth~x|cut.f,dia)


#strip plot
stripplot(~depth,dia)
#conditional plot
stripplot(~depth|cut.f,dia)

#bar plot
barchart(~depth,dia)
#conditional
barchart(~depth|cut.f,dia)

#3D plot
scatterplot3d(dia[,1:3])
#conditional plot
scatterplot3d(dia[,1:3],pch=16,highlight.3d=TRUE,type="h",main="3D Scatter Plot with Vertical Lines")