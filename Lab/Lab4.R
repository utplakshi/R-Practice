#importing the libraries
library("ggplot2") 
library("ggExtra")
library("ggcorrplot")
library("quantmod")
library("lubridate")
#****************Construction of Marginal Histogram/Box Plot********************
#Importing the inbuilt data set ORANGE
Orange
o<-Orange
theme_set(theme_bw())
#people whose age is between 400 and 1500
o_select<-o[o$age>=400 & o$age<1500, ]
o_select
g<- ggplot(o,aes(age,circumference))+geom_count()+geom_smooth(method="lm",se=F)
ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")
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
#*********************Diverging Bar********************************************
data('Orange')#Load data
Orange$age_z<-round((Orange$age - mean(Orange$age))/sd(Orange$age), 2)
Orange$age_type <- ifelse(Orange$age_z < 0, "below", "above") 
Orange <- Orange[order(Orange$age_z), ]
ggplot(Orange, aes(x=circumference, y=age_z, label=age_z)) + 
  geom_bar(stat='identity', aes(fill=age_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()
#*********************Diverging Lollipop Chart*********************************
ggplot(Orange, aes(x=circumference, y=age_z, label=age_z)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = circumference, 
                   yend = age_z, 
                   xend = circumference), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Normalized mileage from 'mtcars': Lollipop") + 
  ylim(-2.5, 2.5) +
  coord_flip()
#*******************Diverging Dot Plot******************************************

# Plot
ggplot(Orange, aes(x=circumference, y=age_z, label=age_z)) + 
  geom_point(stat='identity', aes(col=age_type), size=6)  +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()
#********************Diverging Area Plot***************************************
data("Orange")
# Compute % Returns
Orange$returns_age <- c(0, diff(Orange$age)/Orange$age[-length(Orange$age)])
# Plot
ggplot(Orange[1:100, ], aes(circumference, age)) + 
  geom_area() + 
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title="Area Chart", 
       subtitle = "Perc Returns for Personal Savings", 
       y="age", 
       caption="Source: Orange")
###################PRACTICE####################################################
################# Marginal Histogram/Box plot###################################

data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")

# Scatterplot marginal histogram
theme_set(theme_bw())  # pre-set the bw theme.
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")

################# Correlogram###################################################

# Correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)
###################Deviation####################################################
####################Diverging bars##############################################
theme_set(theme_bw())  

# Data Prep
data("mtcars")
mtcars# load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()
##############Diverging Lollipop Chart##########################################
theme_set(theme_bw())

ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = `car name`, 
                   yend = mpg_z, 
                   xend = `car name`), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Normalized mileage from 'mtcars': Lollipop") + 
  ylim(-2.5, 2.5) +
  coord_flip()
#####################Diverging Dot Plot#########################################
theme_set(theme_bw())

# Plot
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', aes(col=mpg_type), size=6)  +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()
#####################Diverging Area Chart######################################
data("economics", package = "ggplot2")
# Compute % Returns
economics$returns_perc <- c(0, diff(economics$psavert)/economics$psavert[-
                                                                           length(economics$psavert)])
# Create break points and labels for axis ticks
brks <- economics$date[seq(1, length(economics$date), 12)]
lbls <- lubridate::year(economics$date[seq(1, length(economics$date), 12)])
# Plot
ggplot(economics[1:100, ], aes(date, returns_perc)) + 
  geom_area() + 
  scale_x_date(breaks=brks, labels=lbls) + 
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title="Area Chart", 
       subtitle = "Perc Returns for Personal Savings", 
       y="% Returns for Personal savings", 
       caption="Source: economics")
###########################Lab 4 Part 2
library(tidyverse)
O<-Orange
head(o)
as.tibble(o)
Mean_Orange<-mean(Orange$circumference)
Mean_Orange
Var_Orange<-var(Orange$circumference)
SD_Orange<-sd(Orange$circumference)
SD_Orange
####Predictor here(Independent) is age and Circumference is the response(Dependent)
Model<-lm(age~circumference, data=Orange)
Model
anova(Model)
####Predictor here(Independent) is circumference and age is the response(Dependent)
Model1<-lm(circumference~age, data = Orange)
Model1
anova(Model)
summary(Model)
summary(Model1)

