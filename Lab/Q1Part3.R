library(ggplot2)
Bad_Rate=c(4.5,3.9,4.3,3.7,3.3,2.4,2.6,2.4,2,1.4,1.6,1.3,1.1)
Age_Groups=c("20-24","24-27","27-30","30-33","33-36","36-39","39-42","42-45","45-48","48-51","51-54","54-57","57-60")
Number_of_Loans=c(310,511,4000,4568,5698,8209,8117,9000,7600,6000,4000,2000,288)
df<-data.frame(Age_Groups,Number_of_Loans,Bad_Rate)
head(df)  
p=ggplot(df)+geom_col(aes(x=Age_Groups,y=Number_of_Loans),size=1)+geom_line(aes(x=Age_Groups,y=2500*Bad_Rate),size=1.5,group = 1,color='yellow', position = "identity",  stat = "identity")+  scale_y_continuous(sec.axis = sec_axis(~./2500.0, name = "Bad Rate"))
p

