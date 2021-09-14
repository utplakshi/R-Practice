library(ggplot2)
Age_Groups=c("20-24","24-27","27-30","30-33","33-36","36-39","39-42","42-45","45-48","48-51","51-54","54-57","57-60")
Number_of_Loans=c(310,511,4000,4568,5698,8209,8117,9000,7600,6000,4000,2000,288)
df<-data.frame(Age_Groups,Number_of_Loans)
head(df)  
p=ggplot(data=df, aes(x=Age_Groups, y= Number_of_Loans)) + geom_bar(stat='identity') +geom_col()
p

