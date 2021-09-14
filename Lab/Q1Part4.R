library("ggplot2") 
agegrp=c("21-24","24-27","27-30","30-33","33-36","36-39","39-42","42-45","45-48","48-51","51-54","54-57","57-60") 
noofloans=c(310,511,4000,4568,5698,8209,8117,9000,7600,6000,4000,2000,788) 
badrate=c(4.5,3.9,4.3,3.7,3.3,2.4,2.6,2.4,2,1.4,1.6,1.3,1.1) 
bank_data = data.frame(agegrp,noofloans) 
bank_data 
ggplot(bank_data, aes(agegrp, noofloans)) + geom_bar(stat = "identity") + geom_line(data = bank_data, aes(x = agegrp, y = badrate * 2500, color = "Bad Rate", group = 1), size = 1.5) + labs(colour = "", x = "",y = "") + theme(legend.position = c(0.98, 0.98), legend.justification = c(0.98, 0.98)) + scale_color_manual(values = c("orange")) + scale_y_continuous(sec.axis = sec_axis(~./2500, name = "Bad Rate")) 