library(corrplot)
library(ggplot2)

#### DRAFT VERSION FOR TESTING PURPOSES ####

st_path = '/Users/aowens/maine-capstone/data' #must be changed
d_file_2014 = "LoanStats3c.csv"

d14 = read.csv(file.path(st_path,d_file_2014),skip=1)

d14$status[d14$loan_status == "Default"] = "Bad"
d14$status[d14$loan_status == "Late (16-30 days)"] = "Bad"
d14$status[d14$loan_status == "Late (31-120 days)"] = "Bad"
d14$status[d14$loan_status == "Current"] = "Good"
d14$status[d14$loan_status == "Charged Off"] = "Good"
d14$status[d14$loan_status == "Fully Paid"] = "Good"
d14$status[d14$loan_status == "In Grace Period"] = "Good"
d14$status[d14$loan_status == "Default"] = "Bad"

d14_50 = d14[50:112]

#FOR EACH VARIABLE IN THE DF:
num_list = c()
for (name in names(d14[50:112])){
  #generate a summary of the varibles
  summary(d14_50[name])
  #loop through non-character variables:
  if (sapply(d14_50[name],class) != "character" and sapply(d14_50[name],class) != "logical"){
    #generate a list of integer objects for the correlation plot
    num_list <- c(num_list,name)
    #generate a histogram for each
    plot=qplot(name, data=data, geom="histogram") 
    ggsave(plot,file=paste(name[1]," histogram.pdf"))
    #generate a box and whisker plot for the status & loan status variables 
    boxplot(name~status,data=d14_50, main="Car Milage Data", xlab="Loan Status", ylab=names())
  }
}

#get the correlation for each of the variables
M <- cor(d14_50[num_list],use="complete")
corrplot(M, method = "circle")

#plot a scatterplot of all numberic variables
pairs(d14_50[num_list])

