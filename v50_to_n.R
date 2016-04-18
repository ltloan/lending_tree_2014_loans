library(corrplot)
library(ggplot2)

#### DRAFT VERSION FOR TESTING PURPOSES

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

d14_50 = d15[50:112]

#FOR EACH VARIABLE IN THE DF:

#generate a summary of the varibles
summary(d14_50$var1)


#if class(d14_50$var1) != "integer":

#ELIF class(d14_50$var1) == "integer":
#For each integer variable:
#generate a histogram
plot=qplot(var1, data=data, geom="histogram") 
ggsave(plot,file="graph1.pdf")

#generate a box and whisker plot for the status & loan status variables
#status
boxplot(var1~status,data=d14_50, main="Car Milage Data", xlab="Loan Status", ylab=names())

#generate a list of integer objects for the correlation plot
num_list = c()

#get the correlation for each of the variables
M <- cor(d14_50[num_list],use="complete")x
corrplot(M, method = "circle")

#plot a scatterplot of all numberic variables
pairs(d14_50[num_list])

