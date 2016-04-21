setwd("~/MSPA/PREDICT 498 - Capstone/Assignments")
LoanStats2014 <- read.csv("~/MSPA/PREDICT 498 - Capstone/Assignments/LoanStats2014.csv")

myvars <- c("id", "member_id", "loan_amnt", "funded_amnt", "funded_amnt_inv", "term", "int_rate", "installment", "grade", "sub_grade", "emp_title", "emp_length", "home_ownership", "annual_inc", "verification_status", "issue_d", "loan_status", "pymnt_plan", "url", "desc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "out_prncp", "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "recoveries", "collection_recovery_fee", "last_pymnt_d", "last_pymnt_amnt", "next_pymnt_d", "last_credit_pull_d", "collections_12_mths_ex_med", "mths_since_last_major_derog", "policy_code", "application_type", "annual_inc_joint", "dti_joint", "verification_status_joint") 

df1 <- LoanStats2014[myvars]

require(Hmisc)
require(plyr)

sapply(df1, describe)

############### Removed Variables ###############

summary(df1$annual_inc_joint) ## - There are 235629 NAs for this variable
summary(df1$application_type) ## - All observations equal INDIVIDUAL
describe(df1$collection_recovery_fee) ## - Less then 4% of loans entered collections 
describe(df1$collections_12_mths_ex_med) ## - 99% of loans DID NOT enter collections 
describe(df1$delinq_2yrs) ## - The vast majority of observations are zero-value
describe(df1$desc) ## - Narrative/text comments from borrower.
describe(dti_joint) ## - There are 235629 NAs for this variable
describe(df1$earliest_cr_line) ## - Mixture of Day/Month and Month/Year dates
describe(df1$emp_title)
describe(df1$last_credit_pull_d)
describe(df1$next_pymnt_d)
describe(df1$policy_code)
describe(df1$pymnt_plan)
describe(df1$title)
describe(df1$url)
describe(df1$verification_status_joint)
describe(df1$zip_code)

############### Retained Variables ###############

describe(df1$addr_state)
df1$state <- df1$addr_state ## - Converted alphabetical variable to a numeric categorical variable
df1$state <- as.numeric(df1$state)
hist(df1$state)

describe(df1$annual_inc)
hist(df1$annual_inc)
plot(df1$annual_inc)

describe(df1$dti)
hist(df1$dti)

describe(df1$emp_length)
df1$jobtime <- df1$emp_length
df1$jobtime <- as.numeric(df1$jobtime)
describe(df1$jobtime)
hist(df1$jobtime)

describe(df1$funded_amnt)
hist(df1$funded_amnt)

describe(df1$funded_amnt_inv)
hist(df1$funded_amnt_inv)

describe(df1$grade)
df1$grade1 <- df1$grade
df1$grade1 <- as.factor(df1$grade1)
df1$grade1 <- as.numeric(df1$grade1)
df1$gradeA <- df1$grade1
df1$gradeB <- df1$grade1
df1$gradeC <- df1$grade1
df1$gradeD <- df1$grade1
df1$gradeE <- df1$grade1
df1$gradeF <- df1$grade1
df1$gradeG <- df1$grade1
df1$gradeA[df1$gradeA > 1] <- 0
df1$gradeB[df1$gradeB < 2] <- 0
df1$gradeB[df1$gradeB > 2] <- 0
df1$gradeB[df1$gradeB == 2] <- 1
df1$gradeC[df1$gradeC < 3] <- 0
df1$gradeC[df1$gradeC > 3] <- 0
df1$gradeC[df1$gradeC == 3] <- 1
df1$gradeD[df1$gradeD < 4] <- 0
df1$gradeD[df1$gradeD > 4] <- 0
df1$gradeD[df1$gradeD == 4] <- 1
df1$gradeE[df1$gradeE < 5] <- 0
df1$gradeE[df1$gradeE > 5] <- 0
df1$gradeE[df1$gradeE == 5] <- 1
df1$gradeF[df1$gradeF < 6] <- 0
df1$gradeF[df1$gradeF > 6] <- 0
df1$gradeF[df1$gradeF == 6] <- 1
df1$gradeG[df1$gradeG < 7] <- 0
df1$gradeG[df1$gradeG > 7] <- 0
df1$gradeG[df1$gradeG == 7] <- 1

describe(df1$home_ownership)
df1$homes <- df1$home_ownership
df1$homes <- as.numeric(df1$homes)

describe(df1$initial_list_status)
df1$liststatus <- df1$initial_list_status
df1$liststatus <- as.numeric(df1$liststatus)
df1$whole <- df1$liststatus
df1$fractional <- df1$liststatus
df1$whole[df1$whole > 1] <- 1
df1$whole[df1$whole < 2] <- 0
df1$whole[df1$fractional > 1] <- 0
df1$whole[df1$fractional < 2] <- 1
hist(df1$liststatus)
hist(df1$whole)
hist(df1$fractional)

describe(df1$inq_last_6mths)
hist(df1$inq_last_6mths)

describe(df1$installment)
hist(df1$installment)

df1$interest <- df1$int_rate
df1$interest <- as.numeric(sub("%", "", df1$interest))/100
describe(df1$interest)
hist(df1$interest)

describe(df1$issue_d)
df1$issuedate <- df1$issue_d
df1$issuedate <- as.numeric(df1$issuedate)
hist(df1$issuedate)

describe(df1$last_pymnt_amnt)
hist(df1$last_pymnt_amnt)

describe(df1$last_pymnt_d)
df1$lastpayment <- df1$last_pymnt_d
df1$lastpayment <- as.numeric(df1$lastpayment)
hist(df1$lastpayment)

describe(df1$loan_amnt)
hist(df1$loan_amnt)

describe(df1$loan_status)
df1$loanstatus <- df1$loan_status
df1$loanstatus <- as.numeric(df1$loanstatus)
hist(df1$loanstatus)

describe(df1$mths_since_last_delinq)
hist(df1$mths_since_last_delinq)
df1$mths_since_last_delinq[is.na(df1$mths_since_last_delinq)] <- 0 ## - Impute zero for missing values
df1$delinquent <- df1$mths_since_last_delinq
df1$delinquent[df1$never_delinquent > 0] <- 2
df1$delinquent[df1$never_delinquent < 1] <- 1
df1$delinquent[df1$never_delinquent > 1] <- 0
df1$never_delinquent <- df1$mths_since_last_delinq
df1$never_delinquent[df1$delinquent > 0] <- 1
df1$never_delinquent[df1$delinquent < 1] <- 0
describe(df1$never_delinquent)
describe(df1$delinquent)
hist(df1$delinquent)
hist(df1$never_delinquent)

describe(df1$mths_since_last_major_derog)
hist(df1$mths_since_last_major_derog)
df1$mths_since_last_major_derog[is.na(df1$mths_since_last_major_derog)] <- 0 ## - Impute zero for missing values
describe(df1$mths_since_last_major_derog)
hist(df1$mths_since_last_major_derog)

describe(df1$mths_since_last_record)
hist(df1$mths_since_last_record)
df1$mths_since_last_record[is.na(df1$mths_since_last_record)] <- 0 ## - Impute zero for missing values
describe(df1$mths_since_last_record)
hist(df1$mths_since_last_record)

describe(df1$open_acc)
hist(df1$open_acc)

describe(df1$out_prncp)
hist(df1$out_prncp)

describe(df1$out_prncp_inv)
hist(df1$out_prncp_inv)

describe(df1$pub_rec)
hist(df1$pub_rec)

describe(df1$purpose)
df1$need <- df1$purpose
df1$need <- as.numeric(df1$need)
hist(df1$need)

describe(df1$recoveries)
hist(df1$recoveries)

describe(df1$revol_bal)
hist(df1$revol_bal)

describe(df1$revol_util)
hist(df1$revol_util)

df1$revutil <- df1$int_rate
df1$revutil <- as.numeric(sub("%", "", df1$revutil))/100
describe(df1$revutil)
hist(df1$revutil)

describe(df1$sub_grade)
df1$subgrade <- df1$sub_grade
df1$subgrade <- as.numeric(df1$subgrade)
df1$subA <- df1$subgrade
df1$subB <- df1$subgrade
df1$subC <- df1$subgrade
df1$subD <- df1$subgrade
df1$subE <- df1$subgrade
df1$subF <- df1$subgrade
df1$subG <- df1$subgrade
df1$subA[df1$subA > 5] <- 0
df1$subB[df1$subB < 6] <- 0
df1$subB[df1$subB > 10] <- 0
df1$subC[df1$subC < 11] <- 0
df1$subC[df1$subC > 15] <- 0
df1$subD[df1$subD < 16] <- 0
df1$subD[df1$subD > 20] <- 0
df1$subE[df1$subE < 21] <- 0
df1$subE[df1$subE > 25] <- 0
df1$subF[df1$subF < 26] <- 0
df1$subF[df1$subF > 30] <- 0
df1$subG[df1$subG < 31] <- 0
describe(subA)
describe(subB)
describe(subC)
describe(subD)
describe(subE)
describe(subF)
describe(subG)
hist(subA)
hist(subB)
hist(subC)
hist(subD)
hist(subE)
hist(subF)
hist(subG)

df1$terma <- df1$term
df1$terma <- as.factor(df1$terma)
df1$terma <- as.numeric(df1$terma)
df1$term60 <- df1$terma
df1$term36 <- df1$terma
df1$term36[df1$term36 < 2] = 1
df1$term36[df1$term36 > 1] = 0
df1$term60[df1$term60 < 2] = 0
df1$term60[df1$term60 > 1] = 1

describe(df1$total_acc)
hist(df1$total_acc)

describe(df1$total_pymnt)
hist(df1$total_pymnt)

describe(df1$total_pymnt_inv)
hist(df1$total_pymnt_inv)

describe(df1$total_rec_int)
hist(df1$total_rec_int)

describe(df1$total_rec_late_fee)
hist(df1$total_rec_late_fee)

describe(df1$total_rec_prncp)
hist(df1$total_rec_prncp)

describe(df1$verification_status)
df1$verification <- df1$verification_status
df1$verification <- as.numeric(df1$verification)
describe(df1$verification)
hist(df1$verification)


############### Correlations ###############

sapply(df1, class)

corvars <- c("loan_amnt", "funded_amnt", "funded_amnt_inv", "interest", "installment", "annual_inc", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "revol_bal", "revutil", "total_acc", "out_prncp", "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "recoveries", "collection_recovery_fee", "last_pymnt_amnt", "jobtime")

df2 <- df1[corvars]

correlations <- cor(df2, method = c("pearson"))

write.csv(correlations, "C:\\Users\\Michael\\Documents\\MSPA\\PREDICT 498 - Capstone\\Assignments\\correlations.csv")

































