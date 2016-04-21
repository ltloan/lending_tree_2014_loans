library(corrplot)
library(ggplot2)

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

#Variable by Variable EDA:
  ##summary(x)
  #generate a box and whisker plot for the status & loan status variables
  #boxplot(x~status,data=d14_50, main="Loan Status", xlab="Loan Status")
  #hist(x,breaks=50)

#collections_12_mths_ex_med
summary(collections_12_mths_ex_med)
boxplot(collections_12_mths_ex_med~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(collections_12_mths_ex_med,breaks=50)
#contains 2 NA values

#mths_since_last_major_derog
summary(d14_50$mths_since_last_major_derog)
boxplot(mths_since_last_major_derog~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$mths_since_last_major_derog,breaks=50)
#contains 169153 NA values
d14_50$mths_since_last_major_derog[is.na(d14_50$mths_since_last_major_derog)] <- 0

#policy_code
summary(d14_50$policy_code)
#contains 2 NA values - all same values for the rest - dropping variable
d14_50 <- subset(d14_50, select = -c(policy_code))

#application_type
summary(d14_50$application_type)
#contains 235629 "Individual" application types
#DROPPING THIS!
d14_50 <- subset(d14_50, select = -c(application_type))

#annual_inc_joint
summary(d14_50$annual_inc_joint)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(annual_inc_joint))

#dti_joint
summary(d14_50$dti_joint)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(dti_joint))

#verification_status_joint
summary(d14_50$verification_status_joint)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(verification_status_joint))

#acc_now_delinq
summary(d14_50$acc_now_delinq)
#contains 2 NA values
#requires factorization
d14_50$acc_now_delinq <- factor(d14_50$acc_now_delinq)

#tot_coll_amt
summary(d14_50$tot_coll_amt)
#contains 2 NA values
boxplot(tot_coll_amt~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$tot_coll_amt)

#tot_coll_bal
summary(d14_50$tot_coll_bal)
#contains 2 NA values
boxplot(tot_coll_bal~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$tot_coll_bal)

#open_acc_6m
summary(d14_50$open_acc_6m)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(open_acc_6m))

#open_il_6m
summary(d14_50$open_il_6m)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(open_il_6m))

#open_il_12m
summary(d14_50$open_il_12m)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(open_il_12m))

#open_il_24m
summary(d14_50$open_il_24m)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(open_il_24m))

#mths_since_rcnt_il
summary(d14_50$mths_since_rcnt_il)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(mths_since_rcnt_il))

#total_bal_il
summary(d14_50$total_bal_il)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(total_bal_il))

#il_util
summary(d14_50$il_util)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(il_util))

#open_rv_12m
summary(d14_50$open_rv_12m)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(open_rv_12m))

#open_rv_24m
summary(d14_50$open_rv_24m)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(open_rv_24m))

#max_bal_bc
summary(d14_50$max_bal_bc)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(max_bal_bc))

#all_util
summary(d14_50$all_util)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(all_util))

#total_rev_hi_lim
summary(d14_50$total_rev_hi_lim)
boxplot(total_rev_hi_lim~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$total_rev_hi_lim)

#inq_fi
summary(d14_50$inq_fi)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(d14_50$inq_fi))

#total_cu_tl
summary(d14_50$total_cu_tl)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(total_cu_tl))

#inq_last_12m
summary(d14_50$inq_last_12m)
#logical operator, should be dropped as all values are NA
d14_50 <- subset(d14_50, select = -c(inq_last_12m))

#acc_open_past_24mths
summary(d14_50$acc_open_past_24mths)
#contains 2 NA values
boxplot(acc_open_past_24mths~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$acc_open_past_24mths)

#avg_cur_bal
summary(d14_50$avg_cur_bal)
#contains 8 NA values
boxplot(avg_cur_bal~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$avg_cur_bal)

#bc_open_to_buy
summary(d14_50$bc_open_to_buy)
#contains 2447 NA values
boxplot(bc_open_to_buy~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$bc_open_to_buy)

#bc_util
summary(d14_50$bc_util)
#contains 2447 NA values
boxplot(bc_util~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$bc_util)

#chargeoff_within_12_mths
summary(d14_50$bc_util)
#contains 2 NA values; binary variable

#delinq_amnt
summary(d14_50$delinq_amnt)
#contains 2 NA values
boxplot(delinq_amnt~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$delinq_amnt,breaks=50)

#mo_sin_old_il_acct          
summary(d14_50$mo_sin_old_il_acct)
#NAs = 7173
#d14_50$mo_sin_old_il_acct[is.na(d14_50$mo_sin_old_il_acct)] <- 0
boxplot(mo_sin_old_il_acct~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$mo_sin_old_il_acct,breaks=50)

#"mo_sin_old_rev_tl_op"
summary(d14_50$mo_sin_old_rev_tl_op)
#2 values are NA
boxplot(mo_sin_old_rev_tl_op~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$mo_sin_old_rev_tl_op,breaks=50)

#"mo_sin_rcnt_rev_tl_op"
summary(d14_50$mo_sin_rcnt_rev_tl_op)
#2 values are NA
boxplot(mo_sin_rcnt_rev_tl_op~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$mo_sin_rcnt_rev_tl_op,breaks=50)

#mo_sin_rcnt_tl
summary(d14_50$mo_sin_rcnt_tl)
#2 values are NA
boxplot(mo_sin_rcnt_tl~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$mo_sin_rcnt_tl,breaks=50)

#mort_acc
summary(d14_50$mort_acc) #requires factorization
d14_50$mort_acc <- factor(d14_50$mort_acc)

#"mths_since_recent_bc"
summary(d14_50$mths_since_recent_bc)
#NAs = 2248
boxplot(mths_since_recent_bc~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$mths_since_recent_bc,breaks=50)

#"mths_since_recent_bc_dlq"
summary(d14_50$mths_since_recent_bc_dlq)
#NAs = 173350
#setting NAs to 0
d14_50$mths_since_recent_bc_dlq[is.na(d14_50$mths_since_recent_bc_dlq)] <- 0
boxplot(mths_since_recent_bc_dlq~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$mths_since_recent_bc_dlq,breaks=50)

#"mths_since_recent_inq"
summary(d14_50$mths_since_recent_inq)
#NAs = 21694
d14_50$mths_since_recent_inq[is.na(d14_50$mths_since_recent_inq)] <- 0
boxplot(mths_since_recent_inq~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$mths_since_recent_inq,breaks=50)

#"mths_since_recent_revol_delinq"
summary(d14_50$mths_since_recent_revol_delinq)
#NAs = 150865
d14_50$mths_since_recent_revol_delinq[is.na(d14_50$mths_since_recent_revol_delinq)] <- 0
boxplot(mths_since_recent_revol_delinq~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$mths_since_recent_revol_delinq,breaks=50)

#"num_accts_ever_120_pd"
summary(d14_50$num_accts_ever_120_pd)
boxplot(num_accts_ever_120_pd~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_accts_ever_120_pd,breaks=50)

#"num_actv_bc_tl"
summary(d14_50$num_actv_bc_tl)
boxplot(num_actv_bc_tl~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_actv_bc_tl,breaks=50)

#"num_actv_rev_tl"
summary(d14_50$num_actv_rev_tl)
boxplot(num_actv_rev_tl~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_actv_rev_tl,breaks=50)

#"num_bc_sats"
summary(d14_50$num_bc_sats)
boxplot(num_bc_sats~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_bc_sats,breaks=50)

#"num_bc_tl"
summary(d14_50$num_bc_tl)
boxplot(num_bc_tl~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_bc_sats,breaks=50)

#"num_il_tl"      
summary(d14_50$num_il_tl)
boxplot(num_il_tl~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_il_tl,breaks=50)

#"num_op_rev_tl"
summary(d14_50$num_op_rev_tl)
boxplot(num_op_rev_tl~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_op_rev_tl,breaks=50)

#"num_rev_accts"
summary(d14_50$num_rev_accts)
boxplot(num_rev_accts~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_op_rev_tl,breaks=50)

#"num_rev_tl_bal_gt_0"
summary(d14_50$num_rev_tl_bal_gt_0)
boxplot(num_rev_tl_bal_gt_0~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_rev_tl_bal_gt_0,breaks=50)

#"num_sats"
summary(d14_50$num_sats)
boxplot(num_sats~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_sats,breaks=50)

#"num_tl_120dpd_2m"
summary(d14_50$num_tl_120dpd_2m)
#NAs = 7862; replacing with 0
d14_50$num_tl_120dpd_2m[is.na(d14_50$num_tl_120dpd_2m)] <- 0
boxplot(num_tl_120dpd_2m~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_tl_120dpd_2m,breaks=50)

#"num_tl_30dpd"
summary(d14_50$num_tl_30dpd)
#NAs = 2; should be factorized
d14_50$num_tl_30dpd <- factor(d14_50$num_tl_30dpd)

#"num_tl_90g_dpd_24m"
summary(d14_50$num_tl_90g_dpd_24m)
boxplot(num_tl_90g_dpd_24m~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_tl_90g_dpd_24m,breaks=50)

#"num_tl_op_past_12m"
summary(d14_50$num_tl_op_past_12m)
boxplot(num_tl_op_past_12m~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$num_tl_op_past_12m,breaks=50)

#"pct_tl_nvr_dlq"
summary(d14_50$pct_tl_nvr_dlq)
boxplot(pct_tl_nvr_dlq~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$pct_tl_nvr_dlq,breaks=50)

#"percent_bc_gt_75"
summary(d14_50$percent_bc_gt_75)
#NAs = 2559
boxplot(percent_bc_gt_75~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$percent_bc_gt_75,breaks=50)

#"pub_rec_bankruptcies"
summary(d14_50$pub_rec_bankruptcies)
boxplot(pub_rec_bankruptcies~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$pub_rec_bankruptcies,breaks=50)

#"tax_liens"
summary(d14_50$tax_liens)
boxplot(tax_liens~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$tax_liens,breaks=50)

#"tot_hi_cred_lim"
summary(d14_50$tot_hi_cred_lim)
boxplot(tot_hi_cred_lim~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$tot_hi_cred_lim,breaks=50)

#"total_bal_ex_mort"
summary(d14_50$total_bal_ex_mort)
boxplot(total_bal_ex_mort~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$total_bal_ex_mort,breaks=50)

#"total_bc_limit"
summary(d14_50$total_bc_limit)
boxplot(total_bc_limit~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$total_bc_limit,breaks=50)

#"total_il_high_credit_limit"
summary(d14_50$total_il_high_credit_limit)
boxplot(total_il_high_credit_limit~status,data=d14_50, main="Loan Status", xlab="Loan Status")
hist(d14_50$total_il_high_credit_limit,breaks=50)

#status
d14_50$status <- factor(d14_50$status)


##### DROP ALL REMAINING dNA VALUES #####
d <- d14_50[complete.cases(d14_50),]
nrow(d)
#232906 rows remaining

#get the correlation for each of the variables
M <- cor(d14_50[num_list],use="complete")
corrplot(M, method = "circle")

#plot a scatterplot of all numberic variables
pairs(d14_50)
