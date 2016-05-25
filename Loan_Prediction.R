library(data.table)
library(htmlTable)
library(caret)
library(ROCR)
loans <- fread(input='cleandata.csv', sep='auto',data.table=TRUE, nrows=-1, na.strings=NULL)

# Charged off-1 - 1
# Current-2 - 0
# Default-3 - 1
# Fully paid off-4 - 0
# In grace period-5 -0
# Late(16-30)-6 - 1
# Late(31-120)-7 - 1

#rejects <- fread(input='RejectStatsD.csv', sep='auto',data.table=TRUE, nrows=-1, na.strings=NULL)

htmlTable(loans[,.N, by=title])

htmlTable(loans[,.N, by=purpose])

loans[,.('COUNT' = .N), by=loanstatus]

#Identifies loan quality based on loan status
loans[,'LOAN_QUALITY' := ifelse((loanstatus == 1|loanstatus==3|loanstatus==6|loanstatus==7),1,0)]

loans[,.N, by=LOAN_QUALITY]

loans[,.N, by=status]

#******************************Clean Up ***********************************#
#Remove columns
loans[,':='(V1=NULL)]

#handles duplicate features by copying value into a vector, deleting the columns, and then writing vector back to data.table
vec_collections_12_mths_ex_med <- loans[,(collections_12_mths_ex_med)]
vec_mths_since_last_record <- loans[,(mths_since_last_record)]
vec_mths_since_last_major_derog <- loans[,(mths_since_last_major_derog)]

loans[,':='(collections_12_mths_ex_med=NULL)]
loans[,'collections_12_mths_ex_med':= vec_collections_12_mths_ex_med]
loans[,':='(mths_since_last_record=NULL)]
loans[,'mths_since_last_record':= vec_mths_since_last_record]

loans[,':='(mths_since_last_major_derog=NULL)]
loans[,'mths_since_last_major_derog':= vec_mths_since_last_major_derog]

loans[,acc_now_delinq := as.factor(acc_now_delinq)]
loans[,mort_acc := as.factor(mort_acc)]
loans[,num_tl_30dpd := as.numeric(num_tl_30dpd)]

#write.csv(loans, file='loan_prediction_prepped.csv')

set.seed(1)
inTrain <- createDataPartition(y = loans[,(id)],p = .75,list = FALSE)
training <- loans[ c(inTrain)]
testing <- loans[c(-inTrain)]
nrow(training)
nrow(testing)

#Forward Selection
#null_model <- glm(LOAN_QUALITY ~1 ,family=binomial(link='logit'), data=training)

#summary(null_model)

#full_model <- glm(LOAN_QUALITY ~.-id - member_id - loanstatus -status,family=binomial(link='logit'), data=training)

#step(null_model, scope=list(lower=null_model, upper=full_model), direction="forward")

###################################################
# Call:  glm(formula = LOAN_QUALITY ~ recoveries + status + out_prncp_inv + 
#              last_pymnt_amnt + total_rec_prncp + installment + term36 + 
#              subgrade + total_rec_late_fee + funded_amnt_inv, family = binomial(link = "logit"), 
#            data = training)
# 
# Coefficients:
#   (Intercept)          recoveries          statusGood       out_prncp_inv  
# 1.356e+02          -1.807e-01          -1.537e+02          -2.318e-01  
# last_pymnt_amnt     total_rec_prncp         installment              term36  
# -9.018e-04          -2.281e-01          -2.253e-02           2.210e+00  
# subgrade  total_rec_late_fee     funded_amnt_inv  
# 5.656e-02           5.803e-02           2.286e-01  
# 
# Degrees of Freedom: 174765 Total (i.e. Null);  174755 Residual
# Null Deviance:	    109800 
# Residual Deviance: 0.0005531 	AIC: 22
str(training)

training[,.N, by=status]

##############################################################
Call:  glm(formula = LOAN_QUALITY ~ recoveries + total_rec_prncp + installment + 
             out_prncp_inv + loan_amnt + lastpayment + last_pymnt_amnt + 
             total_rec_late_fee + total_pymnt + funded_amnt_inv + total_pymnt_inv + 
             avg_cur_bal + issuedate + gradeA + grade1 + gradeB + interest + 
             subgrade + gradeF + acc_open_past_24mths + revol_bal + subD + 
             subA + revutil + mths_since_recent_bc + subC + out_prncp + 
             gradeG + subG + subF + need + mo_sin_old_rev_tl_op + annual_inc + 
             inq_last_6mths + num_bc_tl + mths_since_recent_inq + term36 + 
             verification + total_acc + num_tl_90g_dpd_24m + num_accts_ever_120_pd + 
             num_tl_30dpd + liststatus + dti + total_il_high_credit_limit + 
             total_bal_ex_mort + percent_bc_gt_75, family = binomial(link = "logit"), 
           data = training)
# 
# Coefficients:
#   (Intercept)                  recoveries             total_rec_prncp  
# -1.162e+01                  -7.796e-02                  -9.893e-02  
# installment               out_prncp_inv                   loan_amnt  
# 2.438e-02                  -1.492e-02                   7.467e-02  
# lastpayment             last_pymnt_amnt          total_rec_late_fee  
# -3.979e-01                  -2.430e-03                   6.817e-02  
# total_pymnt             funded_amnt_inv             total_pymnt_inv  
# 2.891e-02                   2.347e-02                  -2.935e-02  
# avg_cur_bal                   issuedate                      gradeA  
# -6.748e-06                  -2.636e-02                   7.774e+00  
# grade1                      gradeB                    interest  
# 3.312e+00                   6.875e+00                   1.322e+02  
# subgrade                      gradeF        acc_open_past_24mths  
# -1.091e+00                  -1.494e+01                   4.741e-02  
# revol_bal                        subD                        subA  
# -1.309e-05                   1.559e-01                   5.183e-01  
# revutil        mths_since_recent_bc                        subC  
# 2.230e-01                  -2.530e-03                   3.793e-01  
# out_prncp                      gradeG                        subG  
# -8.344e-02                  -3.431e+01                   9.115e-01  
# subF                        need        mo_sin_old_rev_tl_op  
# 3.957e-01                  -3.442e-02                  -8.699e-04  
# annual_inc              inq_last_6mths                   num_bc_tl  
# -8.193e-07                   8.306e-02                   1.897e-02  
# mths_since_recent_inq                      term36                verification  
# 8.484e-03                  -4.446e-01                  -7.350e-02  
# total_acc          num_tl_90g_dpd_24m       num_accts_ever_120_pd  
# -5.671e-03                   9.664e-02                  -2.631e-02  
# num_tl_30dpd                  liststatus                         dti  
# -4.837e-01                  -6.555e-02                   9.091e-03  
# total_il_high_credit_limit           total_bal_ex_mort            percent_bc_gt_75  
# -6.566e-06                   5.555e-06                   1.990e-03  
# 
# Degrees of Freedom: 174765 Total (i.e. Null);  174718 Residual
# Null Deviance:	    109800 
# Residual Deviance: 21800 	AIC: 21900
# There were 50 or more warnings (use warnings() to see the first 50)

fit_model <- glm(formula = LOAN_QUALITY ~ recoveries + total_rec_prncp + installment + 
                   out_prncp_inv + loan_amnt + lastpayment + last_pymnt_amnt + 
                   total_rec_late_fee + total_pymnt + funded_amnt_inv + total_pymnt_inv + 
                   avg_cur_bal + issuedate + gradeA + grade1 + gradeB + interest + 
                   subgrade + gradeF + acc_open_past_24mths + revol_bal + subD + 
                   subA + revutil + mths_since_recent_bc + subC + out_prncp + 
                   gradeG + subG + subF + need + mo_sin_old_rev_tl_op + annual_inc + 
                   inq_last_6mths + num_bc_tl + mths_since_recent_inq + term36 + 
                   verification + total_acc + num_tl_90g_dpd_24m + num_accts_ever_120_pd + 
                   num_tl_30dpd + liststatus + dti + total_il_high_credit_limit + 
                   total_bal_ex_mort + percent_bc_gt_75, family = binomial(link = "logit"), 
                 data = training)

htmlTable(data.frame(as.list(fit_model$coefficients)))



prob <- predict(fit_model, newdata=testing, type='response')

#pred <- prediction(prob, testing$LOAN_QUALITY)

table(actual=testing$LOAN_QUALITY, predicted=prob>.5)

htmlTable(cbind(testing$LOAN_QUALITY, prob))

write.csv(cbind(testing$LOAN_QUALITY, prob), file='pred_performance.csv')

ROCRpred <- prediction(prob, testing$LOAN_QUALITY)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = FALSE, text.adj = c(-0.2,1.7))