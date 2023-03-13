#Load libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(C50)
library(glmnet)
library(tidyverse)
library(lubridate)
library(ROCR)
library(broom)
library(ranger)

options(dplyr.summarise.inform = FALSE)

#Import dataset
LC_Data<-read.csv("C:/Users/Prajwal/Downloads/lcDataSampleFall22/lcDataSampleFall22.csv")

#Data Exploration
#proportion of defaults (‘charged off’ vs ‘fully paid’ loans) in the data

prop_of_defaults<-LC_Data %>% group_by(loan_status)%>%summarise(n=n()) %>% mutate(freq=n*100/sum(n))
setnames(prop_of_defaults,old = c('loan_status','n'),new = c('loanStatus','totalCount'))
print(prop_of_defaults)

#Bar graph to visualize the proportion
ggplot(LC_Data,aes(x = loan_status)) + geom_bar()

#Pie chart representaion of the proportion of defaults
lbls<- prop_of_defaults$'loanStatus'
slices<- prop_of_defaults$totalCount
pie(slices,labels = lbls, main = "Proportion of Charged off v/s Fully paid" )

#Proportion of default rate by Grade
default_by_grade<- LC_Data %>% group_by(grade,loan_status) %>% summarise(n=n()) %>% mutate(freq=n*100/sum(n))
print(default_by_grade)
setnames(default_by_grade,old = c('loan_status','n'),new = c('loanStatus','totalcount'))
print(default_by_grade)

#Line graph representation of Fully paid% with Grade
default_by_grade<- filter(default_by_grade,loanStatus == "Fully Paid")
ggplot(data = default_by_grade,aes(x = grade,y=freq,group = 1)) + geom_line() + geom_point()+labs(y= "Fully paid%",x="Grade")

#Proportion of default rate by SubGrade
default_by_subgrade<-LC_Data%>%group_by(sub_grade,loan_status)%>%summarise(n=n())%>%mutate(freq=n*100/sum(n))
setnames(default_by_subgrade,old = c('loan_status','n'),new = c('loanStatus','totalCount'))
print(default_by_subgrade)

#Line graph representation of Fully paid% with Grade
default_by_subgrade<-filter(default_by_subgrade,loanStatus == "Fully Paid")
ggplot(default_by_subgrade,aes(x=sub_grade,y=freq,group = 1)) + geom_line() + geom_point() + labs(x="Subgrade", y="Fully paid%")

#How does default rate vary with loan grade? Does it vary with sub-grade? And is this what you would expect, and why?
defaultrate_loanGrade<-LC_Data%>%group_by(grade)%>%tally()
setnames(defaultrate_loanGrade,old=c('grade','n'),new=c('Grade','Default Rate'))
print(defaultrate_loanGrade)

defaultrate_loanSubgrade<-LC_Data%>%group_by(sub_grade)%>%tally()
setnames(defaultrate_loanSubgrade,old=c('sub_grade','n'),new=c('Subgrade','Default Rate'))
print(defaultrate_loanSubgrade)

#Bar graph showing the distribution of grades with loan status.
ggplot(LC_Data,aes(x=grade,fill=loan_status)) + geom_bar(position = "fill") + labs(x='Grade', y='Loan Status')

#Bar graph showing the distribution of sub-grades with loan status
ggplot(LC_Data,aes(x=sub_grade,fill=loan_status)) + geom_bar(position="fill") + labs(x='SubGrade',y='Loan Status')

#How many loans are there in each grade? And do loan amounts vary by grade?
#Loans in each grade
Loanscount_eachgrade<-LC_Data%>%group_by(grade)%>%tally()
setnames(Loanscount_eachgrade,old = c('grade','n'),new = c('Grade','Count'))
print(Loanscount_eachgrade)

#Loans variation by grade
Loans_eachgrade<-LC_Data %>% group_by(grade)%>%summarise(sum(loan_amnt))
setnames(Loans_eachgrade,old = c('grade','sum(loan_amnt)'),new = c('Grade','Sum of Loan Amount'))
print(Loans_eachgrade)

#Loans variation by subgrade
Loans_eachsubgrade<-LC_Data %>% group_by(sub_grade)%>%summarise(sum(loan_amnt))
setnames(Loans_eachsubgrade,old = c('sub_grade','sum(loan_amnt)'),new = c('Sub Grade','Sum of Loan Amount'))
print(Loans_eachsubgrade)


#Graph view- segregating Charged off vs Fully Paid
ggplot(LC_Data,aes(x=loan_amnt)) + geom_histogram(aes(fill=grade)) +facet_wrap(~loan_status)

#Calculating mean interest to compare with grade and subgrade 
#Comparison with Grade
int_bygrade<-LC_Data%>%group_by(grade)%>%summarise(Interest_Rate = mean(int_rate))
print(int_bygrade)

#Comparison with Subgrade
int_by_subgrade<-LC_Data%>%group_by(sub_grade)%>%summarise(InterestRateSG = mean(int_rate))
print(int_by_subgrade)

#Plot for mean Interest rate with grade
ggplot(int_bygrade,aes(x=grade,y=Interest_Rate,group=1)) + geom_point() + geom_line()

#Plot for mean Interest rate with subgrade
ggplot(int_by_subgrade,aes(x=sub_grade,y=InterestRateSG,group=1)) + geom_point() + geom_line()

#Summary for Average and standard-deviation of Interest rate by grade and subgrade
characteristics_intRate_grade<-LC_Data%>%group_by(grade)%>%summarise(numLoans=n(),avgInterest=mean(int_rate),sd_interest=sd(int_rate))
print(characteristics_intRate_grade)

characteristics_intRate_subgrade<-LC_Data%>%group_by(sub_grade)%>%summarise(num_Loans=n(),avg_Interest=mean(int_rate),s_d_interest=sd(int_rate))
print(characteristics_intRate_subgrade)

mean_int<-LC_Data%>%group_by(grade,sub_grade)%>%summarise(mean_intrate=mean(int_rate))
print(mean_int)

#Line plot for Standard dev of int rate versus grades of loans
ggplot(characteristics_intRate_grade,aes(x=grade,y=sd_interest,group=1)) + geom_point() + geom_line()

ggplot(characteristics_intRate_subgrade,aes(x=sub_grade,y=s_d_interest,group=1)) + geom_point() + geom_line()

#Minimum interest rates for each grades and subgrades
min_intrate_grade<-LC_Data%>%group_by(grade)%>%summarise(min(int_rate))
print(min_intrate_grade)
min_intrate_subgrade<-LC_Data%>%group_by(sub_grade)%>%summarise(min(int_rate))
print(min_intrate_subgrade)

#Minimum interest rates for each grades and subgrades
max_intrate_grade<-LC_Data%>%group_by(grade)%>%summarise(max(int_rate))
print(max_intrate_grade)
max_intrate_subgrade<-LC_Data%>%group_by(sub_grade)%>%summarise(max(int_rate))
print(max_intrate_subgrade)

#Data For loans fully paid - time-to-payoff
head(LC_Data[,c("last_pymnt_d","issue_d")])
LC_Data$last_pymnt_d<-paste(LC_Data$last_pymnt_d,"-01",sep = "")

#Then convert this character to a date type variable
LC_Data$last_pymnt_d<-parse_date_time(LC_Data$last_pymnt_d, "myd")
head(LC_Data[,c("last_pymnt_d","issue_d")])
LC_Data$actualTerm <- ifelse(LC_Data$loan_status == "Fully Paid", as.duration(LC_Data$issue_d %--% LC_Data$last_pymnt_d) / dyears(1), 3)
head(LC_Data$actualTerm)

head(LC_Data$actualTerm)

ggplot(LC_Data,aes(x=actualTerm,y=grade)) + geom_boxplot() + coord_flip() + labs(x="Average actual Term",y="Grade")
dim(LC_Data)

#Actual Annual Return percentage
LC_Data$Ann_Ret<-ifelse(LC_Data$actualTerm>0,(((LC_Data$total_pymnt - LC_Data$funded_amnt)/LC_Data$funded_amnt)/LC_Data$actualTerm)*100,0)
Annualreturn<- ((LC_Data$total_pymnt - LC_Data$funded_amnt)/LC_Data$funded_amnt)*(12/36)*100

#Return from charged off loans vary by loan grades
LC_Data$return<-LC_Data$total_pymnt - LC_Data$funded_amnt
LC_Data$returnperyear<-(LC_Data$return/LC_Data$funded_amnt)/(3*100)

#Table for return per year  - grade and loan status
return_defaults<-LC_Data%>%group_by(grade,loan_status)%>%summarise(return_per_year<-mean(returnperyear))
print(return_defaults)

#Table for returns per year from default loans-grade
ret_def<-filter(LC_Data,loan_status=="Charged Off")
returns_defaults<-ret_def%>%group_by(grade)%>%summarise(mean_returnperyear=mean(returnperyear),sd_returnperyear=sd(returnperyear),min_returnperyear=min(returnperyear),max_returnperyear=max(returnperyear))
print(returns_defaults)

ret_loan_status<-LC_Data%>%group_by(loan_status)%>%summarise(mean_returnperyear=mean(returnperyear),sd_returnperyear=sd(returnperyear),min_returnperyear=min(returnperyear),max_returnperyear=max(returnperyear))
print(ret_loan_status)

ggplot(ret_loan_status,aes(x=loan_status,y=mean_returnperyear,group=1)) + geom_point() + geom_line() + labs(y="Average return per year",x="Loan Status")

returns_grades<-LC_Data%>%group_by(grade)%>%summarise(mean_returnperyear=mean(returnperyear),sd_returnperyear=sd(returnperyear),min_returnperyear=min(returnperyear),max_returnperyear=max(returnperyear))
print(returns_grades)       

#Line plot for return from loans V/S grades
ggplot(returns_grades,aes(x=grade,y=mean_returnperyear,group=1)) + geom_point() + geom_line() + labs(x="Grade",y="Average return per year")

#Return from loans vary by loan sub grades
return_subgrade<-LC_Data%>%group_by(sub_grade)%>%summarise(mean_returnperyear=mean(returnperyear),sd_returnperyear=sd(returnperyear),min_returnperyear=min(returnperyear),max_returnperyear=max(returnperyear))
print(return_subgrade)

#Line plot for return from loans V/S subgrades
ggplot(return_subgrade,aes(x=sub_grade,y=mean_returnperyear,group=1)) + geom_point() + geom_line() + labs(x="Sub Grades",y="Average return per year")

#Average returns versus Average interest rate
returns_intrate_grade<-LC_Data%>%group_by(grade)%>%summarise(mean_returnperyear=mean(returnperyear),avg_intrate=mean(int_rate))
print(returns_intrate_grade)

returns_intrate_subgrade<-LC_Data%>%group_by(sub_grade)%>%summarise(mean_returnperyear=mean(returnperyear),avg_intrate=mean(int_rate))
print(returns_intrate_subgrade)

dim(LC_Data)

#Loans granted versus purpose
loan_purpose<-LC_Data%>%group_by(purpose)%>%summarise(n=n(),mean_loan=mean(loan_amnt))%>%mutate(freq=n/sum(n)*100)
setnames(loan_purpose,old = c('purpose','n'),new = c('Purpose','Total count'))
print(loan_purpose)

#Loan status versus purpose.
purpose_loan_status<-LC_Data%>%group_by(purpose,loan_status)%>%summarise(n=n(),mean_loan=mean(loan_amnt))%>%mutate(freq=n/sum(n)*100)
print(purpose_loan_status)

ggplot(purpose_loan_status,aes(x=purpose,y=mean_loan,group=1)) + geom_line() + geom_point() + labs(x="Purpose",y="Average Loan Amount")

#Loan grade versus purpose
purpose_loan_grade<-LC_Data%>%group_by(purpose,grade)%>%summarise(n=n(),mean_loan=mean(loan_amnt))%>%mutate(freq=n/sum(n)*100)
print(purpose_loan_grade)

#Employment period versus Mean Loan amount
employ_loanamount<-LC_Data%>%group_by(emp_length)%>%summarise(n=n(),mean_loan=mean(loan_amnt))
print(employ_loanamount)
ggplot(employ_loanamount,aes(x=emp_length,y=mean_loan,group=1)) + geom_point()+geom_line() +labs(x="Employee length",y="Average loan amount")

#Employment length versus grade:
employ_grade<-LC_Data%>%group_by(emp_length,grade)%>%summarise(n=n(),mean_loan=mean(loan_amnt))
print(employ_grade)

#Employment length versus purpose
employ_lenth<-LC_Data%>%group_by(emp_length,purpose)%>%summarise(n=n(),mean_loan=mean(loan_amnt))
print(employ_lenth)

#Annual income versus purpose
Anninc_pur<-LC_Data%>%group_by(purpose)%>%summarise(n=n(),mean_anninc=mean(annual_inc))
print(Anninc_pur)

#Annual income versus grade
Anninc_grade1<-LC_Data%>%group_by(grade)%>%summarise(n=n(),mean_anninc=mean(annual_inc))
print(Anninc_grade1)

ggplot(Anninc_grade1,aes(x=grade,y=mean_anninc,group=1)) + geom_point() + geom_line() + labs(x='Grade',y='Average annual Income')
dim(LC_Data)

#Generate some (at least 3) new derived attributes which you think may be useful for predicting default., and explain what these are. For these, do an analyses as in the questions above (as reasonable based on the derived variables).

#Derived attribute-1: proportion of satisfactory bankcard accounts
LC_Data$satisbankcardAccts<-ifelse(LC_Data$num_bc_tl>0,LC_Data$num_bc_sats/LC_Data$num_bc_tl,0)
head(LC_Data$satisbankcardAccts)

#Derived Attribute-2: length of borrower's history with LC
LC_Data$earliest_cr_line<-paste(LC_Data$earliest_cr_line,"-01",sep="")
LC_Data$earliest_cr_line<-parse_date_time(LC_Data$earliest_cr_line,"myd")
LC_Data$borr_history<-as.duration(LC_Data$earliest_cr_line %--% LC_Data$issue_d)/dyears(1)
head(LC_Data$borr_history)

#Derived attribute-3: ratio of open Accounts to total Accounts
LC_Data$openacc_ratio<-ifelse(LC_Data$total_acc>0,LC_Data$open_acc/LC_Data$total_acc,0)
head(LC_Data$openacc_ratio)

#Summary with line plot for openAccRatio with grades
openAcc_grade<-LC_Data%>%group_by(grade)%>%summarise(avg_openAcc_grade=mean(openacc_ratio))
print(openAcc_grade)
ggplot(openAcc_grade,aes(x=grade,y=avg_openAcc_grade,group=1))+geom_point()+geom_line()


#Summary with line plot for openAccRatio with loan status
openAcc_loanstatus<-LC_Data%>%group_by(loan_status)%>%summarise(avg_open_Acc_loanstatus=mean(openacc_ratio))
print(openAcc_loanstatus)
ggplot(openAcc_loanstatus,aes(x=loan_status,y=avg_open_Acc_loanstatus,group=1)) +geom_point()+geom_line()

#Derived attribute-4: Balance amount to pay
LC_Data$balance_to_pay<-LC_Data$funded_amnt-LC_Data$total_pymnt
glimpse(LC_Data$balance_to_pay)

bal_topay_grade<-LC_Data%>%group_by(grade)%>%summarise(bal_left=sum(balance_to_pay))
print(bal_topay_grade)

bal_topay_subgrade<-LC_Data%>%group_by(sub_grade)%>%summarise(balLeft=sum(balance_to_pay))
print(bal_topay_subgrade)

#Line plot for Balance left by grades
ggplot(bal_topay_grade,aes(x=grade,y=bal_left,group=1)) + geom_line()+geom_point()

#Line plot for Balance left by subgrades
ggplot(bal_topay_subgrade,aes(x=sub_grade,y=balLeft,group=1)) + geom_point() + geom_line()

# negative values indicate most of the loans are paid off and with some interest rate. That's why total payment exceeds the funded amount
#for positive values the loan is charged off

#LC assigned Grade variation by borrow History
loan_v_borrow<-LC_Data%>%group_by(grade)%>%summarise(avg_bor_history=mean(borr_history))
print(loan_v_borrow)

#plot to understand variation between borrow history and grade
ggplot(loan_v_borrow,aes(x=grade,y=avg_bor_history,group=1))+geom_point()+geom_line()

#Summary with line plot for mean borrHistory with Loan status
loan_status_borr<-LC_Data%>%group_by(loan_status)%>%summarise(avg_borrhistory=mean(borr_history))
print(loan_status_borr)
ggplot(loan_status_borr,aes(x=loan_status,y=avg_borrhistory,group=1)) + geom_line() + geom_point()

LC_Data%>%group_by(grade)%>%summarise(avgSatisBankCard_prop=mean(satisbankcardAccts))
dim(LC_Data)

#Checking for missimg values and the proportion of missing values in different variables
#Drop col's with all empty values into new data frame -lcdf
lcdf<-LC_Data%>%select_if(function(x){!all(is.na(x))})
dim(lcdf)
head(lcdf)

#Columns where there are missing values
names(colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0])

#variable imputation
lcdf<-lcdf%>%replace_na(list(bc_open_to_buy=median(lcdf$bc_open_to_buy,na.rm=TRUE),num_tl_120dpd_2m=median(lcdf$num_tl_120dpd_2m,na.rm=TRUE),percent_bc_gt_75=median(lcdf$percent_bc_gt_75,na.rm=TRUE),bc_util=median(lcdf$bc_util,na.rm=TRUE)))
names(colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0])
dim(lcdf)

#Consider the potential for data leakage. You do not want to include variables in your model which may not be available when applying the model; that is, some data may not be available for new loans before they are funded. Leakage may also arise from variables in the data which may have been updated during the loan period (ie., after the loan is funded). Identify and explain which variables will you exclude from the model
# new data after considering for leakage
new_data<-lcdf%>%select(-c(funded_amnt_inv, term, emp_title, pymnt_plan, title, zip_code, addr_state, out_prncp, out_prncp_inv, total_pymnt_inv, total_rec_prncp, total_rec_int,total_rec_late_fee,recoveries, collection_recovery_fee, last_credit_pull_d, policy_code, disbursement_method, debt_settlement_flag, hardship_flag, application_type))

#removing additional variables which are not present in the 
new_data<-new_data%>%select(-c(last_pymnt_amnt,last_pymnt_d))
dim(new_data)

names(colMeans(is.na(new_data))[colMeans(is.na(new_data))>0])
new_data<-new_data%>%select(-c(return,returnperyear))

#Do a univariate analyses to determine which variables (from amongst those you decide to consider for the next stage prediction task) will be #individually useful for predicting the dependent variable (loan_status). For this, you need a measure of relationship between the dependent #variable and each of the potential predictor variables. Given loan-status as a binary dependent variable, which measure will you use? From your #analyses using this measure, which variables do you think will be useful for predicting loan_status? (Note – if certain variables on their own #are highly predictive of the outcome, it is good to ask if this variable has a leakage issue)

library(pROC) #importing the package which has AUC(..) function

#Using sapply function to apply AUC curve on the variables
#considered both numeric and factor variables.
# we need numeric variables to calculate the area under the curve
head(new_data$earliest_cr_line)
new_data$earliest_cr_line<-as.Date(new_data$earliest_cr_line)
new_data$issue_d<-as.Date(new_data$issue_d)
new_data<-mutate_if(new_data,is.character,as.factor)

#dropping the loan status variable
ds_train<-new_data%>%select(-c(loan_status))

aucAll<-sapply(ds_train%>%mutate_if(is.factor,as.numeric)%>%select_if(is.numeric),auc,response=new_data$loan_status)

#To determine which variables have AUC > 0.5
length(aucAll[aucAll>0.5])

selected_col<-names(aucAll[aucAll>0.5])

selected_col <- append(selected_col,"loan_status")

# adding the loan status variable
new_data <- new_data %>% select((selected_col))

library(broom)

#view a table output 
tidy(aucAll[aucAll>0.5])%>%view()

#arranging auc curve values in descending order 
tidy(aucAll)%>%arrange(desc(aucAll))

new_dt<-new_data
glimpse(new_data)

##pre-preprocessing data steps
#removing variables like actualTerm, actualRetrun
#excluding certain elements from the dataset because of data leakage issue. 
new_data2<-new_data
new_data1<-new_data%>%select(-c(Ann_Ret,balance_to_pay,total_pymnt))
new_data1<-new_data1%>%select(-c(grade,actualTerm,funded_amnt))

names(colMeans(is.na(new_data1)))[colMeans(is.na(new_data1))>0]

#replacing some of the missing NA values in the columns by median values
new_data1<- new_data1 %>% replace_na(list(mths_since_last_delinq=median(new_data1$mths_since_last_delinq, na.rm=TRUE),
                                       revol_util = median(new_data1$revol_util, na.rm=TRUE),
                                       avg_cur_bal = median(new_data1$avg_cur_bal, na.rm=TRUE),
                                       mths_since_recent_bc = median(new_data1$mths_since_recent_bc, na.rm=TRUE),
                                       mths_since_recent_inq = median(new_data1$mths_since_recent_inq, na.rm=TRUE),
                                       num_rev_accts = median(new_data1$num_rev_accts, na.rm=TRUE),
                                       pct_tl_nvr_dlq = median(new_data1$pct_tl_nvr_dlq, na.rm=TRUE),
                                       mo_sin_old_il_acct=median(new_data1$mo_sin_old_il_acct, na.rm=TRUE)))
names(colMeans(is.na(new_data1)))[colMeans(is.na(new_data1))>0]
new_data1$loan_status<-factor(new_data1$loan_status)
dim(new_data1)

library(rpart)
library(ranger)
library(rpart.plot)

#Splitting data into 70% training and 30%  testing ratio
PROP=0.7
nr<-nrow(new_data1)
trnIndex<-sample(1:nr,size=round(PROP*nr),replace = FALSE)
final_dataTrn<-new_data1[trnIndex,]
final_dataTst<-new_data1[-trnIndex,]
names(new_data1)


set.seed(673)
lcDT1<-rpart(loan_status~.,data = final_dataTrn,method = "class",parms = list(split="information"),control = rpart.control(cp=-1))
printcp(lcDT1)
plotcp(lcDT1)

library(caret)
library(ROCR)

#model1
lcDT1_1<-rpart(loan_status~.,data = final_dataTrn,method = "class",parms = list(split = "information"),control = rpart.control(cp=0.00019279))

#ROC plot
score<-predict(lcDT1_1,final_dataTst,type = "prob")[,"Charged Off"]
pred<-prediction(score,final_dataTst$loan_status,label.ordering = c("Fully Paid","Charged Off"))

#ROC curve
aucPerf<-performance(pred,"tpr","fpr")
plot(aucPerf)
abline(a=0,b=1)

#AUC value
aucPerf<-performance(pred,"auc")
aucPerf@y.values

#Lift curve
liftPerf<-performance(pred,"lift","rpp")

test_preds<-predict(lcDT1_1,final_dataTst,type = "prob")
thrsh=0.5

test_preds<-ifelse(test_preds[,1]>thrsh,"Charged Off","Fully Paid")
confusionMatrix(factor(test_preds,levels = c("Charged Off","Fully Paid")),final_dataTst$loan_status,positive = "Charged Off")

#Model2
lcDT1_2<-rpart(loan_status~.,data = final_dataTrn,method = "class",parms = list(split="information"),control = rpart.control(cp=0.00017302))

#ROC Plot
score<-predict(lcDT1_2,final_dataTst,type = "prob")[,"Charged Off"]
pred=prediction(score,final_dataTst$loan_status,label.ordering = c("Fully Paid","Charged Off"))

#ROC Curve
aucPerf<-performance(pred,"tpr","fpr")
plot(aucPerf)
abline(a=0,b=1)

#AUC value
aucPerf=performance(pred,"auc")
aucPerf@y.values

#Lift curve
liftPerf<-performance(pred,"lift","rpp")
plot(liftPerf)

test_preds<-predict(lcDT1_2,final_dataTst,type = "prob")
thrsh=0.5
test_preds<-ifelse(test_preds[,1]>thrsh,"Charged Off","Fully Paid")
confusionMatrix(factor(test_preds,levels = c('Charged Off','Fully Paid')),final_dataTst$loan_status,positive="Charged Off")

#Model 3
lcDT1_3<-rpart(loan_status~.,final_dataTst,method="class",parms = list(split="information"),control=rpart.control(cp= 0.000057672))

#ROC Plot
score<-predict(lcDT1_3,final_dataTst,type="prob")[,"Charged Off"]
pred<-prediction(score,final_dataTst$loan_status,label.ordering = c("Charged Off","Fully Paid"))


#ROC Curve
aucPerf<-performance(pred,"tpr","fpr")
plot(aucPerf)
abline(a=0,b=1)

#AUC Value
aucPerf<-performance(pred,"auc")
aucPerf@y.values

#Lift Curve
liftPerf<-performance(pred,"lift","rpp")
plot(liftPerf)

test_preds<-predict(lcDT1_3,final_dataTst,type = "prob")
thrsh=0.5
test_preds<-ifelse(test_preds[,1]>thrsh,"Charged Off","Fully Paid")
confusionMatrix(factor(test_preds,levels = c("Charged Off","Fully Paid")),final_dataTst$loan_status,positive="Charged Off")

set.seed(673)
library(ROSE)
install.packages("DMwR")
install.packages("ovun.samples")
final_dataTrn <- select(final_dataTrn, -c("mths_since_recent_bc_dlq", "total_cu_tl", "inq_fi","inq_last_12m","mths_since_rcnt_il", "total_bal_il", "il_util", "open_rv_24m", "max_bal_bc", "all_util","annual_inc_joint", "dti_joint","open_il_12m","mths_since_last_record"))
is.na(final_dataTrn)
dim(final_dataTrn)
balanced_train <- ovun.sample(loan_status ~ ., data = final_dataTrn,method="over",N=120000)$data
round(100*prop.table(table(final_dataTrn$loan_status)),digits=2)


#Now that we have over sampled the charged off data. We can use the above balanced train set to train our model and test our specificity and accuracy again
dt_model<-rpart(loan_status~.,data = balanced_train,method = "class",parms = list(split="information"),control = rpart.control(cp=-1))
plot(dt_model)
print(dt_model)

#Decision tree
dt_model<-rpart(loan_status~.,data = balanced_train,method="class",parms = list(split="information"),control=rpart.control(minsplit = 50,minbucket = 35,cp=0.0000083846))
plot(dt_model)
print(dt_model)

#Classification method
test_preds<-predict(dt_model,final_dataTst,type="class")
confusionMatrix(factor(test_preds,levels = c("Fully Paid","Charged Off")),final_dataTst$loan_status,positive = "Charged Off")

#Probability Method
test_preds<-predict(dt_model,final_dataTst,type = "prob")[,"Charged Off"]
pred<-prediction(test_preds,final_dataTst$loan_status,label.ordering = c("Fully Paid","Charged Off"))

#ROC curve
roc_curve<-performance(pred,"tpr","fpr")
plot(roc_curve)
abline(a=0,b=1)

#AUC curve
auc_score<-performance(pred,"auc")
auc_score@y.values

#Lift curve
liftPerf<-performance(pred,"lift","rpp")
plot(liftPerf)

## performing tests on training data

#Classification method
test_preds<-predict(dt_model,balanced_train,type="class")
confusionMatrix(factor(test_preds,levels = c("Fully Paid","Charged Off")),balanced_train$loan_status,positive = "Charged Off")

#Probability Method
test_preds<-predict(dt_model,balanced_train,type = "prob")[,"Charged Off"]
pred<-prediction(test_preds,balanced_train$loan_status,label.ordering = c("Fully Paid","Charged Off"))

#ROC Curve
roc_curve<-performance(pred,"tpr","fpr")
plot(roc_curve)
abline(a=0,b=1)

#AUC value
auc_score<-performance(pred,"auc")
auc_score@y.values

#Lift curve
liftPerf<-performance(pred,"lift","rpp")
plot(liftPerf)
dim(new_dt)
dim(new_data1)
# excluding certain elements from the dataset because of data leakage issue. 
new_data1 <- new_dt%>%select(-c("actualTerm","total_pymnt","balance_to_pay"))
names(colMeans(is.na(new_data1)))[colMeans(is.na(new_data1))>0]

new_data1<- new_data1 %>% replace_na(list(mths_since_last_delinq=median(new_data1$mths_since_last_delinq, na.rm=TRUE),
                                          revol_util = median(new_data1$revol_util, na.rm=TRUE),
                                          avg_cur_bal = median(new_data1$avg_cur_bal, na.rm=TRUE),
                                          mths_since_recent_bc = median(new_data1$mths_since_recent_bc, na.rm=TRUE),
                                          mths_since_recent_inq = median(new_data1$mths_since_recent_inq, na.rm=TRUE),
                                          num_rev_accts = median(new_data1$num_rev_accts, na.rm=TRUE),
                                          pct_tl_nvr_dlq = median(new_data1$pct_tl_nvr_dlq, na.rm=TRUE),
                                          mo_sin_old_il_acct=median(new_data1$mo_sin_old_il_acct, na.rm=TRUE) ))
names(colMeans(is.na(new_data1)))[colMeans(is.na(new_data1))>0]
library(ranger)

#Splitting data into 70% training and 30%  testing ratio. 
TRNPROP=0.7
nr<-nrow(new_data1)
trnIndex<-sample(1:nr,size = round(TRNPROP*nr),replace = FALSE)

new_data1Trn<-new_data1[trnIndex,]
new_data1Tst<-new_data1[-trnIndex,]
new_data1Trn<-new_data1Trn%>%select(-c("mths_since_last_record","annual_inc_joint","dti_joint","open_il_12m","mths_since_rcnt_il","total_bal_il","il_util","open_rv_24m","max_bal_bc","all_util","inq_fi","total_cu_tl","inq_last_12m","mths_since_recent_bc_dlq"))

#ran a  random forest based using ranger, splitrule is gini
new_data1T1<-ranger(loan_status~.,data = new_data1Trn,classification = TRUE,num.trees = 200,importance = "permutation",probability = TRUE)
sort(new_data1T1$variable.importance,decreasing = TRUE)

#Making predictions and evaluating performance of the model
#training data 
#predicting values in training data
predtrn<-predict(new_data1T1,new_data1Trn,type = 'response') # type response as a classification 

# we get the predictions of charged off and fully paid loans in the form of probabilities. 
#Next we compare if probability of charged off is greater than fully charged(that is 50% threshold value) then loan is 
#charged off else fully paid
predictions<-ifelse(predtrn$predictions[,"Charged Off"]>predtrn$predictions[,"Fully Paid"],"Charged Off","Fully Paid")

#Performance Evaluation
#creating a confusion matrix
CM<-table(pred=predictions,true=new_data1Trn$loan_status)
CM
mean(predictions==new_data1Trn$loan_status)

# Calculating F1score
precision<-CM[1,1]/(CM[1,1]+CM[1,2])
recall<-CM[1,1]/(CM[1,1]+CM[2,1])
F1<-(2*precision*recall)/(precision+recall)
F1

#testing
predTst<-predict(new_data1T1,new_data1Tst,type = 'response')

#when threshold of charged off >50%
predictions<-ifelse(predTst$predictions[,"Charged Off"]>predTst$predictions[,"Fully Paid"],"Charged Off","Fully Paid")
CM<-table(pred=predictions,true=new_data1Tst$loan_status)
CM
mean(predictions==new_data1Tst$loan_status)
precision=CM[1,1]/(CM[1,1]+CM[1,2])
recall=CM[1,1]/(CM[1,1]+CM[2,1])
F1<-(2*precision*recall)/(precision+recall)
F1

#accuracy for test data is around 93.3%

#altering number of trees in random forest for better precision using a loop
trees.no<-c(1,2,3,4,5,10,15,20,30,40,50,60)
pred<-c()
F1score<-c()
for(i in trees.no)
{
  trial1<-ranger(loan_status~.,data = new_data1Trn,classification = TRUE,
                 num.trees = i,importance='permutation',probability = TRUE)
  predTst<-predict(trial1,new_data1Tst,type='response')
  predictions<-ifelse(predTst$predictions[,"Charged Off"]>predTst$predictions[,"Fully Paid"],"Charged Off","Fully Paid")
  CM<-table(pred=predictions,true=new_data1Tst$loan_status)
  CM
  mean(predictions==new_data1Tst$loan_status)
  precision=CM[1,1]/(CM[1,1]+CM[1,2])
  recall=CM[1,1]/(CM[1,1]+CM[2,1])
  F1<-(2*precision*recall)/(precision+recall)
  pred<-append(pred,(CM[1,1]+CM[2,2])/length(predictions))
  F1score<-append(F1score,F1)
}

glimpse(trees.no)
glimpse(F1score)
plot(trees.no,F1score,main="Number of Trees v/s F1score",
     xlab="Number of Trees",ylab="F1score",pch=19)
plot(trees.no,pred,main="Number of Trees v/s Pred",
     xlab="Number of Trees",ylab="correct prediction%",pch=19)

#we observe that the model has highest accuracy and F1 score when number of trees is 10
new_data1T1<-ranger(loan_status~.,data = new_data1Trn,classification = TRUE,
                    num.trees=10,importance = 'permutation',probability = TRUE)

#testing
#Checking for different threshold values
predTst<-predict(new_data1T1,new_data1Tst,type = 'response')
perc<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)
pred1<-c()
F1score<-c()
for(i in perc){
  predictions<-ifelse(predTst$predictions[,"Charged Off"]>i,"Charged Off","Fully Paid")
  CM<-table(pred=predictions,true=new_data1Tst$loan_status)
  precision<-CM[1,1]/(CM[1,1]+CM[1,2])
  recall<-CM[1,1]/(CM[1,1]+CM[2,1])
  
}
predTst <- predict(new_data1T1, new_data1Tst, type = 'response') # type response as a classification 
perc <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8) # array of threshold values
pred1 <- c()
F1score1 <- c()
predTst <- as.numeric(predTst$predictions[,"Charged Off"])

for (i in perc) { 
  predictions <- ifelse(predTst > i, "Charged Off", "Fully Paid")
  CM <- table(pred = predictions, true = new_data1Tst$loan_status)
  if (nrow(CM) > 1) {
    precision <- CM[1, 1] / (CM[1, 1] + CM[1, 2])
    recall <- CM[1, 1] / (CM[1, 1] + CM[2, 1])
    F1 <- (2 * precision * recall) / (precision + recall)
  } else {
    precision <- NA
    recall <- NA
    F1 <- NA
  }
  pred1 <- append(pred1, (CM[1, 1] + CM[2, 2]) / length(predictions))
  F1score1 <- append(F1score1, F1)
}

plot(perc,pred1,main="Threshold value v/s Prediction",
     xlab="Threshold Value",ylab="Prediction")
plot(perc,F1score1,main="Threshold value v/s F1score",xlab="Threshold value",ylab="F1score")



#Predict ActualReturn
new_data$actual_return<-ifelse(new_data$actualTerm>0,((new_data$total_pymnt-new_data$funded_amnt)/new_data$funded_amnt)*(1/new_data$actualTerm)*100,0)
TRNPROP=0.7
nr<-nrow(new_data)
trnIndex<-sample(1:nr,size = round(TRNPROP*nr),replace = FALSE)

new_data$mths_since_last_delinq[is.na(new_data$mths_since_last_delinq)]<-median(new_data$mths_since_last_delinq,na.rm=TRUE)
new_data$revol_util[is.na(new_data$revol_util)]<-median(new_data$revol_util,na.rm=TRUE)
new_data$avg_cur_bal[is.na(new_data$avg_cur_bal)]<-median(new_data$avg_cur_bal,na.rm=TRUE)
new_data$mo_sin_old_il_acct[is.na(new_data$mo_sin_old_il_acct)]<-median(new_data$mo_sin_old_il_acct,na.rm=TRUE)
new_data$mths_since_recent_bc[is.na(new_data$mths_since_recent_bc)]<-median(new_data$mths_since_recent_bc,na.rm=TRUE)
new_data$mths_since_recent_inq[is.na(new_data$mths_since_recent_inq)]<-median(new_data$mths_since_recent_inq,na.rm=TRUE)
new_data$num_rev_accts[is.na(new_data$num_rev_accts)]<-median(new_data$num_rev_accts,na.rm=TRUE)
new_data$pct_tl_nvr_dlq[is.na(new_data$pct_tl_nvr_dlq)]<-median(new_data$pct_tl_nvr_dlq,na.rm=TRUE)


new_dataTrn<-new_data[trnIndex,]
new_dataTst<-new_data[-trnIndex,]

rfModel_Ret<-ranger(actual_return~.,data = subset(new_dataTrn,select = -c(Ann_Ret,actualTerm,loan_status,mths_since_last_record, annual_inc_joint, dti_joint, open_il_12m, mths_since_rcnt_il, total_bal_il, il_util, open_rv_24m, max_bal_bc, all_util, inq_fi, total_cu_tl, inq_last_12m, mths_since_recent_bc_dlq),num.trees=200,importance='permutation'))
rfPredRet_Trn<-predict(rfModel_Ret,new_dataTrn)
sqrt(mean(rfPredRet_Trn$predictions-new_dataTrn$actual_return)^2)
plot((predict(rfModel_Ret,new_dataTrn))$predictions,new_dataTrn$actual_return)
plot((predict(rfModel_Ret,new_dataTst))$predictions,new_dataTst$actual_return)

lcdf<-read.csv("C:/Users/Prajwal/Downloads/lcDataSampleFall22/lcDataSampleFall22.csv")
head(lcdf)
dim(lcdf)

#Considering only Charged Off and Fully Paid Loans
lcdf<-lcdf%>%filter(loan_status=="Fully Paid"| loan_status=="Charged Off")

#converting last payment received date to a date type variable
lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d,"-01",sep="")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d, "myd")
head(lcdf[,c("last_pymnt_d","issue_d")])

#computing the duration between the two dates in years
lcdf$actualTerm<-ifelse(lcdf$loan_status=="Fully Paid",as.duration(lcdf$issue_d %--% lcdf$last_pymnt_d)/dyears(1),3)

#actual Return with respect to actual Term
lcdf$actualReturn<-ifelse(lcdf$actualTerm>0,((lcdf$total_pymnt-lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm),0)

#Annual Return
lcdf$annRet<-((lcdf$total_pymnt-lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100

#cleaning up the data and remove the unnecessary attributes.
lcdf <- subset(lcdf, select=-c(id, member_id, installment, loan_amnt, earliest_cr_line,mo_sin_old_rev_tl_op,num_actv_bc_tl,num_bc_tl,num_rev_tl_bal_gt_0,num_tl_30dpd,num_tl_90g_dpd_24m,num_tl_op_past_12m,pct_tl_nvr_dlq, initial_list_status,funded_amnt_inv, term, pymnt_plan, inq_last_6mths, mths_since_last_delinq, mths_since_last_record,revol_bal,revol_util, total_acc, out_prncp, out_prncp_inv, total_pymnt_inv,  total_rec_prncp,total_rec_int, total_rec_late_fee,recoveries, collection_recovery_fee, last_pymnt_amnt, next_pymnt_d, last_credit_pull_d,tot_coll_amt,  hardship_flag, hardship_type, hardship_reason, hardship_status, deferral_term, hardship_amount, hardship_start_date, hardship_end_date, payment_plan_start_date, hardship_length, hardship_dpd, hardship_loan_status, orig_projected_additional_accrued_interest, hardship_payoff_balance_amount, hardship_last_payment_amount, disbursement_method, debt_settlement_flag,total_il_high_credit_limit, debt_settlement_flag_date, settlement_status, settlement_date, settlement_amount, settlement_percentage, settlement_term, funded_amnt_inv, term, emp_title, url,	desc, title, zip_code, addr_state, mths_since_last_record,	mths_since_last_major_derog,	policy_code,	application_type, annual_inc_joint,	dti_joint,	verification_status_joint,	open_acc_6m,	open_act_il,	open_il_12m,	open_il_24m, mths_since_rcnt_il,	total_bal_il,	il_util,	open_rv_12m,	open_rv_24m,	max_bal_bc,	all_util,	inq_fi,	total_cu_tl, inq_last_12m,	mths_since_recent_bc_dlq,	mths_since_recent_revol_delinq,	revol_bal_joint,	sec_app_earliest_cr_line, sec_app_inq_last_6mths,	sec_app_mort_acc,	sec_app_open_acc,	sec_app_revol_util,	sec_app_open_act_il, sec_app_num_rev_accts, sec_app_chargeoff_within_12_mths,	sec_app_collections_12_mths_ex_med,	sec_app_mths_since_last_major_derog, emp_title,emp_length, inq_last_6mths,revol_bal, title, zip_code, addr_state, out_prncp, out_prncp, out_prncp_inv,recoveries, collection_recovery_fee, total_acc, last_pymnt_d, last_pymnt_amnt, last_credit_pull_d, tot_coll_amt, total_rev_hi_lim,total_bc_limit, avg_cur_bal,bc_open_to_buy,bc_util,chargeoff_within_12_mths,mths_since_recent_bc, mo_sin_old_il_acct,num_tl_120dpd_2m, percent_bc_gt_75, mths_since_recent_inq, num_rev_accts))
dim(lcdf)
glimpse(lcdf)

#Ensure factor
lcdf[sapply(lcdf,is.character)]<-lapply(lcdf[sapply(lcdf,is.character)],as.factor)
sapply(lcdf,class)

#splitting the data into training and test. 
TRNPROP = 0.7  #proportion of examples in the training sample
nr<-nrow(lcdf)
trnIndex<- sample(1:nr, size = round(TRNPROP * nr), replace=FALSE)
lcdfTrn <- lcdf[trnIndex, ]
lcdfTst <- lcdf[-trnIndex, ]
library(xgboost)

fdum<-dummyVars(~.,data=lcdf %>% select(-loan_status)) #do not include loan_status for this
dxlcdf <- predict(fdum,lcdf)

# for loan_status, check levels and convert to dummy vars and keep the class label of interest
levels(lcdf$loan_status)
dylcdf <- class2ind(lcdf$loan_status, drop2nd = FALSE)
colcdf <- dylcdf [ , 1] #(fullyPaid or chargedOff as label of interest)

#Training, test subsets
nr<-nrow(lcdf)
trnIndex<-sample(1:nr,size=round(0.7*nr),replace = FALSE)
dxlcdfTrn<-dxlcdf[trnIndex,]
colcdfTrn<-colcdf[trnIndex]
dxlcdfTst<-dxlcdf[-trnIndex,]
colcdfTst<-colcdf[-trnIndex]
dxTrn<-xgb.DMatrix(subset(dxlcdfTrn,select = -c(actualTerm,actualReturn)),label=colcdfTrn)
dxTst<-xgb.DMatrix(subset(dxlcdfTst,select = -c(actualTerm,actualReturn)),label=colcdfTst)

xgbWatchlist<-list(train=dxTrn,eval=dxTst)
#we can watch the progress of learning through performance on these datasets

#list of parameters for the xgboost model development functions
xgbParam <- list (
  max_depth=5,eta=0.01,
  objective="binary:logistic",
  eval_metric = "error",eval_metric="auc")

#can specify which evaluation metrics we want to watch
xgb_1sM1<-xgb.train(xgbParam,dxTrn,nrounds = 500,xgbWatchlist,early_stopping_rounds = 10)
xgb_1sM1$best_iteration
xpredTrg<-predict(xgb_1sM1,dxTrn)
head(xpredTrg)

#use cross-validation on training dataset to determine best model 
xgbParam<-list(max_depth=3,eta=0.1,objective="binary:logistic",eval_metric="error",eval_metric="auc")

xgb_lscv<-xgb.cv(xgbParam,dxTrn,nfold = 5,nrounds=500,early_stopping_rounds = 10)
xgb_lscv$best_iteration

# or for the best iteration based on performance measure (among those specified in xgbParam)
best_cvIter<-which.max(xgbParam$evaluation_log$test_auc_mean)

#learn the best model without xval
xgb_lsbest<-xgb.train(xgbParam,dxTrn,nrounds = xgb_lscv$best_iteration)

#Variable Importance
xgb.importance(model=xgb_lsbest)%>%view()

xgbParam<-list(max_depth=4,objective="binary:logistic",eval_metric="error",eval_metric="auc")
xgb_1sM1<-xgb.train(xgbParam,dxTrn,nrounds=500,xgbWatchlist,early_stopping_rounds = 10,eta=1)

xgbParam<-list(
   max_depth = 4,
   objective="binary:logistic",
   eval_metric=c("auc","error")
  )

xgb_1sM1<-xgb.train(xgbParam,dxTrn,nrounds=500,xgbWatchlist,early_stopping_rounds = 10,eta=0.5)
xgb_1sM1<-xgb.train(xgbParam,dxTrn,nrounds=500,xgbWatchlist,early_stopping_rounds = 10,eta=0.1)
xgb_1sM1<-xgb.train(xgbParam,dxTrn,nrounds=500,xgbWatchlist,early_stopping_rounds = 10,eta=0.5)

##hyper-parameters---- 
xgbParamGrid<-expand.grid(
  max_depth=c(3,5,8),
  eta=c(0.01,0.1,0.5)
)

for(i in 1:nrow(xgbParamGrid)) {
  xgb_tune<-xgboost(data = dxTrn,objective="reg:squarederror",nrounds=50,eta=xgbParamGrid$eta[i],
                    max_depth=xgbParamGrid$max_depth[i],early_stopping_rounds = 8)
  xgbParamGrid$bestTree[i]<-xgb_tune$evaluation_log[xgb_tune$best_iteration]$iter
  xgbParamGrid$bestPerf[i]<-xgb_tune$evaluation_log[xgb_tune$best_iteration]$train_rmse
}

xgParam<-list(
  booster="gbtree",
  objective="reg:squarederror",
  eta=0.001,
  max_depth=2,
  min_child_weight=1,
  colsample_bytree=0.6
  )

xgb_1sM1<-xgb.train(xgbParam,dxTrn,nrounds=500,
                    xgbWatchlist,early_stopping_rounds = 10)

#AUC
#train data
table(pred=as.numeric(xpredTrg>0.5),act=colcdfTrn)

#test data
xpredTst<-predict(xgb_1sM1,dxTst,type='class')
table(pred=as.numeric(xpredTst>0.5),act=colcdfTst)
length(xpredTst)
length(colcdfTst)

pred_xgb_1sM1<-prediction(xpredTst,lcdfTst$loan_status,label.ordering = c('Charged Off','Fully Paid'))
aucPerf_xgb_1sM1<-performance(pred_xgb_1sM1,"tpr","fpr")
plot(aucPerf_xgb_1sM1)
abline(a=0,b=1)

## Model 2
xgbParam<-list(max_depth=3,eta=0.1,objective="binary:logistic",eval_metric="error",eval_metric="auc")
xgb_1sM1<-xgb.train(xgbParam,dxTrn,nrounds = 500,xgbWatchlist,early_stopping_rounds = 10)
xgb_1sM1$best_iteration
xpredTrg<-predict(xgb_1sM1,dxTrn)
head(xpredTrg)
xpredTst<-predict(xgb_1sM1,dxTst)
xgb_1sM1

library(ROCR)

#Train Data
table(pred=as.numeric(xpredTrg>0.5),act=colcdfTrn)
table(pred=as.numeric(xpredTst>0.5),act=colcdfTst)

xpredTst<-predict(xgb_1sM1,dxTst)
pred_xgb_1sM1<-prediction(xpredTst,lcdfTst$loan_status,label.ordering = c("Charged Off","Fully Paid"))

aucPerf_xgb_1sM1<-performance(pred_xgb_1sM1,"tpr","fpr")
plot(aucPerf_xgb_1sM1)
abline(a=0,b=1)
