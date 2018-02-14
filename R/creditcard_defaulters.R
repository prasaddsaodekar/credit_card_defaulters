# rm(list=ls())
library(plyr)
library(dplyr)
library(ggplot2)
library(ROCR)
library(caret)
library(vcd)

options(scipen = 1000000000)

cc_train <- read.csv("C:/Users/psaodekarx092757/Downloads/CTS/ML_201/CreditCards/default_of_credit_card_clients_TRAIN.csv")

colnames(cc_train) <- as.character(unlist(cc_train[1,]))
cc_train <- cc_train[-1,]

cc_train <- cc_train %>% rename(DEF_PAY_NEXT_MNTH = `default payment next month`, PAY_1 = PAY_0)
colnames(cc_train) <- tolower(colnames(cc_train))
glimpse(cc_train)

colsToNum <- c('limit_bal','bill_amt1','bill_amt2','bill_amt3','bill_amt4','bill_amt5','bill_amt6',
               'pay_1','pay_2','pay_3','pay_4','pay_5','pay_6', 
               'pay_amt1','pay_amt2','pay_amt3','pay_amt4','pay_amt5','pay_amt6')
cc_train[colsToNum] <- lapply(cc_train[colsToNum], function(x) as.numeric(as.character(x)) )
cc_train$age <- as.integer(cc_train$age)
attach(cc_train)
############################################################################

                  ### EDA ###

############################################################################

hist(cc_train$limit_bal)
  # limit_bal is right skewed data

gg <- function(ds,indvar){
  library(ggplot2)
  ggplot(ds,aes_string(indvar)) + 
    geom_histogram(bins = 15) +
    coord_cartesian(xlim = c( 0,450000 )) +
    ggtitle(paste0("Histogram of ",indvar))
}

bill_amt <- paste0('bill_amt',1:6)
for(i in 1:length(bill_amt)){
  var <- bill_amt[i]
  var_new <- paste0('gg_',bill_amt[i])
  plt <<- gg(cc_train,var)
  assign(var_new,plt)
  rm(plt)
}
do.call(grid.arrange, lapply( paste0('gg_bill_amt',1:6), get) )
    ### From plots, see all bill_amts are right skewed
##########################################

pay_amt <- paste0('pay_amt',1:6)
for(i in 1:length(pay_amt)){
  var <- pay_amt[i]
  var_new <- paste0('gg_',pay_amt[i])
  plt <<- gg(cc_train,var)
  assign(var_new,plt)
  rm(plt)
}
do.call(grid.arrange, lapply( paste0('gg_pay_amt',1:6), get) )
    ### From plots, see all pay_amts are right skewed
##########################################

# rm(list = c(paste0('gg_pay',1:6)))
gg_pay1 <- ggplot(cc_train,aes_string( as.numeric(cc_train$pay_1)) ) + geom_histogram() +
  xlab("pay 1") + ggtitle(paste0("Histogram of pay_1"))
gg_pay2 <- ggplot(cc_train,aes_string( as.numeric(cc_train$pay_2)) ) + geom_histogram() +
  xlab("pay 2") + ggtitle(paste0("Histogram of pay_2"))
gg_pay3 <- ggplot(cc_train,aes_string( as.numeric(cc_train$pay_3)) ) + geom_histogram() +
  xlab("pay 3") + ggtitle(paste0("Histogram of pay_3"))
gg_pay4 <- ggplot(cc_train,aes_string( as.numeric(cc_train$pay_4)) ) + geom_histogram() +
  xlab("pay 4") + ggtitle(paste0("Histogram of pay_4"))
gg_pay5 <- ggplot(cc_train,aes_string( as.numeric(cc_train$pay_5)) ) + geom_histogram() +
  xlab("pay 5") + ggtitle(paste0("Histogram of pay_5"))
gg_pay6 <- ggplot(cc_train,aes_string( as.numeric(cc_train$pay_6)) ) + geom_histogram() +
  xlab("pay 6") + ggtitle(paste0("Histogram of pay_6"))

do.call(grid.arrange, lapply( paste0('gg_pay',1:6), get) )

################################################################################################
################################################################################################

cc_train_prep <- cc_train

log_vars <- c('limit_bal',paste0('pay_amt',1:6),paste0('bill_amt',1:6))
cc_train_prep[log_vars] <- lapply(cc_train_prep[log_vars], function(x) ifelse(x>0,round(log(x),2),0))

### Avgs of Bill_Amt & pay_amt ###
cc_train_prep <- cc_train_prep %>%
  group_by(id) %>%
  mutate(bill_amt_avg = round(sum(bill_amt1,bill_amt2,bill_amt3,bill_amt4,bill_amt5,bill_amt6)/6,2)
         ,pay_amt_avg = round(sum(pay_amt1,pay_amt2,pay_amt3,pay_amt4,pay_amt5,pay_amt6)/6,2))

for(i in 1:length(bill_amt)){
  c <- bill_amt[i]
  c_new <- paste0('rel_',bill_amt[i])
  x = round((cc_train_prep[,c]/cc_train_prep[,'bill_amt_avg']),2)
  cc_train_prep[,c_new] <- x  
  rm(x)
}

for(i in 1:length(pay_amt)){
  c <- pay_amt[i]
  c_new <- paste0('rel_',pay_amt[i])
  x = round((cc_train_prep[,c]/cc_train_prep[,'pay_amt_avg']),2)
  cc_train_prep[,c_new] <- x  
  rm(x)
}
table(cc_train_prep$education)
# detach(cc_train)

cc_train_prep$sex <- factor(cc_train_prep$sex, labels = c('M','F'))
cc_train_prep$marriage <- factor(cc_train_prep$marriage, labels = c('NA','Married','Single','Others'))
cc_train_prep$education <- factor(cc_train_prep$education)
levels(cc_train_prep$education)
cc_train_prep$education <- mapvalues(cc_train_prep$education,
                                     c('0','1','2','3','4','5','6'),
                                     c('Others','Grad School','University','High School','Others','Others','Others'))
cc_train_prep$def_pay_next_mnth <- factor(cc_train_prep$def_pay_next_mnth)
glimpse(cc_train_prep)

# mosaicplot(def_pay_next_mnth ~ age_bkt, data = cc_train_prep, main="Def Pay - Age", shade=TRUE)
mosaicplot(def_pay_next_mnth ~ sex, data = cc_train_prep, main="Def Pay - Sex", shade=TRUE)
mosaicplot(def_pay_next_mnth ~ education, data = cc_train_prep, main="Def Pay - Education", shade=TRUE)
mosaicplot(def_pay_next_mnth ~ marriage, data = cc_train_prep, main="Def Pay - Marriage", shade=TRUE)

round(prop.table(table(cc_train_prep$education,cc_train_prep$def_pay_next_mnth),1),2)
##########################

gg_log <- function(ds,indvar){
  library(ggplot2)
  ggplot(ds,aes_string(indvar)) + 
    geom_histogram(bins = 15) +
    coord_cartesian(xlim = c( 0,30 )) +
    ggtitle(paste0("Histogram of Log of ",indvar))
}
bill_log_amt <- paste0('bill_log_amt',1:6)
for(i in 1:length(bill_log_amt)){
  var <- bill_log_amt[i]
  var_new <- paste0('gg_log_',bill_log_amt[i])
  plt <<- gg_log(cc_train_prep,var)
  assign(var_new,plt)
  rm(plt)
}
do.call(grid.arrange, lapply( paste0('gg_log_bill_amt',1:6), get) )
    ### From plots, see its not skewed
##########################################

        ### Plot limit_bal & monthly bill_amts/pay_amts on 1 plot
limit_bal_sc <- scale(cc_train_prep$limit_bal)
rel_bill_amt1_sc <- scale(cc_train_prep$rel_bill_amt1)

ggplot()+
  # geom_density(aes(limit_bal_sc),bins = 50)
  geom_density(aes(rel_bill_amt1_sc),bins = 50,col="dark green")
##########################################

pay_log_amt <- paste0('pay_log_amt',1:6)
for(i in 1:length(pay_log_amt)){
  var <- pay_log_amt[i]
  var_new <- paste0('gg_log_',pay_log_amt[i])
  plt <<- gg_log(cc_train_prep,var)
  assign(var_new,plt)
  rm(plt)
}
do.call(grid.arrange, lapply( paste0('gg_log_pay_amt',1:6), get) )
###################################################################################

summary(cc_train_prep)

## NANs introduced due to division by bill_amt_avg / pay_amt_avg; which is zero.
character.NAN <- names(which(colSums(is.na(cc_train_prep))>0))
cc_train_prep[character.NAN] <- lapply(cc_train_prep[character.NAN], function(x) replace(x,is.na(x),0))

cc_train_prep$marriage <- mapvalues(cc_train_prep$marriage,
                                     c('NA','Married','Single','Others'),
                                     c('Others','Married','Single','Others'))

####################################################################################
####################################################################################
####################################################################################

cc_train_model <- cc_train_prep %>%
  select(limit_bal,sex,education,marriage,age_bkt,paste0('pay_',1:6),
         paste0('rel_bill_amt',1:6),paste0('rel_pay_amt',1:6),def_pay_next_mnth)
cc_train_model$id <- NULL

summary(cc_train_model)

#####################################

# detach(cc_train_prep)
cc_train_model_LR <- cc_train_model

attach(cc_train_model_LR)
# rm(fit.train)
fit.train <- glm(data=cc_train_model_LR, formula = def_pay_next_mnth ~ ., 
                 family=binomial(link=logit))
summary(fit.train)

cc_train_model_LR$pred_vals <- fit.train$fitted.values
cc_train_model_LR$pred <- ifelse(cc_train_model_LR$pred_vals <= 0.5, 0, 1)

cc_train_model_LR_cf <- table(cc_train_model_LR$def_pay_next_mnth, cc_train_model_LR$pred)
caret::confusionMatrix(cc_train_model_LR_cf)

plot(fit.train, col="green", lwd=2, main="ROC Curve for Logistic: Training Dataset")

conf_matrix_train_LR <- roc(cc_train_model_LR$def_pay_next_mnth, cc_train_model_LR$pred)
plot(roc(cc_train_model_LR$def_pay_next_mnth, cc_train_model_LR$pred),
     main = "CC Train - LR")
conf_matrix_train_LR$auc  # 0.605

# > confusionMatrix(cc_train_model_LR$def_pay_next_mnth, cc_train_model_LR$pred)
#     0    1
# 0 16523 3402
# 1   889 1686

# cc_train_model_LR_evals <- cc_train_model_LR[c(388,1645,22084,832,650,2363),]

######################################################################
######################################################################

library(Information)
library(InformationValue)
library(gridExtra)
## Creating buckets using IV
# rm(cc_train_model_LR_IV)
cc_train_model_LR_IV <- cbind(cc_train_model,'age' = cc_train$age)

# cc_train_model_LR_IV$def_pay_next_mnth <- mapvalues(cc_train_model_LR_IV$def_pay_next_mnth,
#                                                     c(1,2),
#                                                     c(0,1))
cc_train_model_LR_IV$def_pay_next_mnth <- as.numeric(cc_train_model_LR_IV$def_pay_next_mnth)
str(cc_train_model_LR_IV$def_pay_next_mnth)

### Ranking variables using penalized IV  
IV <- create_infotables(data = cc_train_model_LR_IV,
                   # valid = valid,
                   y="def_pay_next_mnth")

IV$Summary
# IV[["Tables"]][["age"]]
MultiPlot(IV, IV$Summary$Variable[1:9])

cc_train_model_LR_IV <- cc_train_model_LR_IV %>%
  mutate(pay_1_rng = ifelse( pay_1 <= -2, "lt -2", ifelse( pay_1> -2 & pay_1<= -1, "-2 to -1", ifelse( pay_1> -1 & pay_1<= 0, "-1 to 0", ifelse( pay_1> 0 & pay_1<= 1, "0 to 1", "gt 1")))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>%
  mutate(pay_2_rng = ifelse( pay_2 <= -2, "lt -2", ifelse( pay_2> -2 & pay_2<= -1, "-2 to -1", ifelse( pay_2> -1 & pay_2<= 1, "-1 to 1", "gt 1"))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>%
  mutate(pay_3_rng = ifelse( pay_3 <= -2, "lt -2", ifelse( pay_3> -2 & pay_3<= -1, "-2 to -1", ifelse( pay_3> -1 & pay_3<= 1, "-1 to 1", "gt 1"))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>%
  mutate(pay_4_rng = ifelse( pay_4 <= -2, "lt -2", ifelse( pay_4> -2 & pay_4<= -1, "-2 to -1", ifelse( pay_4> -1 & pay_4<= 1, "-1 to 1", "gt 1"))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>%
  mutate(pay_5_rng = ifelse( pay_5 <= -2, "lt -2", ifelse( pay_5> -2 & pay_5<= -1, "-2 to -1", ifelse( pay_5> -1 & pay_5<= 1, "-1 to 1", "gt 1"))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>%
  mutate(pay_6_rng = ifelse( pay_6 <= -2, "lt -2", ifelse( pay_6> -2 & pay_6<= -1, "-2 to -1", ifelse( pay_6> -1 & pay_6<= 1, "-1 to 1", "gt 1"))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_pay_amt1_rng =
ifelse(rel_pay_amt1<=0.85,"lt 0.85",
ifelse( rel_pay_amt1> 0.85 & rel_pay_amt1<= 0.98, "0.85 to 0.98",
ifelse( rel_pay_amt1> 0.98 & rel_pay_amt1<= 1, "0.98 to 1",
ifelse( rel_pay_amt1> 1 & rel_pay_amt1<= 1.03, "1 to 1.03",
ifelse( rel_pay_amt1> 1.03 & rel_pay_amt1<= 1.08, "1.03 to 1.08",
ifelse( rel_pay_amt1> 1.08 & rel_pay_amt1<= 1.17, "1.08 to 1.17",
ifelse( rel_pay_amt1> 1.17 & rel_pay_amt1<= 1.3, "1.17 to 1.3",
ifelse( rel_pay_amt1> 1.3 & rel_pay_amt1<= 1.6, "1.3 to 1.6",
"gt 1.6")))))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_pay_amt2_rng =
ifelse(rel_pay_amt2<=0.83,"lt 0.83",
ifelse( rel_pay_amt2> 0.83 & rel_pay_amt2<= 0.97, "0.83 to 0.97",
ifelse( rel_pay_amt2> 0.97 & rel_pay_amt2<= 1, "0.97 to 1",
ifelse( rel_pay_amt2> 1 & rel_pay_amt2<= 1.02, "1 to 1.02",
ifelse( rel_pay_amt2> 1.02 & rel_pay_amt2<= 1.06, "1.02 to 1.06",
ifelse( rel_pay_amt2> 1.06 & rel_pay_amt2<= 1.16, "1.06 to 1.16",
ifelse( rel_pay_amt2> 1.16 & rel_pay_amt2<= 1.28, "1.16 to 1.28",
ifelse( rel_pay_amt2> 1.28 & rel_pay_amt2<= 1.57, "1.28 to 1.57",
"gt 1.57")))))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_pay_amt5_rng =
ifelse(rel_pay_amt5<=0.91,"lt 0.91",
ifelse( rel_pay_amt5> 0.91 & rel_pay_amt5<= 0.96, "0.91 to 0.96",
ifelse( rel_pay_amt5> 0.96 & rel_pay_amt5<= 0.98, "0.96 to 0.98",
ifelse( rel_pay_amt5> 0.98 & rel_pay_amt5<= 1, "0.98 to 1",
ifelse( rel_pay_amt5> 1 & rel_pay_amt5<= 1.07, "1 to 1.07",
ifelse( rel_pay_amt5> 1.07 & rel_pay_amt5<= 1.18, "1.07 to 1.18",
ifelse( rel_pay_amt5> 1.18 & rel_pay_amt5<= 1.42, "1.18 to 1.42",
"gt 1.42"))))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_pay_amt6_rng =
ifelse(rel_pay_amt6<=0.88,"lt 0.88",
ifelse( rel_pay_amt6> 0.88 & rel_pay_amt6<= 0.94, "0.88 to 0.94",
ifelse( rel_pay_amt6> 0.94 & rel_pay_amt6<= 0.97, "0.94 to 0.97",
ifelse( rel_pay_amt6> 0.97 & rel_pay_amt6<= 0.99, "0.97 to 0.99",
ifelse( rel_pay_amt6> 0.99 & rel_pay_amt6<= 1.06, "0.99 to 1.06",
ifelse( rel_pay_amt6> 1.06 & rel_pay_amt6<= 1.17, "1.06 to 1.17",
ifelse( rel_pay_amt6> 1.17 & rel_pay_amt6<= 1.39, "1.17 to 1.39",
"gt 1.39"))))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(limit_bal_rng =
ifelse(limit_bal<=9.9,"lt 9.9",
ifelse( limit_bal> 9.9 & limit_bal<= 10.6, "9.9 to 10.6",
ifelse( limit_bal> 10.6 & limit_bal<= 11, "10.6 to 11",
ifelse( limit_bal> 11 & limit_bal<= 11.41, "11 to 11.41",
ifelse( limit_bal> 11.41 & limit_bal<= 11.7, "11.41 to 11.7",
ifelse( limit_bal> 11.7 & limit_bal<= 11.98, "11.7 to 11.98",
ifelse( limit_bal> 11.98 & limit_bal<= 12.21, "11.98 to 12.21",
ifelse( limit_bal> 12.21 & limit_bal<= 12.43, "12.21 to 12.43",
ifelse( limit_bal> 12.43 & limit_bal<= 12.77, "12.43 to 12.77",
"gt 12.77"))))))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_pay_amt4_rng =
ifelse(rel_pay_amt4<=0.91,"lt 0.91",
ifelse( rel_pay_amt4> 0.91 & rel_pay_amt4<= 0.95, "0.91 to 0.95",
ifelse( rel_pay_amt4> 0.95 & rel_pay_amt4<= 0.98, "0.95 to 0.98",
ifelse( rel_pay_amt4> 0.98 & rel_pay_amt4<= 1, "0.98 to 1",
ifelse( rel_pay_amt4> 1 & rel_pay_amt4<= 1.07, "1 to 1.07",
ifelse( rel_pay_amt4> 1.07 & rel_pay_amt4<= 1.18, "1.07 to 1.18",
ifelse( rel_pay_amt4> 1.18 & rel_pay_amt4<= 1.41, "1.18 to 1.41",
"gt 1.41"))))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_pay_amt3_rng =
ifelse(rel_pay_amt3<=0.93,"lt 0.93",
ifelse( rel_pay_amt3> 0.93 & rel_pay_amt3<= 0.97, "0.93 to 0.97",
ifelse( rel_pay_amt3> 0.97 & rel_pay_amt3<= 0.99, "0.97 to 0.99",
ifelse( rel_pay_amt3> 0.99 & rel_pay_amt3<= 1.02, "0.99 to 1.02",
ifelse( rel_pay_amt3> 1.02 & rel_pay_amt3<= 1.1, "1.02 to 1.1",
ifelse( rel_pay_amt3> 1.1 & rel_pay_amt3<= 1.21, "1.1 to 1.21",
ifelse( rel_pay_amt3> 1.21 & rel_pay_amt3<= 1.48, "1.21 to 1.48",
"gt 1.48"))))))))

###########################
cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_bill_amt1_rng =
ifelse(rel_bill_amt1<=0.79,"lt 0.79",
ifelse( rel_bill_amt1> 0.79 & rel_bill_amt1<= 0.97, "0.79 to 0.97",
ifelse( rel_bill_amt1> 0.97 & rel_bill_amt1<= 0.98, "0.97 to 0.98",
ifelse( rel_bill_amt1> 0.98 & rel_bill_amt1<= 0.99, "0.98 to 0.99",
ifelse( rel_bill_amt1> 0.99 & rel_bill_amt1<= 1, "0.99 to 1",
ifelse( rel_bill_amt1> 1 & rel_bill_amt1<= 1.01, "1 to 1.01",
ifelse( rel_bill_amt1> 1.01 & rel_bill_amt1<= 1.03, "1.01 to 1.03",
ifelse( rel_bill_amt1> 1.03 & rel_bill_amt1<= 1.1, "1.03 to 1.1",
ifelse( rel_bill_amt1> 1.1 & rel_bill_amt1<= 1.39, "1.1 to 1.39",
"gt 1.39"))))))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_bill_amt6_rng =
ifelse(rel_bill_amt6<=0.86,"lt 0.86",
ifelse( rel_bill_amt6> 0.86 & rel_bill_amt6<= 0.94, "0.86 to 0.94",
ifelse( rel_bill_amt6> 0.94 & rel_bill_amt6<= 0.97, "0.94 to 0.97",
ifelse( rel_bill_amt6> 0.97 & rel_bill_amt6<= 0.98, "0.97 to 0.98",
ifelse( rel_bill_amt6> 0.98 & rel_bill_amt6<= 0.99, "0.98 to 0.99",
ifelse( rel_bill_amt6> 0.99 & rel_bill_amt6<= 1, "0.99 to 1",
ifelse( rel_bill_amt6> 1 & rel_bill_amt6<= 1.01, "1 to 1.01",
ifelse( rel_bill_amt6> 1.01 & rel_bill_amt6<= 1, "1.01 to 1",
"gt 1")))))))))

############################
cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_bill_amt5_rng =
ifelse(rel_bill_amt5<=0.91,"lt 0.91",
ifelse( rel_bill_amt5> 0.91 & rel_bill_amt5<= 0.96, "0.91 to 0.96",
ifelse( rel_bill_amt5> 0.96 & rel_bill_amt5<= 0.98, "0.96 to 0.98",
ifelse( rel_bill_amt5> 0.98 & rel_bill_amt5<= 0.99, "0.98 to 0.99",
ifelse( rel_bill_amt5> 0.99 & rel_bill_amt5<= 1, "0.99 to 1",
ifelse( rel_bill_amt5> 1 & rel_bill_amt5<= 1.01, "1 to 1.01",
ifelse( rel_bill_amt5> 1.01 & rel_bill_amt5<= 1.14, "1.01 to 1.14",
"gt 1.14"))))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_bill_amt4_rng =
ifelse(rel_bill_amt4<=0.94,"lt 0.94",
ifelse( rel_bill_amt4> 0.94 & rel_bill_amt4<= 0.98, "0.94 to 0.98",
ifelse( rel_bill_amt4> 0.98 & rel_bill_amt4<= 0.99, "0.98 to 0.99",
ifelse( rel_bill_amt4> 0.99 & rel_bill_amt4<= 1, "0.99 to 1",
ifelse( rel_bill_amt4> 1 & rel_bill_amt4<= 1.03, "1 to 1.03",
ifelse( rel_bill_amt4> 1.03 & rel_bill_amt4<= 1.18, "1.03 to 1.18",
"gt 1.18")))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_bill_amt2_rng =
ifelse(rel_bill_amt2<=0.97,"lt 0.97",
ifelse( rel_bill_amt2> 0.97 & rel_bill_amt2<= 0.99, "0.97 to 0.99",
ifelse( rel_bill_amt2> 0.99 & rel_bill_amt2<= 1, "0.99 to 1",
ifelse( rel_bill_amt2> 1 & rel_bill_amt2<= 1.02, "1 to 1.02",
ifelse( rel_bill_amt2> 1.02 & rel_bill_amt2<= 1.06, "1.02 to 1.06",
ifelse( rel_bill_amt2> 1.06 & rel_bill_amt2<= 1.25, "1.06 to 1.25",
"gt 1.25")))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(rel_bill_amt3_rng =
ifelse(rel_bill_amt3<=0.97,"lt 0.97",
ifelse( rel_bill_amt3> 0.97 & rel_bill_amt3<= 0.99, "0.97 to 0.99",
ifelse( rel_bill_amt3> 0.99 & rel_bill_amt3<= 1, "0.99 to 1",
ifelse( rel_bill_amt3> 1 & rel_bill_amt3<= 1.01, "1 to 1.01",
ifelse( rel_bill_amt3> 1.01 & rel_bill_amt3<= 1.05, "1.01 to 1.05",
ifelse( rel_bill_amt3> 1.05 & rel_bill_amt3<= 1.2, "1.05 to 1.2",
"gt 1.2")))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(age_rng =
ifelse(age<=4,"lt 4",
ifelse( age> 4 & age<= 6, "4 to 6",
ifelse( age> 6 & age<= 8, "6 to 8",
ifelse( age> 8 & age<= 10, "8 to 10",
ifelse( age> 10 & age<= 12, "10 to 12",
ifelse( age> 12 & age<= 15, "12 to 15",
ifelse( age> 15 & age<= 18, "15 to 18",
ifelse( age> 18 & age<= 22, "18 to 22",
ifelse( age> 22 & age<= 28, "22 to 28",
"gt 28"))))))))))

cc_train_model_LR_IV <- cc_train_model_LR_IV %>% mutate(
education_rng = education
, sex_rng = sex
, marriage_rng = marriage
)

cc_train_model_LR_IV <- cc_train_model_LR_IV %>%
  ungroup() %>%
  select(pay_1_rng,pay_2_rng,pay_3_rng,pay_4_rng,pay_5_rng,pay_6_rng,       
         rel_pay_amt1_rng,rel_pay_amt2_rng,rel_pay_amt5_rng, 
         rel_pay_amt6_rng,limit_bal_rng,rel_pay_amt4_rng, 
         rel_pay_amt3_rng,rel_bill_amt5_rng,rel_bill_amt4_rng,
         rel_bill_amt2_rng,rel_bill_amt3_rng,education_rng,    
         age_rng,sex_rng,marriage_rng,def_pay_next_mnth)

# cc_train_model_LR_IV$def_pay_next_mnth <- mapvalues(cc_train_model_LR_IV$def_pay_next_mnth,
#                                                     c(1,2),
#                                                     c(0,1))

cc_train_model_LR_IV[colnames(cc_train_model_LR_IV)] <- 
  lapply(cc_train_model_LR_IV[colnames(cc_train_model_LR_IV)], function(x) as.factor(x) )
glimpse(cc_train_model_LR_IV)
############################################################

fit.train_IV <- glm(data=cc_train_model_LR_IV,
                    formula = def_pay_next_mnth ~ ., 
                    family=binomial(link=logit))
summary(fit.train_IV)

cc_train_model_LR_IV$pred_vals <- fit.train_IV$fitted.values
cc_train_model_LR_IV$pred <- ifelse(cc_train_model_LR_IV$pred_vals <= 0.5, 0, 1)

cc_train_model_LR_IV_cf <- table(cc_train_model_LR_IV$def_pay_next_mnth, cc_train_model_LR_IV$pred)
caret::confusionMatrix(cc_train_model_LR_IV_cf)


#       0     1   Sum
# 0   16560   852 17412
# 1    3265  1823  5088
# Sum 19825  2675 22500

conf_matrix_train_LR_IV <- roc(cc_train_model_LR_IV$def_pay_next_mnth, cc_train_model_LR_IV$pred)
plot(roc(cc_train_model_LR_IV$def_pay_next_mnth, cc_train_model_LR_IV$pred),
     main = "CC Train - LR w IV")
conf_matrix_train_LR_IV$auc #.655

###################################
    ### auc on Training dataset improved by > 8% ###
###################################

######################################################################
######################################################################
      
        ###Check using IV with other dataset

######################################################################
######################################################################

# rm(fit.train_IV2)
cc_train_model_LR_IV2 <- cc_train_model_LR_IV

fit.train_IV2 <- glm(data=cc_train_model_LR_IV2,
  formula = def_pay_next_mnth ~ 
  pay_1_rng + pay_2 + pay_3_rng + pay_5_rng + pay_6_rng + rel_pay_amt1_rng + 
    limit_bal_rng + rel_bill_amt5_rng + rel_bill_amt4_rng + rel_bill_amt2_rng +
    rel_bill_amt3_rng + education_rng + sex_rng,
  family=binomial(link=logit))
summary(fit.train_IV2)

cc_train_model_LR_IV2$pred_vals <- fit.train_IV2$fitted.values
cc_train_model_LR_IV2$pred <- ifelse(cc_train_model_LR_IV2$pred_vals <= 0.5, 0, 1)

t2 <- addmargins(table(cc_train_model_LR_IV2$def_pay_next_mnth, cc_train_model_LR_IV2$pred))
(t2[1]+t2[5])/t2[9]
  # 0.82

#       0     1   Sum
# 0   16561   851 17412
# 1    3280  1808  5088
# Sum 19841  2659 22500

plot(fit.train_IV2, col="green", lwd=2, main="ROC Curve for Logistic: Training Dataset w IV2")

conf_matrix_train_LR_IV2 <- roc(cc_train_model_LR_IV2$def_pay_next_mnth, cc_train_model_LR_IV2$pred)
plot(roc(cc_train_model_LR_IV2$def_pay_next_mnth, cc_train_model_LR_IV2$pred),
     main = "CC Train - LR w IV2")
conf_matrix_train_LR_IV2$auc #.653

######################################################################
######################################################################

            ### Using Random Forest on IV data ###

######################################################################
######################################################################

cc_train_model_IV_RF <- cc_train_model_LR_IV[,c(1:22)]

library(randomForest)
attach(cc_train_model_IV_RF)

## Calling syntax to build the Random Forest
RF0 <- randomForest(as.factor(def_pay_next_mnth) ~ ., data = cc_train_model_IV_RF, 
                   ntree = 3, 
                   mtry = 6, 
                   nodesize = 10,
                   importance=TRUE)
print(RF0)
cc_train_model_IV_RF$pred_values0 <- RF0$predicted
cc_train_model_IV_RF$pred0 <- ifelse(cc_train_model_IV_RF$pred_values0 <= 0.5, 0, 1)
addmargins(table(RF0$y,RF0$predicted))

RF <- randomForest(as.factor(def_pay_next_mnth) ~ ., data = cc_train_model_IV_RF, 
                   ntree = 50, 
                   mtry = 6, 
                   nodesize = 10,
                   importance=TRUE)
print(RF)
addmargins(table(RF$y,RF$predicted))


# OOB estimate of  error rate: 19.22%
# Confusion matrix:
#   0    1 class.error
# 0 16315 1097  0.06300253
# 1  3227 1861  0.63423742

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3, bty = "n")
title(main="Error Rates RF Train Dataset")

RF2 <- randomForest(def_pay_next_mnth ~ ., data = cc_train_model_IV_RF, 
                    ntree = 80, 
                    mtry = 6, 
                    nodesize = 5,
                    importance=TRUE)
print(RF2)

# OOB estimate of  error rate: 19.12%
# Confusion matrix:
#   0    1 class.error
# 0 16304 1108  0.06363428
# 1  3195 1893  0.62794811


      ### With random forest; False Positives are more. 
      ### Need model with lesser FPs (since concerned about FPs to be less)

###############################################################################
###############################################################################
###############################################################################

        ###Implementing both IV models on Test dataset###

###############################################################################
###############################################################################
###############################################################################

cc_test <- read.csv("C:/Users/psaodekarx092757/Downloads/CTS/ML_201/CreditCards/default_of_credit_card_clients_TEST.csv")

colnames(cc_test) <- as.character(unlist(cc_test[1,]))
cc_test <- cc_test[-1,]

cc_test <- cc_test %>% rename(PAY_1 = PAY_0)
colnames(cc_test) <- tolower(colnames(cc_test))
glimpse(cc_test)

colsToNumTest <- c('limit_bal','bill_amt1','bill_amt2','bill_amt3','bill_amt4','bill_amt5','bill_amt6',
                   'pay_1','pay_2','pay_3','pay_4','pay_5','pay_6', 
                   'pay_amt1','pay_amt2','pay_amt3','pay_amt4','pay_amt5','pay_amt6')
cc_test[colsToNumTest] <- lapply(cc_test[colsToNumTest], function(x) as.numeric(as.character(x)) )
cc_test$age <- as.integer(cc_test$age)

################################################################################################
################################################################################################

cc_test_prep <- cc_test

# orig_vars <- c('limit_bal',paste0('pay_amt',1:6),paste0('bill_amt',1:6))  
# log_vars <- c('limit_bal_log',paste0('pay_amt',1:6,'_log'),paste0('bill_amt',1:6,'_log'))
cc_test_prep[log_vars] <- lapply(cc_test_prep[log_vars], function(x) ifelse(x>0,round(log(x),2),0))

### Avgs of Bill_Amt & pay_amt ###
cc_test_prep <- cc_test_prep %>%
  group_by(id) %>%
  mutate(bill_amt_avg = round(sum(bill_amt1,bill_amt2,bill_amt3,bill_amt4,bill_amt5,bill_amt6)/6,2)
         ,pay_amt_avg = round(sum(pay_amt1,pay_amt2,pay_amt3,pay_amt4,pay_amt5,pay_amt6)/6,2))

for(i in 1:length(bill_amt)){
  c <- bill_amt[i]
  c_new <- paste0('rel_',bill_amt[i])
  x = round((cc_test_prep[,c]/cc_test_prep[,'bill_amt_avg']),2)
  cc_test_prep[,c_new] <- x  
  rm(x)
}

for(i in 1:length(pay_amt)){
  c <- pay_amt[i]
  c_new <- paste0('rel_',pay_amt[i])
  x = round((cc_test_prep[,c]/cc_test_prep[,'pay_amt_avg']),2)
  cc_test_prep[,c_new] <- x  
  rm(x)
}
table(cc_test_prep$education)
# detach(cc_test)
attach(cc_test_prep)
cc_test_prep$sex <- factor(cc_test_prep$sex, labels = c('M','F'))
cc_test_prep$marriage <- factor(cc_test_prep$marriage, labels = c('NA','Married','Single','Others'))
cc_test_prep$education <- factor(cc_test_prep$education)
levels(cc_test_prep$education)
cc_test_prep$education <- mapvalues(cc_test_prep$education,
                                    c('0','1','2','3','4','5','6'),
                                    c('Others','Grad School','University','High School','Others','Others','Others'))
# cc_test_prep$age_bkt <- cut(cc_test_prep$age, breaks=c(0,10,20,30,40,50,60),
# labels=c("0--10","11--20","21--30","31--40","41--50","51--60"))
# cc_test_prep$age <- NULL
# cc_test_prep$def_pay_next_mnth <- factor(cc_test_prep$def_pay_next_mnth)

glimpse(cc_test_prep)
summary(cc_test_prep)

## NANs introduced due to division by bill_amt_avg / pay_amt_avg; which is zero.
character.NAN.Test <- names(which(colSums(is.na(cc_test_prep))>0))
cc_test_prep[character.NAN.Test] <- lapply(cc_test_prep[character.NAN.Test], function(x) replace(x,is.na(x),0))

cc_test_prep$marriage <- mapvalues(cc_test_prep$marriage,
                                   c('NA','Married','Single','Others'),
                                   c('Others','Married','Single','Others'))

####################################################################################
####################################################################################
####################################################################################

cc_test_model <- cc_test_prep %>%
  select(limit_bal,sex,education,marriage,age_bkt,paste0('pay_',1:6),
         paste0('rel_bill_amt',1:6),paste0('rel_pay_amt',1:6),def_pay_next_mnth)
cc_test_model$id <- NULL

summary(cc_test_model)

# detach(cc_test_prep)
cc_test_model_LR <- cc_test_model
cc_test_model_LR$def_pay_next_mnth <- NULL

attach(cc_test_model_LR)

cc_test_model_LR$pred_vals <- predict(fit.train, newdata = cc_test_model_LR, type = "response")
# cc_test_model_LR$pred_vals <- fit.train$fitted.values
cc_test_model_LR$pred <- ifelse(cc_test_model_LR$pred_vals <= 0.5, 0, 1)

######################################################################
######################################################################

cc_test_model_LR_IV <- cc_test_model

# # cc_test_model_LR_IV$def_pay_next_mnth <- mapvalues(cc_test_model_LR_IV$def_pay_next_mnth,
# #                                                     c(1,2),
# #                                                     c(0,1))
# cc_test_model_LR_IV$def_pay_next_mnth <- as.numeric(cc_test_model_LR_IV$def_pay_next_mnth)
# str(cc_test_model_LR_IV$def_pay_next_mnth)

cc_test_model_LR_IV <- cc_test_model_LR_IV %>%
  mutate(pay_1_rng = ifelse( pay_1 <= -2, "lt -2", ifelse( pay_1> -2 & pay_1<= -1, "-2 to -1", ifelse( pay_1> -1 & pay_1<= 0, "-1 to 0", ifelse( pay_1> 0 & pay_1<= 1, "0 to 1", "gt 1")))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>%
  mutate(pay_2_rng = ifelse( pay_2 <= -2, "lt -2", ifelse( pay_2> -2 & pay_2<= -1, "-2 to -1", ifelse( pay_2> -1 & pay_2<= 1, "-1 to 1", "gt 1"))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>%
  mutate(pay_3_rng = ifelse( pay_3 <= -2, "lt -2", ifelse( pay_3> -2 & pay_3<= -1, "-2 to -1", ifelse( pay_3> -1 & pay_3<= 1, "-1 to 1", "gt 1"))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>%
  mutate(pay_4_rng = ifelse( pay_4 <= -2, "lt -2", ifelse( pay_4> -2 & pay_4<= -1, "-2 to -1", ifelse( pay_4> -1 & pay_4<= 1, "-1 to 1", "gt 1"))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>%
  mutate(pay_5_rng = ifelse( pay_5 <= -2, "lt -2", ifelse( pay_5> -2 & pay_5<= -1, "-2 to -1", ifelse( pay_5> -1 & pay_5<= 1, "-1 to 1", "gt 1"))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>%
  mutate(pay_6_rng = ifelse( pay_6 <= -2, "lt -2", ifelse( pay_6> -2 & pay_6<= -1, "-2 to -1", ifelse( pay_6> -1 & pay_6<= 1, "-1 to 1", "gt 1"))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_pay_amt1_rng =
                                                        ifelse(rel_pay_amt1<=0.85,"lt 0.85",
                                                               ifelse( rel_pay_amt1> 0.85 & rel_pay_amt1<= 0.98, "0.85 to 0.98",
                                                                       ifelse( rel_pay_amt1> 0.98 & rel_pay_amt1<= 1, "0.98 to 1",
                                                                               ifelse( rel_pay_amt1> 1 & rel_pay_amt1<= 1.03, "1 to 1.03",
                                                                                       ifelse( rel_pay_amt1> 1.03 & rel_pay_amt1<= 1.08, "1.03 to 1.08",
                                                                                               ifelse( rel_pay_amt1> 1.08 & rel_pay_amt1<= 1.17, "1.08 to 1.17",
                                                                                                       ifelse( rel_pay_amt1> 1.17 & rel_pay_amt1<= 1.3, "1.17 to 1.3",
                                                                                                               ifelse( rel_pay_amt1> 1.3 & rel_pay_amt1<= 1.6, "1.3 to 1.6",
                                                                                                                       "gt 1.6")))))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_pay_amt2_rng =
                                                        ifelse(rel_pay_amt2<=0.83,"lt 0.83",
                                                               ifelse( rel_pay_amt2> 0.83 & rel_pay_amt2<= 0.97, "0.83 to 0.97",
                                                                       ifelse( rel_pay_amt2> 0.97 & rel_pay_amt2<= 1, "0.97 to 1",
                                                                               ifelse( rel_pay_amt2> 1 & rel_pay_amt2<= 1.02, "1 to 1.02",
                                                                                       ifelse( rel_pay_amt2> 1.02 & rel_pay_amt2<= 1.06, "1.02 to 1.06",
                                                                                               ifelse( rel_pay_amt2> 1.06 & rel_pay_amt2<= 1.16, "1.06 to 1.16",
                                                                                                       ifelse( rel_pay_amt2> 1.16 & rel_pay_amt2<= 1.28, "1.16 to 1.28",
                                                                                                               ifelse( rel_pay_amt2> 1.28 & rel_pay_amt2<= 1.57, "1.28 to 1.57",
                                                                                                                       "gt 1.57")))))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_pay_amt5_rng =
                                                        ifelse(rel_pay_amt5<=0.91,"lt 0.91",
                                                               ifelse( rel_pay_amt5> 0.91 & rel_pay_amt5<= 0.96, "0.91 to 0.96",
                                                                       ifelse( rel_pay_amt5> 0.96 & rel_pay_amt5<= 0.98, "0.96 to 0.98",
                                                                               ifelse( rel_pay_amt5> 0.98 & rel_pay_amt5<= 1, "0.98 to 1",
                                                                                       ifelse( rel_pay_amt5> 1 & rel_pay_amt5<= 1.07, "1 to 1.07",
                                                                                               ifelse( rel_pay_amt5> 1.07 & rel_pay_amt5<= 1.18, "1.07 to 1.18",
                                                                                                       ifelse( rel_pay_amt5> 1.18 & rel_pay_amt5<= 1.42, "1.18 to 1.42",
                                                                                                               "gt 1.42"))))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_pay_amt6_rng =
                                                        ifelse(rel_pay_amt6<=0.88,"lt 0.88",
                                                               ifelse( rel_pay_amt6> 0.88 & rel_pay_amt6<= 0.94, "0.88 to 0.94",
                                                                       ifelse( rel_pay_amt6> 0.94 & rel_pay_amt6<= 0.97, "0.94 to 0.97",
                                                                               ifelse( rel_pay_amt6> 0.97 & rel_pay_amt6<= 0.99, "0.97 to 0.99",
                                                                                       ifelse( rel_pay_amt6> 0.99 & rel_pay_amt6<= 1.06, "0.99 to 1.06",
                                                                                               ifelse( rel_pay_amt6> 1.06 & rel_pay_amt6<= 1.17, "1.06 to 1.17",
                                                                                                       ifelse( rel_pay_amt6> 1.17 & rel_pay_amt6<= 1.39, "1.17 to 1.39",
                                                                                                               "gt 1.39"))))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(limit_bal_rng =
                                                        ifelse(limit_bal<=9.9,"lt 9.9",
                                                               ifelse( limit_bal> 9.9 & limit_bal<= 10.6, "9.9 to 10.6",
                                                                       ifelse( limit_bal> 10.6 & limit_bal<= 11, "10.6 to 11",
                                                                               ifelse( limit_bal> 11 & limit_bal<= 11.41, "11 to 11.41",
                                                                                       ifelse( limit_bal> 11.41 & limit_bal<= 11.7, "11.41 to 11.7",
                                                                                               ifelse( limit_bal> 11.7 & limit_bal<= 11.98, "11.7 to 11.98",
                                                                                                       ifelse( limit_bal> 11.98 & limit_bal<= 12.21, "11.98 to 12.21",
                                                                                                               ifelse( limit_bal> 12.21 & limit_bal<= 12.43, "12.21 to 12.43",
                                                                                                                       ifelse( limit_bal> 12.43 & limit_bal<= 12.77, "12.43 to 12.77",
                                                                                                                               "gt 12.77"))))))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_pay_amt4_rng =
                                                        ifelse(rel_pay_amt4<=0.91,"lt 0.91",
                                                               ifelse( rel_pay_amt4> 0.91 & rel_pay_amt4<= 0.95, "0.91 to 0.95",
                                                                       ifelse( rel_pay_amt4> 0.95 & rel_pay_amt4<= 0.98, "0.95 to 0.98",
                                                                               ifelse( rel_pay_amt4> 0.98 & rel_pay_amt4<= 1, "0.98 to 1",
                                                                                       ifelse( rel_pay_amt4> 1 & rel_pay_amt4<= 1.07, "1 to 1.07",
                                                                                               ifelse( rel_pay_amt4> 1.07 & rel_pay_amt4<= 1.18, "1.07 to 1.18",
                                                                                                       ifelse( rel_pay_amt4> 1.18 & rel_pay_amt4<= 1.41, "1.18 to 1.41",
                                                                                                               "gt 1.41"))))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_pay_amt3_rng =
                                                        ifelse(rel_pay_amt3<=0.93,"lt 0.93",
                                                               ifelse( rel_pay_amt3> 0.93 & rel_pay_amt3<= 0.97, "0.93 to 0.97",
                                                                       ifelse( rel_pay_amt3> 0.97 & rel_pay_amt3<= 0.99, "0.97 to 0.99",
                                                                               ifelse( rel_pay_amt3> 0.99 & rel_pay_amt3<= 1.02, "0.99 to 1.02",
                                                                                       ifelse( rel_pay_amt3> 1.02 & rel_pay_amt3<= 1.1, "1.02 to 1.1",
                                                                                               ifelse( rel_pay_amt3> 1.1 & rel_pay_amt3<= 1.21, "1.1 to 1.21",
                                                                                                       ifelse( rel_pay_amt3> 1.21 & rel_pay_amt3<= 1.48, "1.21 to 1.48",
                                                                                                               "gt 1.48"))))))))

###########################
cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_bill_amt1_rng =
                                                        ifelse(rel_bill_amt1<=0.79,"lt 0.79",
                                                               ifelse( rel_bill_amt1> 0.79 & rel_bill_amt1<= 0.97, "0.79 to 0.97",
                                                                       ifelse( rel_bill_amt1> 0.97 & rel_bill_amt1<= 0.98, "0.97 to 0.98",
                                                                               ifelse( rel_bill_amt1> 0.98 & rel_bill_amt1<= 0.99, "0.98 to 0.99",
                                                                                       ifelse( rel_bill_amt1> 0.99 & rel_bill_amt1<= 1, "0.99 to 1",
                                                                                               ifelse( rel_bill_amt1> 1 & rel_bill_amt1<= 1.01, "1 to 1.01",
                                                                                                       ifelse( rel_bill_amt1> 1.01 & rel_bill_amt1<= 1.03, "1.01 to 1.03",
                                                                                                               ifelse( rel_bill_amt1> 1.03 & rel_bill_amt1<= 1.1, "1.03 to 1.1",
                                                                                                                       ifelse( rel_bill_amt1> 1.1 & rel_bill_amt1<= 1.39, "1.1 to 1.39",
                                                                                                                               "gt 1.39"))))))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_bill_amt6_rng =
                                                        ifelse(rel_bill_amt6<=0.86,"lt 0.86",
                                                               ifelse( rel_bill_amt6> 0.86 & rel_bill_amt6<= 0.94, "0.86 to 0.94",
                                                                       ifelse( rel_bill_amt6> 0.94 & rel_bill_amt6<= 0.97, "0.94 to 0.97",
                                                                               ifelse( rel_bill_amt6> 0.97 & rel_bill_amt6<= 0.98, "0.97 to 0.98",
                                                                                       ifelse( rel_bill_amt6> 0.98 & rel_bill_amt6<= 0.99, "0.98 to 0.99",
                                                                                               ifelse( rel_bill_amt6> 0.99 & rel_bill_amt6<= 1, "0.99 to 1",
                                                                                                       ifelse( rel_bill_amt6> 1 & rel_bill_amt6<= 1.01, "1 to 1.01",
                                                                                                               ifelse( rel_bill_amt6> 1.01 & rel_bill_amt6<= 1, "1.01 to 1",
                                                                                                                       "gt 1")))))))))

############################
cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_bill_amt5_rng =
                                                        ifelse(rel_bill_amt5<=0.91,"lt 0.91",
                                                               ifelse( rel_bill_amt5> 0.91 & rel_bill_amt5<= 0.96, "0.91 to 0.96",
                                                                       ifelse( rel_bill_amt5> 0.96 & rel_bill_amt5<= 0.98, "0.96 to 0.98",
                                                                               ifelse( rel_bill_amt5> 0.98 & rel_bill_amt5<= 0.99, "0.98 to 0.99",
                                                                                       ifelse( rel_bill_amt5> 0.99 & rel_bill_amt5<= 1, "0.99 to 1",
                                                                                               ifelse( rel_bill_amt5> 1 & rel_bill_amt5<= 1.01, "1 to 1.01",
                                                                                                       ifelse( rel_bill_amt5> 1.01 & rel_bill_amt5<= 1.14, "1.01 to 1.14",
                                                                                                               "gt 1.14"))))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_bill_amt4_rng =
                                                        ifelse(rel_bill_amt4<=0.94,"lt 0.94",
                                                               ifelse( rel_bill_amt4> 0.94 & rel_bill_amt4<= 0.98, "0.94 to 0.98",
                                                                       ifelse( rel_bill_amt4> 0.98 & rel_bill_amt4<= 0.99, "0.98 to 0.99",
                                                                               ifelse( rel_bill_amt4> 0.99 & rel_bill_amt4<= 1, "0.99 to 1",
                                                                                       ifelse( rel_bill_amt4> 1 & rel_bill_amt4<= 1.03, "1 to 1.03",
                                                                                               ifelse( rel_bill_amt4> 1.03 & rel_bill_amt4<= 1.18, "1.03 to 1.18",
                                                                                                       "gt 1.18")))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_bill_amt2_rng =
                                                        ifelse(rel_bill_amt2<=0.97,"lt 0.97",
                                                               ifelse( rel_bill_amt2> 0.97 & rel_bill_amt2<= 0.99, "0.97 to 0.99",
                                                                       ifelse( rel_bill_amt2> 0.99 & rel_bill_amt2<= 1, "0.99 to 1",
                                                                               ifelse( rel_bill_amt2> 1 & rel_bill_amt2<= 1.02, "1 to 1.02",
                                                                                       ifelse( rel_bill_amt2> 1.02 & rel_bill_amt2<= 1.06, "1.02 to 1.06",
                                                                                               ifelse( rel_bill_amt2> 1.06 & rel_bill_amt2<= 1.25, "1.06 to 1.25",
                                                                                                       "gt 1.25")))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(rel_bill_amt3_rng =
                                                        ifelse(rel_bill_amt3<=0.97,"lt 0.97",
                                                               ifelse( rel_bill_amt3> 0.97 & rel_bill_amt3<= 0.99, "0.97 to 0.99",
                                                                       ifelse( rel_bill_amt3> 0.99 & rel_bill_amt3<= 1, "0.99 to 1",
                                                                               ifelse( rel_bill_amt3> 1 & rel_bill_amt3<= 1.01, "1 to 1.01",
                                                                                       ifelse( rel_bill_amt3> 1.01 & rel_bill_amt3<= 1.05, "1.01 to 1.05",
                                                                                               ifelse( rel_bill_amt3> 1.05 & rel_bill_amt3<= 1.2, "1.05 to 1.2",
                                                                                                       "gt 1.2")))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(age_rng =
                                                        ifelse(age<=4,"lt 4",
                                                               ifelse( age> 4 & age<= 6, "4 to 6",
                                                                       ifelse( age> 6 & age<= 8, "6 to 8",
                                                                               ifelse( age> 8 & age<= 10, "8 to 10",
                                                                                       ifelse( age> 10 & age<= 12, "10 to 12",
                                                                                               ifelse( age> 12 & age<= 15, "12 to 15",
                                                                                                       ifelse( age> 15 & age<= 18, "15 to 18",
                                                                                                               ifelse( age> 18 & age<= 22, "18 to 22",
                                                                                                                       ifelse( age> 22 & age<= 28, "22 to 28",
                                                                                                                               "gt 28"))))))))))

cc_test_model_LR_IV <- cc_test_model_LR_IV %>% mutate(
  education_rng = education
  , sex_rng = sex
  , marriage_rng = marriage
)

cc_test_model_LR_IV <- cc_test_model_LR_IV %>%
  ungroup() %>%
  select(pay_1_rng,pay_2_rng,pay_3_rng,pay_4_rng,pay_5_rng,pay_6_rng,       
         rel_pay_amt1_rng,rel_pay_amt2_rng,rel_pay_amt5_rng, 
         rel_pay_amt6_rng,limit_bal_rng,rel_pay_amt4_rng, 
         rel_pay_amt3_rng,rel_bill_amt1_rng,rel_bill_amt6_rng,
         rel_bill_amt5_rng,rel_bill_amt4_rng,
         rel_bill_amt2_rng,rel_bill_amt3_rng,education_rng,    
         age_rng,sex_rng,marriage_rng)

cc_test_model_LR_IV[colnames(cc_test_model_LR_IV)] <- 
  lapply(cc_test_model_LR_IV[colnames(cc_test_model_LR_IV)], function(x) as.factor(x) )
###################################################

cc_test_model_LR$pred <- ifelse(cc_test_model_LR$pred_vals <= 0.5, 0, 1)
table(cc_test_model_LR$pred)

cc_test_model_LR_IV$pred_vals <- predict(fit.train_IV,newdata = cc_test_model_LR_IV, type = "response")
cc_test_model_LR_IV$pred <- ifelse(cc_test_model_LR_IV$pred_vals <= 0.5, 0, 1)
table(cc_test_model_LR_IV$pred)

cc_test_model_LR_IV_res <- as.data.frame( cbind( cc_test$id, cc_test_model_LR_IV$pred_vals, cc_test_model_LR_IV$pred ) )
colnames(cc_test_model_LR_IV_res) <- c('ID','Pred. Prob','Pred. Value')

write.csv(cc_test_model_LR_IV_res, 'C:/Users/psaodekarx092757/Downloads/CTS/ML_201/CreditCards/credit_card_defaulters_test_pred.csv')
