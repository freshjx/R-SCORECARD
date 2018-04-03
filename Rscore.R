######################  设置工作路径 #######################
setwd('C:\\Users\\Administrator\\Desktop\\scorecard')
df <- read.csv('score_sample.csv')          
df<-df[,sapply(df,function(x) {sum(is.na(x))/nrow(df) < 0.95})] 


##################### 生成年龄和性别 ####################
df$id = as.character(df$id)
df$user_date = as.character(df$user_date)
df$curr_age = as.numeric(substr(df$user_date,1,4))-as.numeric(substr(df$id,7,10))
df$sex = ifelse(as.numeric(substr(df$id,17,17))%%2 == 0,0,1)


##################### 分层抽样及样本外 ########################
df_1 = df[,-grep("cons_.*_level$",names(df))]
df_2 =  df_1[,11:664]
df_2$tl_id_lasttime <- NULL
df_2$tl_cell_lasttime <- NULL
df_2$year <- NULL
df_2$month <- NULL

#####################  iv ######################
library(dplyr)

iv_var = setdiff(names(df_2),"flagy")
iv_l = list()
for(i in iv_var){

  mid_df = df[,c(i,"flagy")]
  Bad = sum(mid_df$flagy == 1,na.rm = T)
  Good = sum(mid_df$flagy == 0,na.rm =T)
  g_df = group_by(mid_df,mid_df[,1])%>%
    summarise(bad = sum(flagy == 1),
              good = sum(flagy == 0),
              woe = log((bad/good)/(Bad/Good)),
              iv = (bad/Bad-good/Good)*woe
    )
  g_df$iv[g_df$iv == Inf] <- 0 
  Iv = round(sum(g_df$iv),4)
  iv_l[[i]] = Iv
  
}

iv =  as.data.frame(unlist(iv_l))


##################### 拆分训练集及测试集 ####################
library(caret)
set.seed(10)
N = createDataPartition(y = df_2$flagy,p = 0.75,list = FALSE)
train = df_2[N,]
test = df_2[-N,]

######################  分箱生成woe ######################
library(woeBinning)
best_woe = woe.binning(train,'flagy',train,event.class = 0,stop.limit = 0.05)
woe_deploy = woe.binning.deploy(train,best_woe,min.iv.total=0.02,add.woe.or.dum.var='woe')
df_woe = woe_deploy[,c(grep('^woe.',names(woe_deploy)))]
df_woe$y = train$flagy

######################## 使用lasso ########################
library(h2o)
h2o.init()
h2o_df = as.h2o(df_woe)
fit <- h2o.glm(x=setdiff(names(h2o_df), "y") , y="y",h2o_df,family = "binomial", nfolds = 0,alpha = 0.5, lambda_search = F)
h2o_coef = h2o.varimp(fit)
h2o_coef_extract = h2o_coef[h2o_coef$coefficients>0,]
h2o_glm = h2o_df[,h2o_coef_extract$names[!is.na(h2o_coef_extract$names)]]
h2o_glm = as.data.frame(h2o_glm)
h2o_glm$y = df_woe$y

#####################  glm 逻辑回归-方法  #####################
num <- 1
rem.name = setdiff(names(h2o_glm),"y")

while(num > 0){
  
  ind_df = h2o_glm[,c(rem.name,"y")]
  glm_mode = glm(y~.,ind_df,family = "binomial",control=list(maxit=100))
  rem.name = names(glm_mode$coefficients)[glm_mode$coefficients > 0 
                                          & !is.na(glm_mode$coefficients)]
  num = length(names(ind_df)) - (length(rem.name)+1)
  
}

summary(glm_mode)

#########################  逐步回归 ##########################
glm_step<-step(glm_mode,direction = c("both")) 
summary(glm_step)

######################  删除不显著变量  ####################
sum_df = summary(glm_step)$coefficients
sum_mat = sum_df[sum_df[,4]<0.1,]
r_name = setdiff(row.names(sum_mat),"(Intercept)")
fit_df = h2o_glm[,c(r_name,"y")]

#####################  生成模型 ###################
fit = glm(y~.,fit_df,family = "binomial",control=list(maxit=100))
fit_step = step(fit,direction = c("both")) 
summary(fit_step)

######################## 计算方差膨胀因子########################
library(car)
vif(fit_step)

####################### 相关系数检验 #########################
cor_fin = df_woe[,remnames_fin]
cor_var = cor(cor_fin)
write.csv(cor_var,'cor_var.csv')

######################### 计算AUC及ks ############################
library(scorecard)
pre = predict(fit_step,df_woe,type='response')  # 预测每个样本概率
perf_eva(h2o_glm$y, pre, type = c("ks","lift","roc","pr"))

########################### 手动调分箱 #######################
rem_var = setdiff(names(fit_step$coefficients),"(Intercept)")
library(woeBinning)
var = gsub("woe\\.|\\.binned","",rem_var)
df_adj = df[,var]
df_adj$y = df$flagy
adj_bin = woe.binning(df_adj,'y',df_adj,event.class = 0)
adj_deploy = woe.binning.deploy(df_adj,adj_bin,add.woe.or.dum.var='woe')
adj_woe = adj_deploy[,c(grep('^woe.',names(adj_deploy)))]
adj_woe$y = df$y


##################### VAR-Bin #######################
library(dplyr)
train_one_analysis = matrix(rep(NA,12),1,12)
train_one_analysis = as.data.frame(train_one_analysis)
names(train_one_analysis) =c('Bin','Total.Count','Total.Distr.','Count.1','Count.0',
                             'Distr.1','Distr.0','Rate.0','Rate.1','WOE','IV','VAR')
all_num = nrow(adj_deploy)
bad_all = sum(adj_deploy$y == 1,na.rm = T)
good_all = sum(adj_deploy$y == 0,na.rm = T)
for (i in var){
  var_ans = adj_deploy[,grep(i,names(adj_deploy))]
  var_ans$y = adj_deploy$y
  tra_tab = group_by(var_ans,var_ans[,2])%>%
    summarise(Total.Count = n(),
              Total.Distr. = round(Total.Count/all_num,3)*100,
              Count.1 = sum(y == 1,na.rm = T),
              Count.0 = sum(y == 0,na.rm = T),
              Distr.1 = Count.1/bad_all,
              Distr.0 = Count.0/good_all,
              Rate.0 = round(Count.0/Total.Count,3)*100,
              Rate.1 = round(Count.1/Total.Count,3)*100,
              WOE = round(log(Distr.1/Distr.0)*100,2),
              IV = round((Distr.1-Distr.0)/100*WOE/100,3))
  names(tra_tab)[1] = 'Bin'
  tra_tab[nrow(tra_tab)+1,][2:11] = colSums(tra_tab[,2:11])
  tra_tab$VAR <- i
  tra_tab[nrow(tra_tab)+1,] = rep(NA,12)
  train_one_analysis = rbind(train_one_analysis,tra_tab)
}
write.xlsx(train_one_analysis,'train_one_analysis.xlsx',colWidths = "auto")

####################### ADJ-Bin #################

library(dplyr)
a = 'age'  
df_new = df[c(a,'y')] 
df_new$bin = cut(df[,a],c(-Inf,25,30,Inf),include.lowest = T) 
all_num = nrow(df)
bad_all = sum(df$y == 1,na.rm = T)
good_all = sum(df$y == 0,na.rm = T)
new_table = group_by(df_new,bin)%>%
  mutate(Total.Count = n(),
         Total.Distr. = round(Total.Count/all_num,3)*100,
         Count.1 = sum(y == 1,na.rm = T),
         Count.0 = sum(y == 0,na.rm = T),
         Distr.1 = Count.1/bad_all,
         Distr.0 = Count.0/good_all,
         Rate.0 = round(Count.0/Total.Count,3)*100,
         Rate.1 = round(Count.1/Total.Count,3)*100,
         WOE = round(log(Distr.1/Distr.0)*100,2),
         IV = round((Distr.1-Distr.0)/100*WOE/100,3))
sum_table = group_by(df_new,bin)%>%
  summarise(Total.Count = n(),
            Total.Distr. = round(Total.Count/all_num,3)*100,
            Count.1 = sum(y == 1,na.rm = T),
            Count.0 = sum(y == 0,na.rm = T),
            Distr.1 = Count.1/bad_all,
            Distr.0 = Count.0/good_all,
            Rate.0 = round(Count.0/Total.Count,3)*100,
            Rate.1 = round(Count.1/Total.Count,3)*100,
            WOE = round(log(Distr.1/Distr.0)*100,2),
            IV = round((Distr.1-Distr.0)/100*WOE/100,3))
woe_value = paste0('woe.',a,'.binned')
bin = paste0(a,'.binned')
df_new$woe_value = new_table$WOE
names(df_new)[3] = bin
names(df_new)[4] = woe_value


###########################   woe赋值  #############################
adj_woe$"woe.age.binned" = df_new$"woe.age.binned"

adj_fit = glm(y~.,adj_woe,family = "binomial",control=list(maxit=100))
#step_fit<-step(adj_fit,direction = c("both"))
step_fit = adj_fit
summary(step_fit)

library(scorecard)
pred = predict(step_fit,adj_woe,type='response') 
perf_plot(adj_woe$y,pred, title = "train", groupnum = 20)


##################### score打分 #######################
score = function(p0,pdo,odds,df,model){
  B = pdo/log(2)
  A = p0+B*log(odds)
  Intercept = A-B*coef(model)[1]
  n_var = length(coef(model))-1
  num = Intercept/n_var
  for(i in 1:n_var){
    df[n_var+1+i] = round(num-B*coef(model)[i+1]*df[i])
  }
  df$point = rowSums(df[,(n_var+2):ncol(df)])
  hist(df$point)
  print(summary(df$point))
  df_point = df
  df_point
}
df_point<-score(p0 = 700, pdo = 60 ,odds = 0.02,df = adj_woe,mode = step_fit)

save(step_fit,file = 'fin_fit.RData')

write.csv(df_point,'mode_point.csv',na = '',row.names = F)

# p0 = 700;pdo = 60;odds = 0.02
# 
# B = pdo/log(2)
# A = p0+B*log(odds)
# 
# pred = predict(step_fit,adj_woe,type='response')
# 
# score = A - B * log(pred/(1-pred))
# 
# hist(score)
# 
# prop.table(table(cut(score,c(-Inf,580,630,Inf),include.lowest = T,right = F)))
# 
# prop.table(table(cut(score,c(-Inf,520,600,Inf),include.lowest = T,right = F)))


###############  制作评分卡 ################
ls = list()
for (i in var){
  df_var =  df_point[,grep(i,names(df_point))]
  score_card = unique(df_var)
  ls[[i]] = score_card
}


#################### 训练集 ####################
name_fin = setdiff(names(fit_step$coefficients),"(Intercept)")
var = gsub("woe\\.|\\.binned","",name_fin)
train_df = train[,var]
train_df$y = train$y
train_bin = woe.binning(train_df,'y',train_df,event.class = 0)
train_deploy = woe.binning.deploy(train_df,train_bin,add.woe.or.dum.var='woe')
train_woe = train_deploy[,c(grep('^woe.',names(train_deploy)))]
train_woe$y = train$y
train_fit = glm(y~.,train_woe,family = "binomial",control=list(maxit=100))
train_step<-step(train_fit,direction = c("both")) 
summary(train_step)
train_pre = predict(train_step,train_woe,type='response') 
library(woebin)
perf_plot(train$y,train_pre, title = "train", groupnum = 20)
train_table =  woe.binning.table(train_bin)

#####################  测试集 ########################
var = gsub("woe\\.|\\.binned","",remnames_fin)
test_df = test[,var]
test_df$y = test$y
test_deploy = woe.binning.deploy(test_df,train_bin,add.woe.or.dum.var='woe')
test_woe = test_deploy[,c(grep('^woe.',names(test_deploy)))]
test_woe$y = test$y
test_pre = predict(train_step,test_woe,type='response') 
library(woebin)
perf_plot(test$y,test_pre, title = "test", groupnum = 20)


##################### 训练集单变量提取 #######################
name_fin = setdiff(names(fit_step$coefficients),"(Intercept)")
var = gsub("woe\\.|\\.binned","",name_fin)
library(dplyr)
train_one_analysis = matrix(rep(NA,12),1,12)
train_one_analysis = as.data.frame(train_one_analysis)
names(train_one_analysis) =c('Bin','Total.Count','Total.Distr.','Count.1','Count.0',
                             'Distr.1','Distr.0','Rate.0','Rate.1','WOE','IV','VAR')
all_num = nrow(train_deploy)
bad_all = sum(train_deploy$y == 1,na.rm = T)
good_all = sum(train_deploy$y == 0,na.rm = T)
for (i in var){
  var_ans = train_deploy[,grep(i,names(train_deploy))]
  var_ans$y = train_df$y
  tra_tab = group_by(var_ans,var_ans[,2])%>%
    summarise(Total.Count = n(),
              Total.Distr. = round(Total.Count/all_num,3),
              Count.1 = sum(y == 1,na.rm = T),
              Count.0 = sum(y == 0,na.rm = T),
              Distr.1 = round(Count.1/bad_all,3),
              Distr.0 = round(Count.0/good_all,3),
              Rate.0 = round(Count.0/Total.Count,3),
              Rate.1 = round(Count.1/Total.Count,3),
              WOE = round(log(Distr.1/Distr.0)*100,2),
              IV = round((Distr.1-Distr.0)*WOE/100,3))
  names(tra_tab)[1] = 'Bin'
  tra_tab[nrow(tra_tab)+1,][2:11] = colSums(tra_tab[,2:11])
  tra_tab$VAR <- i
  tra_tab[nrow(tra_tab)+1,] = rep(NA,12)
  train_one_analysis = rbind(train_one_analysis,tra_tab)
}

##################### 测试集单变量提取 ######################
test_one_analysis = matrix(rep(NA,12),1,12)
test_one_analysis = as.data.frame(test_one_analysis)
names(test_one_analysis) =c('Bin','Total.Count','Total.Distr.','Count.1','Count.0',
                            'Distr.1','Distr.0','Rate.0','Rate.1','WOE','IV','VAR')
all_num = nrow(test_deploy)
bad_all = sum(test_deploy$y == 1,na.rm = T)
good_all = sum(test_deploy$y == 0,na.rm = T)
for (i in var){
  var_ans = test_deploy[,grep(i,names(test_deploy))]
  var_ans$y = test_df$y
  tes_tab = group_by(var_ans,var_ans[,2])%>%
    summarise(Total.Count = n(),
              Total.Distr. = round(Total.Count/all_num,3),
              Count.1 = sum(y == 1,na.rm = T),
              Count.0 = sum(y == 0,na.rm = T),
              Distr.1 = round(Count.1/bad_all,3),
              Distr.0 = round(Count.0/good_all,3),
              Rate.0 = round(Count.0/Total.Count,3),
              Rate.1 = round(Count.1/Total.Count,3),
              WOE = round(log(Distr.1/Distr.0)*100,2),
              IV = round((Distr.1-Distr.0)*WOE/100,3))
  names(tes_tab)[1] = 'Bin'
  tes_tab[nrow(tes_tab)+1,][2:11] = colSums(tes_tab[,2:11])
  tes_tab$VAR <- i
  tes_tab[nrow(tes_tab)+1,] = rep(NA,12)
  test_one_analysis = rbind(test_one_analysis,tes_tab)
}


library(openxlsx)
L = list()
L$train_one_analysis = train_one_analysis
L$test_one_analysis = test_one_analysis
write.xlsx(L,'train_test_one_analysis.xlsx')


##################### 模型woe调整 #####################
train_woe$woe.ir_id_x_cell_notmat_days.binned = ifelse((is.na(train_df$ir_id_x_cell_notmat_days)|train_df$ir_id_x_cell_notmat_days == 0),-6.3196827,37.0655258)
test_woe$woe.ir_id_x_cell_notmat_days.binned = ifelse((is.na(test_df$ir_id_x_cell_notmat_days)|test_df$ir_id_x_cell_notmat_days == 0),-6.3196827,37.0655258)

##################### 模型打分 #######################
B = 50/log(2)
A = 500+B*log(36/64)
train_woe$Intercept = A-B*train_step$coefficients[1]
for(i in 1:(length(train_step$coefficients)-1)){
  train_woe[13+i] = train_step$coefficients[i+1]*train_woe[i]*(-B)
}
train_woe$point = train_woe$Intercept+rowSums(train_woe[,14:24])
train_woe$score = round(300+700*(train_woe$point-(sum(apply(train_woe[,14:24],2,min))+train_woe$Intercept))/(sum(apply(train_woe[,14:24],2,max))-sum(apply(train_woe[,14:24],2,min))))
#train_woe$point = 361.11962-72.134752*rowSums(train_woe[,13:24])
hist(train_woe$score)
summary(train_woe$score)
train_woe$pred = train_pre

#######################  评分卡  ##########################
woe_var = names(train_woe)[grep('^woe.*binned$',names(train_woe))]
test_one_analysis = matrix(rep(NA,12),1,12)
test_one_analysis = as.data.frame(test_one_analysis)
names(test_one_analysis) =c('Bin','Total.Count','Total.Distr.','Count.1','Count.0',
                            'Distr.1','Distr.0','Rate.0','Rate.1','WOE','IV','VAR')
all_num = nrow(test_deploy)
bad_all = sum(test_deploy$y == 1,na.rm = T)
good_all = sum(test_deploy$y == 0,na.rm = T)
for (i in woe_var){
  var_ans = test_deploy[,grep(i,names(test_deploy))]
  var_ans$y = test_df$y
  tes_tab = group_by(var_ans,var_ans[,2])%>%
    summarise(Total.Count = n(),
              Total.Distr. = round(Total.Count/all_num,3),
              Count.1 = sum(y == 1,na.rm = T),
              Count.0 = sum(y == 0,na.rm = T),
              Distr.1 = round(Count.1/bad_all,3),
              Distr.0 = round(Count.0/good_all,3),
              Rate.0 = round(Count.0/Total.Count,3),
              Rate.1 = round(Count.1/Total.Count,3),
              WOE = round(log(Distr.1/Distr.0)*100,2),
              IV = round((Distr.1-Distr.0)*WOE/100,3))
  names(tes_tab)[1] = 'Bin'
  tes_tab[nrow(tes_tab)+1,][2:11] = colSums(tes_tab[,2:11])
  tes_tab$VAR <- i
  tes_tab[nrow(tes_tab)+1,] = rep(NA,12)
  test_one_analysis = rbind(test_one_analysis,tes_tab)
}


##################### 黑名单逻辑 #######################
sl_df = df4[,c(grep('^sl_',names(df4)))]
df4$if_blacklist = ifelse((sl_df$sl_id_bank_bad == 0|sl_df$sl_id_bank_bad == 1
                           |sl_df$sl_id_p2p_bad == 0|sl_df$sl_id_p2p_bad == 1
                           |sl_df$sl_id_p2p_fraud == 0|sl_df$sl_id_p2p_fraud == 1
                           |sl_df$sl_id_court_bad == 0|sl_df$sl_id_court_bad == 1
                           |sl_df$sl_id_court_executed == 0|sl_df$sl_id_court_executed == 1
                           |sl_df$sl_cell_bank_bad == 0|sl_df$sl_cell_bank_bad == 1
                           |sl_df$sl_cell_p2p_bad == 0|sl_df$sl_cell_p2p_bad == 1
                           |sl_df$sl_cell_p2p_fraud == 0|sl_df$sl_cell_p2p_fraud == 1
),1,0)
df4$if_blacklist[is.na(df4$if_blacklist)]<-0
zz$score = ifelse(df4$flag_stability_c == 1 | 
                    df4$flag_inforelation == 1 | 
                    df4$flag_applyloanstr == 1,df4$score,'')
train_woe$score = ifelse(train_woe$if_blacklist == 1,300,train_woe$score)

###################### 评分输出逻辑 #####################
train_woe$score = ifelse(train$flag_stability_c == 1 | 
                           train$flag_inforelation == 1 | 
                           train$flag_applyloanstr == 1,train_woe$score,'')

#######################  决策表  #######################
train_woe$pred = train_pre
train_woe = train_woe[!is.na(train_woe$score),]
train_woe = arrange(train_woe,pred)
train_woe$group = ceiling(as.integer(row.names(train_woe))/(nrow(train_woe)/20))
df_group = group_by(train_woe,group)%>%
  summarise(score_range = paste(range(score)[1],range(score)[2],sep = ','),
            cnt = n(),
            Bad_num = sum(y == 1,na.rm = T),
            Bad_rate = Bad_num/cnt,
            pred_p  = mean(pred,na.rm = T)
  )
write.csv(df_group,'df_group1.csv')

######################### psi ###########################
train_woe = train_woe[!is.na(train_woe$score),]
train_woe = arrange(train_woe,desc(score))
bad_all = sum(train_woe$y == 1,na.rm =T)
all_num = nrow(train_woe)
train_woe$group = cut(train_woe$score,seq(300,1000,35),right = F,include.lowest = T)
df_group = group_by(train_woe,group)%>%
  summarise(cnt = n(),
            Bad_num = sum(y == 1,na.rm = T),
            Bad_rate = Bad_num/cnt,
            bin_rate = cnt/all_num,
            TPR = Bad_num/bad_all
  )
#总样本
sum((df_group_1$bin_rate-df_group$bin_rate)*log(df_group_1$bin_rate/df_group$bin_rate))
#坏样本
sum((df_group_1$TPR-df_group$TPR)*log(df_group_1$TPR/df_group$TPR))
#坏客户率
sum((df_group_1$Bad_rate-df_group$Bad_rate)*log(df_group_1$Bad_rate/df_group$Bad_rate))


