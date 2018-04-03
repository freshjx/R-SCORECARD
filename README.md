一、	读取数据集，定义Y变量（第一步）

定义数据集中需要预测的指标（即目标变量），其中1代表坏客户（逾期客户），0则是代表好客户（未逾期）

代码：（读取数据集）

setwd('C:\\Users\\shichen\\Desktop')#设置工作路径，即csv文件所在位置（文件名称mode.csv）
data<-read.csv('mode.csv')#通过函数read.csv()读取mode文件，生成数据集data
dim(data)#查看数据集总体情况，有多少行，多少列
table(data$y)#查看数据集中，好坏样本个数，其中y代表目标变量
prop.table(table(data$y))#查看数据集中，好坏样本占比

二、	删除缺失率95%以上变量（第二步）

处理缺失，删除95%及以上缺失率变量

代码：（删除缺失率较高变量）

data<-data[,sapply(data,function(x){sum(is.na(x))/nrow(data)<0.95})]#删除高缺失变量
dim(data)#查看目前数据情况，剩余多少行多少列

三、	变量分类

根据业务含义，将连续变量及分类变量进行分类处理，其中分类变量需要转化成因子型数据，便于woe分箱时，给出相应函数要求的数据类型

代码：（根据业务理解分成连续变量及分类变量）

data_str_1<-data[,c(grep('^flag_',names(data)))]#flag开头的分类变量
data_str_2<-data[,c(grep('^stab_auth_',names(data)))]#稳定性模块分类变量
data_str_3<-data[,c(grep('^ns_',names(data)))]#网购指数分类变量
data_str_3<-data_str_3[,-c(grep('ns_avg_cnt',names(data_str_3)))]#去除月均消费频次变量
data_str_4<-data[,c(grep('^sl_',names(data)))]#特殊名单分类变量
data_str_5<-data[,c(grep(paste0('^ir_.*_cell$','|','^ir_.*_id$'),names(data)))]#个人关联分类变量
data_str_6<-data[,c(grep(paste0('^is','|','^cont','|','^time','|','_mode$','|','_cate$','|','^auth','|','matchtype'),names(data)))]#用户行为分类变量
data_str<-cbind(data_str_1,data_str_2,data_str_3,data_str_4,data_str_5,data_str_6)#将多个数据集合并成一个字符型数据集
data_str<-as.data.frame(apply(data_str,2,as.factor))#按照列将数据集每一列转化成因子型数据
str_names=names(data)%in%names(data_str)#将分类变量列名提取出来
data_num<-data[,!str_names]#将数值型变量提取出来生成数据集data_num
data_str_num<-cbind(data_str,data_num)#合成新的数据集用于woe分箱
data_str_num$y=data$y#将预测变量添加进入新的数据集

四、	变量分箱（第三步）

3.1变量分箱原理

在评分卡建模中，变量分箱（binning）是对连续变量离散化（discretization）的一种称呼。要将logistic模型转换为标准评分卡的形式，这一环节是必须完成的。信用评分卡开发中一般有常用的等距分段、等深分段、最优分段。
其中等距分段（Equvallengthintervals）是指分段的区间是一致的，比如年龄以十年作为一个分段；等深分段（Equalfrequencyintervals）是先确定分段数量，然后令每个分段中数据数量大致相等；最优分段（OptimalBinning）又叫监督离散化（superviseddiscretizaion），使用递归划分（RecursivePartitioning）将连续变量分为分段，背后是一种基于条件推断查找最佳分组的算法（ConditionalInferenceTree）
“条件推断决策树”（conditionalinferencetrees）背景理论：它根据统计检验来确定自变量和分割点的选择。即先假设所有自变量与因变量均独立。再对它们进行卡方独立检验，检验P值小于阀值的自变量加入模型，相关性最强的自变量作为第一次分割的自变量。自变量选择好后，用置换检验来选择分割点。用party包建立的决策树不需要剪枝，因为阀值就决定了模型的复杂程度。所以如何决定阀值参数是非常重要的（参见ctree_control）。较为流行的做法是取不同的参数值进行交叉检验，选择误差最小的模型参数。

3.2R-woeBinning

将连续变量和分类变量进行最优分箱（原理为条件推断决策树），筛选IV>=0.02变量

3.3代码：

library(woeBinning)#加载woe分箱的包
best_iv_str=woe.binning(data_str_num,'y',data_str_num)#将变量进行最优分箱，变量结果会按照IV值降序排列，排在前面的变量是IV值较高变量，依次降序往下排。
best_iv_str[1,]#可以查看第一个IV最高的变量名称，分箱详情，以及IV值。
best_iv_str_woe_deploy=woe.binning.deploy(data_str_num,best_iv_str,min.iv.total=0.1,add.woe.or.dum.var='woe')
#将分享之后的结果进行展示，为一个包含变量，woe分箱区段，woe值的数据集，min.iv.total=0.1，这个参数决定IV筛选变量最低值，由于各变量均为最优分箱，相对等高，等宽分箱IV值要大，可筛选IV>=0.1以上的变量，根据模型调整，可修改此参数。
data_woe=best_iv_str_woe_deploy[,c(grep('^woe.',names(best_iv_str_woe_deploy)))]#生成woe数据集
data_woe$y=data$y#生成带目标变量y的woe数据集

五、	分层抽样（第四步）

对woe转换之后的样本数据集，进行分层抽样，3/4的训练集。1/4的测试集

代码：

library(caret)
set.seed(123)#设置随机种子，保证两次抽样结果一样
inTrain<-createDataPartition(y=data_woe$y,p=0.75,list=FALSE)#使用createDataPartition的好处在于，它能将低熵数据集随机抽取出我们需要的训练集来。比如我们的数据集共有100个样本点，前50个是一类，后50个是一类。我们为了让训练集里两类样本都各有一些，必然希望从前50个样本点随机抽取一定比例，后50个里也随机抽取相应比例的样本点来组成训练集。这个手动过程因为涉及到人的主观意识，从而不能保证完全随机化。而createDataPartition会自动从y的各个level随机取出等比例的数据来，组成训练集，给我们省了很多事
training<-data_woe[inTrain,]#生成训练集
testing<-data_woe[-inTrain,]#生成测试集

六、	删除多重共线性（第五步）

6.1变量筛选

在风险建模的过程中，变量选择可以具体细化为单变量变量筛选(UnivariateVariableSelection）和多变量变量筛选(MultivariateVariableSelection)。多变量变量筛选一般会利用Stepwise算法（逐步回归）在变量池中选取最优变量。
单变量筛选，或者说单变量分析，是通过比较指标分箱和对应分箱的违约概率来确定指标是否符合经济意义，常见的单变量分析方法：WoE分析。

6.2经验

多变量筛选-逐步回归法：
将IV>=0.02变量筛选出来，如果进行向前向后逐步回归需要将变量顺序打乱，即将列变量随机化，每50个变量一组进行逐步回归，逐步回归后将其中估计参数负向变量删除，重新进行逐步回归，直到没有负向参数估计变量，将最终模型使用变量保存下来。例如一共分成5组，每组逐步回归变量保留下来，生成一个新的变量组，将新的变量组再进行逐步回归，生成最终模型。此方法缺点耗时长，处理繁琐。
多变量筛选-相关分析删除法：
R中有自带函数分析相关矩阵，并且删除某一变量与多个变量相关时，删除平均相关数最多的变量，最后剩余变量间没有相关性，即删除多重共线性，此方法耗时短，可以方便进行多次变量筛选，如IV取不同阈值时，模型效果变化
结论：
1.两种方法模型效果波动范围大概在0.01-0.03之间，其中逐步回归效果更好，变量可解释性都可以，逐步回归最后入模变量模块较单一，相关分析删减，模块综合性较好，可以有多个模块变量，不考虑时间，逐步回归效果更好，目前正在研究主成分分析及聚类分析，看是否可以提升建模效率及模型效果。

方法一：使用逐步回归删除多重共线性，生成训练集模型

代码：

set.seed(1234)#设置随机种子，使两次生成结果一直
training=training[,sample(1:nrow(training),nrow(training))]#由于最优分箱时，产生的变量IV顺序为降序，即前面变量均为IV较高变量，逐步回归时对结果产生影响，所以做列变量随机处理，如果不做处理，可以使用向后逐步回归。
y_index=which(names(training)==‘y’)#查找y所在列的位置
glm_df=training[,c(1:50,y_index)]#由于，如果全部变量跑逐步回归，R处理速度慢，当有多重共线性的时候50个变量大概1个半小时，跑完一个逐步回归，300变量的话，全部整理完大概9个小时，太耗时，当运行逻辑回归时，有的变量参数不估计，原因也是多重共线性导致的。所以需要改进此方法，提前处理多重共线性，正在研究变量的聚类分析。但是模型效果相比方法一要好，提升大概0.01-0.03左右，根据不同IV阈值限制，有所不同。
glm_mode=glm(flagy~.,glm_df,family="binomial",control=list(maxit=100))#变量分组跑逐步回归
glm_step<-step(glm_mode,direction=c("both"))#向前向后逐步回归
summary(glm_step)#查看逐步回归之后的模型
remain_vars=names(glm_step$coefficients)[glm_step$coefficients>0]#保留估计系数大于0的变量
remain_names=names(glm_df)%in%remain_vars#筛选符合条件逻辑值
glm_df_remain=glm_df[,remain_names]#筛选符合条件变量
glm_df_remain$flagy=glm_df$flagy#生成入模数据集，将此数据集，再运行上诉操作，至到没有负向估计参数。
以上步骤不断重复，大概300变量分成6组，会有6组最终remain_names，将它们生成一个新的组，重新进行上
述操作，先逻辑回归，再逐步回归，生成最终模型。

方法二：利用相关分析法删除相关变量，再进行逻辑回归及逐步回归，生成训练集模型

代码：

cor_var=cor(training)#生成变量相关矩阵
col_del=training[,-c(findCorrelation(cor_var,cutoff=.6,exact=TRUE))]#删除相关性特别大的变量
col_del$y=training$y#生成没有多重共线性的数据集，此方法缺点有可能删掉与目标变量相关性较大变量
glm_df=col_del#将数据集赋给glm_df运行逻辑回归
glm_mode=glm(flagy~.,glm_df,family="binomial",control=list(maxit=100))#运行逻辑回归
glm_step<-step(glm_mode,direction=c("both"))#向前向后逐步回归
summary(glm_step)#查看逐步回归之后的模型
remain_vars=names(glm_step$coefficients)[glm_step$coefficients>0]#保留估计系数大于0的变量
remain_names=names(glm_df)%in%remain_vars#筛选符合条件逻辑值
glm_df_remain=col_del[,remain_names]#筛选符合条件变量
glm_df_remain$flagy=col_del$flagy#生成入模数据集，将此数据集，再运行上诉操作，至到没有负向估计参数

七、	检查是否存在多重共线性（第六步）

将生成好的模型，利用方差膨胀因子VIF进行多重共线性检查，调整模型

代码：

vif(glm_step)#检查方差膨胀因子，即模型是否存在多重共线性，如果存在再进行相关性检验。

八、	模型预测（第七步）

将最终模型对训练集进行预测，计算AUC及KS

代码：

pre_result=predict(glm_step,glm_df_remain,type='response')#预测每个样本为1的概率
plot(seq(1,dim(training)[1],length=dim(training)[1]),sort(pre_result),col='blue')#画概率曲线图
Library(pROC)#加载包，计算AUC
pred_df=data.frame(pre_result=pre_result,flagy=training$flagy)#生成预测概率值与真实观测值的数据集
set.seed(100)#设置随机种子
pred_df=pred_df[sample(1:nrow(pred_df),nrow(pred_df)),]#由于ks曲线呈现锯齿状，随机化之后曲线可以变得平滑
modelroc<-roc(pred_df$flagy,pred_df$pre_result)#计算ROC
plot(modelroc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)#生成AUC的值及曲线面积图
ks_fun(pred_df$pre_result,pred_df$flagy,20)#根据自己写的ks函数，计算ks值，根据分位数原理分段，段数可调节

九、	模型验证（第八步）

将模型对测试集按照相应变量woe分箱进行预测，计算AUC及KS
模型验证时，根据生成好的模型glm_step，及模型用到的相应变量，在测试集上提取出来，并进行验证

代码：

test_df=testing[,remain_names]#筛选训练集最终入模变量
pre_test=predict(glm_step,test_df,type='response')#预测测试集每个样本为1的概率
plot(seq(1,dim(testning)[1],length=dim(testing)[1]),sort(pre_test),col='blue')#画概率曲线图
pred_test_df=data.frame(pre_result=pre_test,flagy=testing$flagy)#生成测试集预测概率值与真实观测值的数据集
set.seed(1000)#设置随机种子
pred_test_df=pred_test_df[sample(1:nrow(pred_test_df),nrow(pred_test_df)),]#由于ks曲线呈现锯齿状，随机化之后曲线可以变得平滑
library(pROC)
modelroc<-roc(pred_test_df$flagy,pred_test_df$pre_result)#计算测试集ROC
plot(modelroc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)#生成测试集AUC的值及曲线面积图
ks_fun(pred_test_df$pre_result,pred_test_df$flagy,20)#根据自己写的ks函数，计算ks值，根据分位数原理分段，段数可调节。


