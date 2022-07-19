if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load(tidyverse,  # 书写整洁的代码
               magrittr,   # 包含更多管道符——%<>% 
               readxl,     # 读取 xlsx
               writexl,    # 输出xlsx
               mlr3verse,  # mlr3 机器学习模型库
               purrr,      # map
               DALEX,      # DALEX 模型解释
               ddpcr,      # 屏蔽提示消息
               ggplot2     # 画图
               )    

rm(list = ls())
read_xlsx("element.xlsx") -> data_raw

# 数据前处理
data_raw %>% 
  mutate(ptbirth = as.factor(ptbirth)) -> data

# 预测问题——预测早产 ptbirth 
# 首先设置好机器学习的 task
# --回归问题：as_task_regr()
# --分类问题：as_task_classif()
data %>% 
  select(birthweight,
         age,
         weight,
         height,
         education,
         smoking,
         As:Ti) %>% 
  as_task_regr(target = "birthweight",
               id = "birthweight") -> task.birthweight

# 我们建立弹性网络、随机森林、Xgboost三种模型，并比较它们的准确性
# 设置弹性网络 learner
learner.elastic.net = lrn("regr.glmnet",
                          predict_sets = c("train", "test"))
# 使用 autotune（不是说唱的那个！） 自动调参
# 弹性网络需要优化的参数有两个：alpha和 lambda，它们共同决定了两个正则项的系数
#这里设置参数的调节范围为0-1
search_space = ps(
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1)
)
terminator = trm("evals", n_evals = 10)  # 10次迭代
tuner = tnr("random_search")             # 自调参的方法
at = AutoTuner$new(
  learner = learner.elastic.net,         # 需要调试参数的learner
  resampling = rsmp("cv", folds = 5),    # 使用5折交叉验证评价模型效果
  measure = msr("regr.mse"),             # 使用均方误差 mse 作为评价指标
  search_space = search_space,           # 调参范围
  terminator = terminator,               # 迭代方法
  tuner = tuner                          # 调参方法
)
at$train(task.birthweight)

at$model$tuning_instance$result_learner_param_vals

learner.elastic.net$param_set$values <- at$model$tuning_instance$result_learner_param_vals
learner.elastic.net$param_set

# Randomforest
learner.rf = lrn("regr.ranger",
                 predict_sets = c("train", "test"))
learner.rf$param_set

search_space = ps(
  max.depth = p_int(lower = 6, upper = 100),
  mtry.ratio = p_dbl(lower = 0.3, upper = 0.7),
  num.trees = p_int(lower = 10, upper = 500)
)
terminator = trm("evals", n_evals = 10)  
tuner = tnr("random_search")             
at = AutoTuner$new(
  learner = learner.rf,         
  resampling = rsmp("cv", folds = 5),    
  measure = msr("regr.mse"),           
  search_space = search_space,           
  terminator = terminator,               
  tuner = tuner                          
)
ddpcr::quiet(at$train(task.birthweight))
learner.rf$param_set$values <- at$model$tuning_instance$result_learner_param_vals
learner.rf$param_set

# Xgboost
learner.xgb = lrn("regr.xgboost",
                  predict_sets = c("train", "test"))
learner.xgb$param_set

search_space = ps(
  subsample = p_dbl(lower = 0.5, upper = 1),
  colsample_bytree = p_dbl(lower = 0.5, upper = 1),
  max_depth = p_int(lower = 3, upper = 10),
  nrounds = p_int(lower = 2, upper = 200)
)
terminator = trm("evals", n_evals = 10)  
tuner = tnr("random_search")   

at = AutoTuner$new(
  learner = learner.xgb,         
  resampling = rsmp("cv", folds = 5),    
  measure = msr("regr.mse"),           
  search_space = search_space,           
  terminator = terminator,               
  tuner = tuner                          
)

ddpcr::quiet(at$train(task.birthweight))
learner.xgb$param_set$values <- at$model$tuning_instance$result_learner_param_vals
learner.xgb$param_set

# 模型评价
design = benchmark_grid(
  tasks = task.birthweight,
  learners = c(learner.elastic.net,
               learner.rf,
               learner.xgb),
  resamplings = rsmp("cv", folds = 5))

benchmark(design) -> bmr
bmr

list(msr("regr.mse", predict_sets = "train", id = "mse_train"),
     msr("regr.mse", predict_sets = "test", id ="mse_test")) -> measures_bmr
bmr$aggregate(measures_bmr)
bmr %>% autoplot(type = "boxplot")

# 正式建模
learner.rf$train(task.birthweight)

# 模型解释
expl_rf <- DALEX::explain(model = learner.rf,
                          data = data %>% select(task.birthweight$feature_names) %>% as.data.frame(),
                          y = data$birthweight,
                          label = "Randomforest")

DALEX::model_parts(explainer = expl_rf,
            loss_function = loss_default(expl_rf$model_info$type),
            B=10,
            type = "difference") -> importance.rf
importance.rf %>% 
  plot()+
  theme(plot.background = element_rect(fill = 'white'))+
  ggtitle("Feature Importance",
          "")

Pdp <- function(feature){
  DALEX::model_profile(explainer = expl_rf,
                       variables = feature) %>% 
    plot(geom = "profiles")+
    theme(plot.background = element_rect(fill = 'white'))
}
map(c("age","Sr","As"),
    Pdp)

# 获得预测值
learner.rf$predict(task.birthweight)

data %>% 
  select(age,
         weight,
         height,
         education,
         smoking,
         As:Ti) %>% 
  head(50)   -> newdata
learner.rf$predict_newdata(newdata)
  
# 分类问题
data %>% 
  select(ptbirth,
         age,
         weight,
         height,
         education,
         smoking,
         As:Ti) %>% 
  as_task_classif(target = "ptbirth",
               id = "ptbirth") -> task.birthweight

learner.rf = lrn("classif.ranger",
                 predict_sets = c("train", "test"))

# 生存分析
# data %>% 
#   as_task_surv(time = time.Y,
#                event= event.Y,
#                type= 'right') -> task.surv
# learner.rf = lrn("surv.ranger")
# 有哪些模型？key都怎么写？
mlr_learners$keys()

