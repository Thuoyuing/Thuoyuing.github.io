if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load(tidyverse,  # 书写整洁的代码
               magrittr,   # 包含更多管道符——%<>% 
               readxl,     # 读取 xlsx
               purrr,      # map
               tableone,   # 自动生成：表1.研究对象基本特征
               ggplot2,    # 画图
               gghalves,   # 小提琴图
               gridExtra,  # 多个图片排版
               psych)      # 相关矩阵图      
rm(list = ls())
read_xlsx("element.xlsx") -> data_raw

data_raw %>% 
  mutate(BMI = map2_dbl(weight,                # 计算 BMI，是二元函数
                        height,                # 使用 map2，让每一对weight与height
                        ~ .x/(.y/100)^2)) %>%  # 都按照这里的表达式计算
  mutate(age = case_when(age < 35 ~ 1,         
                         age >= 35 ~ 2),       # 将年龄以35岁为界限，分为1，2 
         BMI = case_when(BMI < 18.5 ~ 1,
                         BMI < 25 ~ 2,
                         BMI >= 25 ~ 3)) %>%   # 将BMI分为三个等级
  mutate(across(c(ptbirth,
                  age,
                  education,
                  smoking,
                  BMI),
                as.factor)) %>%                # 分类变量转为因子
  select(ptbirth,
         region,
         age,
         BMI,
         education,
         smoking,
         As:Ti) -> data      

data

CreateTableOne(data = data,
               vars = c("region","age","BMI","education","smoking",
                        "As","Sr","B","Al","Th","Ti"),
               strata = "ptbirth",
               addOverall = T) %>% 
  print(showAllLevels = T,
        nonnormal = c("As","Sr","B")) -> Table.one     # As,Sr,B使用非参数检验

Table.one %>%
  as.data.frame() %>%
  write.csv("TableOne.csv")

# 设置画图的主题，可以自己配置
theme(text = element_text(family = 'serif'),     # Times new roman字体
      plot.title = element_text(size='17',       
                                hjust = 0.5),   
      axis.title= element_text(size='15'),       
      panel.background = element_rect(fill = 'white', 
                                      color='black'),
      #panel.grid.major = element_line(color='grey',linetype = 2) 
      panel.grid.major = element_blank() 
      ) -> mytheme  

# 设定画图的函数，方便后续用 map 批量作图
vioboxplotfun = function(element_name){
  data %>% 
    select(ptbirth,
           region,
           element_name) %>% 
    mutate(ptbirth = factor(ptbirth,
                                labels = c("No",
                                           "Yes"))) %>% 
    rename("Conc." = element_name) %>% 
    ggplot(aes(y = Conc.))+
    geom_half_violin(aes(fill = ptbirth), 
                     size = 0.5,
                     trim=T, 
                     side="l",
                     adjust = 1,
                     width = 0.55)+
    geom_half_dotplot(aes(fill = ptbirth, 
                          color = NA),
                      alpha = 0.75,
                      method="histodot",  
                      stackdir="up", 
                      stackratio = 1.2, 
                      position = position_nudge(x=0.06,
                                                y=0)) + #half dot plot
    geom_boxplot(width = 0.08)+
    scale_fill_manual(values = c("#33CCCC",
                                 "#FF6666",
                                 "#33CCCC",
                                 "#FF6666"))+
    scale_color_manual(values = c("#black",
                                  "#black"))+
    # scale_y_continuous(limits = c(8,125))+
    labs(title = element_name, 
         y = "Conc. (ng/mL)")+
    facet_grid(region~ptbirth)+
    mytheme
}
# 选择需要作图的元素名称
data %>% 
  select(As:Ti) %>% 
  names() -> element_names
element_names
# 我们先对一个元素——As画图
vioboxplotfun(element_names[1])

# 使用 map 对每一个元素批量画图
#    ————对element_names中的每一个元素，都执行vioboxplotfun
map(element_names,         # 对element_names中的每一个元素
    vioboxplotfun) %>%     # 都执行vioboxplotfun
  marrangeGrob(nrow = 2, ncol = 3) %>%    # 将图片按2行3列排版
  ggsave(file = 'Xplot_byregion.png',     # 保存图片
         plot = .,
         width = 15,
         height = 8,
         limitsize = FALSE)


# 设定画图的函数，方便后续用 map 批量作图
QQplotfun = function(element_name){
  data %>% 
    select(ptbirth,
           region,
           element_name) %>% 
    mutate(ptbirth = factor(ptbirth,
                                labels = c("No",
                                           "Yes"))) %>% 
    rename("Conc." = element_name) %>% 
    ggplot(aes(sample = Conc., 
               color = ptbirth))+
    stat_qq()+
    stat_qq_line()+
    scale_fill_manual(values = c("#33CCCC",
                                 "#FF6666",
                                 "#33CCCC",
                                 "#FF6666"))+
    labs(title = element_name)+
    facet_grid(region~ptbirth)+
    mytheme
}

# 选择需要作图的元素名称
data %>% 
  select(As:Ti) %>% 
  names() -> element_names
element_names

# 先画一个图
QQplotfun(element_names[1])

# 批量作图 并保存
map(element_names, QQplotfun) %>% 
  marrangeGrob(nrow = 2, ncol = 3) %>% 
  ggsave(file = 'XQQplot_byregion.png', 
         plot = ., 
         width = 15, 
         height = 8,
         limitsize = FALSE)

# 计算Corr matrix
psych::corr.test(data %>% select(As:Ti),
                 method = 'pearson',
                 adjust = 'none') -> corr.matrix
# 调色
col2 <- colorRampPalette(c( "#053061","#2166AC","#4393C3", "#92C5DE","#D1E5F0",
                            "#FFFFFF",
                            "#FDDBC7","#F4A582","#D6604D", "#B2182B", "#67001F" ))
#画图
corrplot::corrplot(corr.matrix$r, 
                   method='color', 
                   col=col2(200), 
                   tl.cex=0.8, 
                   tl.col='black',
                   p.mat=corr.matrix$p,
                   sig.level=0.05, 
                   insig='label_sig',
                   pch.col='black',
                   pch.cex=2,mar=c(0,1,3,1))
corrplot::corrplot(corr.matrix$r, 
                   add=TRUE, 
                   type='lower', 
                   method='number', 
                   col=col2(200), 
                   number.cex=1,
                   tl.pos='n', 
                   cl.pos='n')

# t.test
data %>% 
  select(As:Ti) %>% 
  names() %>% 
  tibble(Variable=.)

data %>% 
  select(As:Ti) %>% 
  names() %>% 
  tibble(Variable=.) %>% 
  mutate(df0 = map(Variable,
                  ~ select(data %>% filter(ptbirth==0),
                           .)),
         df1 = map(Variable,
                   ~ select(data %>% filter(ptbirth==1),
                            .)),
         mean0 = map_dbl(df0,
                         ~ mean(as_vector(.))),
         mean1 = map_dbl(df1,
                         ~ mean(as_vector(.))),
         ttest = map2(df0,
                      df1,
                      ~ t.test(.x,
                               .y)),
         t = map_dbl(ttest,
                     ~ .$statistic),
         p_value = map_dbl(ttest,
                           ~ .$p.value)) -> ttest.by.ptbirth
ttest.by.ptbirth

ttest.by.ptbirth %>% 
  select(Variable,
         mean0,
         mean1,
         t,
         p_value)

# logit
data %>% 
  select(As:Ti) %>% 
  names() %>% 
  tibble(Variable=.) %>% 
  mutate(df = map(Variable,
                  ~ select(data,
                           ptbirth,
                           .)),
         model = map(df,
                     ~ glm(ptbirth~.,
                           family=binomial(link='logit'),
                           data=.,)),
         model_coefs = map(model, ~broom::tidy(.,conf.int = T, conf.level = 0.95))) %>% 
  unnest(model_coefs) %>% 
  select(Variable,term, estimate, conf.low, conf.high, p.value) %>% 
  set_names(c('Model','Vars','beta','ci_l','ci_h','p')) -> logistic.raw
logistic.raw

logistic.raw %>% 
  filter(Model==Vars) %>% 
  mutate(OR_raw = map_dbl(beta,
                          ~ exp(.)),
         low_raw = map_dbl(ci_l,
                           ~ exp(.)),
         high_raw = map_dbl(ci_h,
                            ~ exp(.))) %>% 
  rename(p_raw = p) %>% 
  select(Vars,
         OR_raw,
         low_raw,
         high_raw,
         p_raw) -> logistic.smry
logistic.smry



data %>% 
  select(As:Ti) %>% 
  names() %>% 
  tibble(Variable=.) %>% 
  mutate(df = map(Variable,
                  ~ select(data,
                           ptbirth,
                           .,
                           region,
                           age,
                           BMI,
                           education,
                           smoking)),
         models = map(df,
                     ~ glm(ptbirth~.,
                           family=binomial(link='logit'),
                           data=.,)),
         model_coefs = map(models, ~broom::tidy(.,conf.int = T, conf.level = 0.95))) %>% 
  unnest(model_coefs) %>% 
  select(Variable,term, estimate, conf.low, conf.high, p.value) %>% 
  set_names(c('Model','Vars','adj_beta','adj_ci_l','adj_ci_h','adj_p')) -> logistic.adj

logistic.adj %>% 
  filter(Model==Vars) %>% 
  mutate(OR_adj = map_dbl(adj_beta,
                          ~ exp(.)),
         low_adj = map_dbl(adj_ci_l,
                           ~ exp(.)),
         high_adj = map_dbl(adj_ci_h,
                            ~ exp(.))) %>% 
  rename(p_adj = adj_p) %>% 
  select(Vars,
         OR_adj,
         low_adj,
         high_adj,
         p_adj) -> logistic.adj.smry
logistic.adj.smry

left_join(logistic.smry,
          logistic.adj.smry,
          by = "Vars") -> forest.plot.data
forest.plot.data

forest.plot.data %<>% 
  pivot_longer(-c(Vars),
               names_to = c(".value","Model"),
               names_sep = "_") 
forest.plot.data

forest.plot.data %>% 
  ggplot(aes(x = Vars, y = OR , fill = Model, col = Model))+
  geom_point(position = position_dodge(0.8))+
  geom_pointrange(aes(ymax = high, ymin = low),
                  position = position_dodge(0.8), size = 0.5)+
  coord_flip()+
  geom_hline(aes(yintercept = 1))+
  labs(x = "Element", y = "OR (95%CI)")+
  mytheme+
  theme(strip.text = element_text(size = 14))
ggsave(file = 'logit.plot.png',
       width = 8, 
       height = 6,
       limitsize = FALSE)



