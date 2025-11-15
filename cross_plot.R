
library('ggplot2')
library('dabestr')
library('ggpubr')
library('Rmisc')
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggerr)

setwd('C:/Huiying data/3rd chapter/b_biochar')

df<- read.csv('data_df.csv',header=T)


df_all<-df%>%
  filter(B=='1')
colnames(df_all)[colnames(df_all) == "gluco"] <- "all"


df_zero<-df%>%
  filter(B=='0')
colnames(df_zero)[colnames(df_zero) == "gluco"] <- "zero"

#### crossplot ####
setwd('C:/Huiying data/3rd chapter/b_biochar')

df<- read.csv('data_df.csv',header=T)
#df$type<- paste(df$treatments,df$Group,sep='_')


df_all<-df%>%
  filter(B=='1')
colnames(df_all)[colnames(df_all) == "pH"] <- "all"


df_zero<-df%>%
  filter(B=='0')
colnames(df_zero)[colnames(df_zero) == "pH"] <- "zero"


ES_zero<- data.frame(matrix(data = NA, nrow = 200, ncol = 2))
colnames(ES_zero) = c("treatments", "ef")
ES_zero[,1] = rep(c('1','2','4','6'),each=50)

sum<- numeric(0)
sum<- append(sum,bs)

population_CK =df_zero[df_zero["remark"]=="CT",]

size_CK = length(population_CK)

bs = numeric(0)
population_TR =df_zero[df_zero["lv"]=="1",]
size_TR = length(population_TR)
i=1
n_iter=50
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- append(sum,bs)


bs = numeric(0)
population_TR =df_zero[df_zero["lv"]=="2",]
size_TR = length(population_TR)
i=1
n_iter=50
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- append(sum,bs)


bs = numeric(0)
population_TR =df_zero[df_zero["lv"]=="4",]
size_TR = length(population_TR)
i=1
n_iter=50
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- append(sum,bs)


bs = numeric(0)
population_TR =df_zero[df_zero["lv"]=="6",]
size_TR = length(population_TR)
i=1
n_iter=50
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- append(sum,bs)
ES_zero[,2]<- sum

ES_all<- data.frame(matrix(data = NA, nrow = 200, ncol = 2))
colnames(ES_all) = c("treatments", "ef_all")
ES_all[,1] = rep(c('1','2','4','6'),each=50)

population_CK =df_all[df_all["remark"]=="CT",]

size_CK = length(population_CK)


bs = numeric(0)
population_TR =df_all[df_all["lv"]=="1",]
size_TR = length(population_TR)
i=1
n_iter=50
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- numeric(0)
sum_B<- append(sum_B,bs)


bs = numeric(0)
population_TR =df_all[df_all["lv"]=="2",]
size_TR = length(population_TR)
i=1
n_iter=50
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)

bs = numeric(0)
population_TR =df_all[df_all["lv"]=="4",]
size_TR = length(population_TR)
i=1
n_iter=50
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)
bs = numeric(0)
population_TR =df_all[df_all["lv"]=="6",]
size_TR = length(population_TR)
i=1
n_iter=50
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)
ES_all[,2]<- sum_B

df_test <- cbind(ES_zero, ef_all = ES_all$ef_all)

df_test$treatments<- factor(df_test$treatments,levels=c('1','2','4','6'))
df_summary <- df_test %>%
  dplyr::group_by(treatments) %>%
  dplyr::summarise(
    mean_zero = mean(ef, na.rm = TRUE),
    ci_lower_zero = mean(ef, na.rm = TRUE) - qt(0.975, df = dplyr::n() - 1) * sd(ef, na.rm = TRUE) / sqrt(dplyr::n()),
    ci_upper_zero = mean(ef, na.rm = TRUE) + qt(0.975, df = dplyr::n() - 1) * sd(ef, na.rm = TRUE) / sqrt(dplyr::n()),
    
    mean_all = mean(ef_all, na.rm = TRUE),
    ci_lower_all = mean(ef_all, na.rm = TRUE) - qt(0.975, df = dplyr::n() - 1) * sd(ef_all, na.rm = TRUE) / sqrt(dplyr::n()),
    ci_upper_all = mean(ef_all, na.rm = TRUE) + qt(0.975, df = dplyr::n() - 1) * sd(ef_all, na.rm = TRUE) / sqrt(dplyr::n())
  )


p_aboveground<- ggplot(df_summary, aes(x = mean_all, y = mean_zero, color = treatments)) +
  xlab('with biobar')+ggtitle('(aboveground)')+
  ylab('without biochar')+
  geom_hline(yintercept=0,linetype='solid')+
  geom_vline(xintercept=0,linetype='solid')+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # y = x line (red and dashed)
  geom_errorbar(
    aes(ymin = ci_lower_zero, ymax = ci_upper_zero),
    width = 0.04,            # error bar
    size = 0.8           # line width
  ) +
  geom_errorbarh(
    aes(xmin = ci_lower_all, xmax = ci_upper_all),
    height = 0.04,           # error bar
    size = 0.8            # line width
  ) +
  theme_minimal() +       
  theme(legend.position = "bottom")+
  coord_fixed()
#library(ggpubr)



p_pH<- ggplot(df_summary, aes(x = mean_all, y = mean_zero, color = treatments)) +
  xlab('with biobar')+ggtitle('(pH)')+
  ylab('without biochar')+
  geom_hline(yintercept=0,linetype='solid')+
  geom_vline(xintercept=0,linetype='solid')+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # y = x line (red and dashed)
  geom_errorbar(
    aes(ymin = ci_lower_zero, ymax = ci_upper_zero),
    width = 0.008,          
    size = 0.5           
  ) +
  geom_errorbarh(
    aes(xmin = ci_lower_all, xmax = ci_upper_all),
    height = 0.008,           
    size = 0.5            
  ) +
  theme_minimal() +       
  theme(legend.position = "right")+
  coord_fixed()


ggarrange(
  p_aboveground, p_belowground, p_whc, p_wsa,
  ncol = 2, nrow = 2,
  common.legend = TRUE,
  legend = "bottom"
)

ggarrange(
  p_gluco, p_cello, p_ace, p_phos,
  ncol = 2, nrow = 2,
  common.legend = TRUE,
  legend = "bottom"
)