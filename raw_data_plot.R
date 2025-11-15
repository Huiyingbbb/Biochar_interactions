
setwd('C:/Huiying data/3rd chapter/b_biochar')

df<- read.csv('data_df.csv',header=T)
#df$type<- paste(df$treatments,df$Group,sep='_')


df_all<-df%>%
  filter(B=='1')
df_zero<-df%>%
  filter(B=='0')

colnames(df_all)[colnames(df_all) == "belowground"] <- "all"
colnames(df_zero)[colnames(df_zero) == "belowground"] <- "zero"

ES_zero<- data.frame(matrix(data = NA, nrow = 180, ncol = 2))
colnames(ES_zero) = c("treatments", "ef")
ES_zero[,1] = rep(c('S','Be','V','A','P','R','2f','4f','6f'),each=20)

population_CK =df_zero[df_zero["remark"]=="CT",]

size_CK = length(population_CK)

bs = numeric(0)
population_TR =df_zero[df_zero["remark"]=="S",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- numeric(0)
sum<- append(sum,bs)


bs = numeric(0)
population_TR =df_zero[df_zero["remark"]=="Be",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- append(sum,bs)


bs = numeric(0)
population_TR =df_zero[df_zero["remark"]=="V",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- append(sum,bs)


bs = numeric(0)
population_TR =df_zero[df_zero["remark"]=="A",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- append(sum,bs)

bs = numeric(0)
population_TR =df_zero[df_zero["remark"]=="P",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- append(sum,bs)


bs = numeric(0)
population_TR =df_zero[df_zero["remark"]=="R",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}

sum<- append(sum,bs)

bs = numeric(0)
population_TR =df_zero[df_zero["remark"]=="2f",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}

sum<- append(sum,bs)

bs = numeric(0)
population_TR =df_zero[df_zero["remark"]=="4f",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}

sum<- append(sum,bs)

bs = numeric(0)
population_TR =df_zero[df_zero["remark"]=="6f",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$zero, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$zero, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum<- append(sum,bs)


ES_zero[,2]<- sum

# all #
ES_all<- data.frame(matrix(data = NA, nrow = 180, ncol = 2))
colnames(ES_all) = c("treatments", "ef_all")
ES_all[,1] = rep(c('S','Be','V','A','P','R','2f','4f','6f'),each=20)

population_CK =df_all[df_all["remark"]=="CT",]

size_CK = length(population_CK)


bs = numeric(0)
population_TR =df_all[df_all["remark"]=="S",]
size_TR = length(population_TR)
i=1
n_iter=20
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
population_TR =df_all[df_all["remark"]=="Be",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)

bs = numeric(0)
population_TR =df_all[df_all["remark"]=="V",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)

bs = numeric(0)
population_TR =df_all[df_all["remark"]=="A",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)

bs = numeric(0)
population_TR =df_all[df_all["remark"]=="P",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)


bs = numeric(0)
population_TR =df_all[df_all["remark"]=="R",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)

bs = numeric(0)
population_TR =df_all[df_all["remark"]=="2f",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)

bs = numeric(0)
population_TR =df_all[df_all["remark"]=="4f",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)

bs = numeric(0)
population_TR =df_all[df_all["remark"]=="6f",]
size_TR = length(population_TR)
i=1
n_iter=20
for (i in 1:n_iter) {
  k_CK = mean(sample(population_CK$all, size_CK, replace = T), na.rm = TRUE)
  k_TR = mean(sample(population_TR$all, size_TR, replace = T), na.rm = TRUE)
  d_bs = k_TR - k_CK
  bs = append(bs, k_TR - k_CK)
  i=i+1
}
sum_B<- append(sum_B,bs)

ES_all[,2]<- sum_B
ES_all$treatments<- factor(ES_all$treatments,levels=c('S','Be','V','A','P','R','2f','4f','6f'))
ES_zero$treatments<- factor(ES_zero$treatments,levels=c('S','Be','V','A','P','R','2f','4f','6f'))
df_new <- cbind(ES_zero, ES_all[ , !(names(ES_all) %in% "treatments")])
colnames(df_new)<- c('treatments','ef','ef_all')

df_zero$remark<- factor(df_zero$remark,levels=c('CT','S','Be','V','A','P','R','2f','4f','6f'))
df_all$remark<- factor(df_all$remark,levels=c('CT','S','Be','V','A','P','R','2f','4f','6f'))


df_summary <- df_new %>%
  dplyr::group_by(treatments) %>%
  dplyr::summarise(
    mean_zero = mean(ef, na.rm = TRUE),
    ci_lower_zero = mean(ef, na.rm = TRUE) - qt(0.975, df = dplyr::n() - 1) * sd(ef, na.rm = TRUE) / sqrt(dplyr::n()),
    ci_upper_zero = mean(ef, na.rm = TRUE) + qt(0.975, df = dplyr::n() - 1) * sd(ef, na.rm = TRUE) / sqrt(dplyr::n()),
    
    mean_all = mean(ef_all, na.rm = TRUE),
    ci_lower_all = mean(ef_all, na.rm = TRUE) - qt(0.975, df = dplyr::n() - 1) * sd(ef_all, na.rm = TRUE) / sqrt(dplyr::n()),
    ci_upper_all = mean(ef_all, na.rm = TRUE) + qt(0.975, df = dplyr::n() - 1) * sd(ef_all, na.rm = TRUE) / sqrt(dplyr::n())
  )

new_row <- c("CT", rep(0, ncol(df_summary) - 1))
df_summary$treatments<- as.character(df_summary$treatments)
df_summary <- rbind(new_row, df_summary)

df_summary$treatments<- factor(df_summary$treatments,levels=c('CT','S','Be','V','A','P','R','2f','4f','6f'))


ct_zero <- mean(df_zero$zero[df_zero$remark == 'CT'], na.rm = TRUE)
ct_all <- mean(df_all$all[df_all$remark == 'CT'], na.rm = TRUE)
dodge_width <- 0.5
p_without_belowground<- ggplot() +
  # mean data point of without biochar #
  geom_point(data = df_summary,
             aes(x = treatments, y = as.numeric(mean_zero)+ ct_zero),
             shape = 16, size = 2.2,
             position = position_dodge(width = dodge_width)) +
  # mean data point of with biochar #
  #geom_point(data = df_summary,
  #    aes(x = treatments, y = as.numeric(mean_all)+ ct_all),
  #    shape = 16, size = 2.2,
  #   position = position_dodge(width = dodge_width)) +
  # raw datapoints #
  geom_point(data = df_zero,
             aes(x = remark, y = zero),
             colour = '#669aba', alpha = 0.08, size = 2,
             position = position_jitter(width = 0.35)) +
  ylim(0,200)+
  # effect size #
  geom_errorbar(data=df_summary,aes(x = treatments,ymin=as.numeric(ci_lower_zero)+ct_zero,ymax=as.numeric(ci_upper_zero)+ct_zero),width=0.1,size=1,color='#669aba')+
  geom_hline(yintercept = ct_zero, linetype = "dashed", color = "#b3b3b3", size = 0.6)+
  # plain jitter, no dodge
  theme_bw() +
  labs(title='a')+
  labs(x = "factors", y = "without biochar belowground (%)") +
  theme(legend.position = "none")

p_with_belowground<- ggplot() +
  # mean data point of without biochar #
  geom_point(data = df_summary,
             aes(x = treatments, y = as.numeric(mean_all)+ ct_all),
             shape = 16, size = 2.2,
             position = position_dodge(width = dodge_width)) +
  # raw datapoints #
  geom_point(data = df_all,
             aes(x = remark, y = all),
             colour = '#297270', alpha = 0.08, size = 2,
             position = position_jitter(width = 0.35)) +
  # effect size #
  geom_errorbar(data=df_summary,aes(x = treatments,ymin=as.numeric(ci_lower_all)+ct_all,ymax=as.numeric(ci_upper_all)+ct_all),width=0.1,size=1,color='#297270')+
  geom_hline(yintercept = ct_zero, linetype = "dashed", color = "#b3b3b3", size = 0.6)+
  geom_hline(yintercept = ct_all, linetype = "dashed", color = "black", size = 0.6)+
  # plain jitter, no dodge
  theme_bw() +
  labs(title='c')+
  ylim(0,200)+
  labs(x = "factors") +
  theme(legend.position = "none")

# Combine with patchwork
# 5inch+7inch pdf save
library(patchwork)
p_without_wsa+ regression_WSA_without+p_with_wsa+regression_wsa_with+plot_layout(ncol = 4, widths = c(2,1.5,2, 1.5), guides = "collect")
p_without_whc+regression_whc_without+p_with_whc+regression_whc_with+plot_layout(ncol = 4, widths = c(2,1.5,2, 1.5), guides = "collect")
p_without_pH+regression_pH_without+p_with_pH+regression_pH_with+plot_layout(ncol = 4, widths = c(2,1.5,2, 1.5), guides = "collect")
p_without_medicago+regression_medicago_without+p_with_medicago+regression_medicago_with+plot_layout(ncol = 4, widths = c(2,1.5,2, 1.5), guides = "collect")
p_without_belowground+regression_below_without+p_with_belowground+regression_below_with+plot_layout(ncol = 4, widths = c(2,1.5,2, 1.5), guides = "collect")

(p_without_aboveground+p_with_aboveground) +
  plot_layout(ncol = 2, guides = "collect")
