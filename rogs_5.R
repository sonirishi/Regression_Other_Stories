setwd('D:/datascience/Practice/gelman')

library(rstanarm)

df = read.table('unemp.txt',header = TRUE)

df$lag = c(NA,df$y[1:nrow(df)-1])

fit_ts = stan_glm(y ~ lag, data = df)

summary(fit_ts)

df = read.table('kidiq.txt',header=TRUE)

model = stan_glm(X_score ~ mom_hs + mom_iq, data = df)

df_noise = df

for (i in 1:5){
  df_noise[paste0('noise',i)] = rnorm(nrow(df))
}

model_noise = stan_glm(X_score ~ mom_hs + mom_iq + noise1 + 
                         noise2 + noise3 + noise4 + noise5 , data = df_noise)

summary(model_noise)

model2 = stan_glm(X_score ~ mom_hs, data = df)

loo_1 = loo(model2)

print(loo_1)

for (i in 1:nrow(df)){
  model = stan_glm(X_score ~ mom_hs, data = df[-i,])
  data = posterior_predict(model,newdata = df[i,])
}

model2 = stan_glm(X_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data = df)

df$inter = df$mom_hs*df$mom_iq

model3 = stan_glm(X_score ~ mom_hs + mom_iq + inter, data = df)

summary(model3)

summary(model2)

df = read.delim('arsenic.txt',header=TRUE,sep=",")

logit = stan_glm(switch ~ arsenic + dist + dist100 + assoc + educ + educ4,
                 family = binomial(link = 'logit'), data = df)

summary(logit)

df$resid = logit$residuals

quantile(df$arsenic)

df$z_aresnic = cut(df$arsenic, breaks=50)

library(dplyr)

df_summ = df %>% group_by(z_aresnic) %>% summarise(mean_resid = mean(resid), 
                                                   mad_resid = mad(resid))

plot(x = df_summ$z_aresnic, y = df_summ$mean_resid)

df$fold = sample(1:3,nrow(df),replace=TRUE)

for(i in 1:3){
  logit = stan_glm(switch ~ arsenic + dist + dist100 + assoc + educ + educ4,
                   family = binomial(link = 'logit'), data = df[df$fold != i,])
  pred_df = df[df$fold == i,]
  pred_switch = predict(logit, newdata = pred_df, type='response')
  resid = df[df$fold == i,"switch"] - pred_switch
  if (i == 1){
    resid_all = resid
  }else{
    resid_all = c(resid_all,resid)
  }
}
 
df$residualoof = resid_all

df_summ1 = df %>% group_by(z_aresnic) %>% summarise(mean_resid = mean(residualoof), 
                                                   mad_resid = mad(residualoof))

### binned residual out of fold and train data (oof being more useful) 

plot(x=df_summ1$z_aresnic,y=df_summ1$mean_resid)
points(x=df_summ$z_aresnic,y=df_summ$mean_resid,col='red')
