setwd('E:/Documents/Practice/gelman/')
library(rstanarm)

df = read.csv('congress.csv')

head(df)

mod = stan_glm(v88_adj ~ v86_adj + inc88, data=df)

modsim <- as.matrix(mod)/

summary(mod)

df_test <- data.frame(v86_adj=df$v88_adj,inc88=df$inc90)

fit_new <- posterior_predict(mod,newdata = df_test)

dim(fit_new)

dempred <- rowSums(fit_new>0.5)

print(apply(modsim,2,mean))
print(apply(modsim,2,median))
print(apply(modsim,2,mad))

summary(mod)

cor(modsim)   # high correlation between some features, 
# these are not captured in output but captured in likelihood calc

df_summ <- apply(fit_new,2,mean)

df_summ1 <- apply(fit_new,2,sd)

lmmod = lm(v88_adj ~ v86_adj + inc88, data=df)

lmfit <- predict(lmmod,newdata = df_test)

print(sum(lmfit>0.5))

df <- read.csv('nes.csv')

model1 <- stan_glm(partyid7 ~ real_ideo + race_adj + factor(age_discrete) +
                     educ1 + female + income,data=df)

model_coeff <- as.data.frame(model1$coefficients)
model_coeff_se <- as.data.frame(model1$ses)

### running multiple cross sectional models and 
## comparing coefficients across time
## they look in range for current model

for (yr in unique(df$year)){
  df_1 <- df[df$year == yr,]
  mod <- stan_glm(partyid7 ~ real_ideo + race_adj + factor(age_discrete) +
                    educ1 + female + income,data=df, refresh=0)
  temp_coeff <- as.data.frame(mod$coefficients)
  temp_se <- as.data.frame(mod$ses)
  model_coeff <- cbind.data.frame(model_coeff,temp_coeff)
  model_coeff_se <- cbind.data.frame(model_coeff_se,temp_se)
}
par(mar=c(.1,.1,.1,.1))
plot(y=array(model_coeff[1,2:ncol(model_coeff)]),x=unique(df$year))

apply(model_coeff,1,mean)
apply(model_coeff,1,max)
apply(model_coeff,1,min)

apply(model_coeff_se,1,mean)
apply(model_coeff_se,1,max)
apply(model_coeff_se,1,min)
