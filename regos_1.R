setwd('E:/Documents/Practice/gelman/')
library(rstanarm)

df = read.csv('hills.csv')

model1 = stan_glm(vote~growth,data = df)

summary(model1)

a = 46
b = 3
sigma = 3.9

## Fake Data

stanrun = function(){
  
  df1 = as.data.frame(cbind(a+b*df$growth+rnorm(nrow(df),0,sigma),df$growth))
  colnames(df1) = c('vote','growth')
  
  model2 = stan_glm(vote~growth,data = df1, refresh=0)
  return(model2)
}

slope = rep(0,1000)
slope_se = rep(0,1000)
cover_68 = rep(0,1000)
cover_95 = rep(0,1000)

for (i in 1:1000){
  modelobj = stanrun()
  slope[i] = modelobj$coefficients[2]  
  slope_se[i] = modelobj$ses[2]
  cover_68[i] = abs(b - slope[i]) < qt(0.84,14)*slope_se[i]
  cover_95[i] = abs(b - slope[i]) < qt(0.975,14)*slope_se[i]
}

print(mean(cover_68))

print(mean(cover_95))
