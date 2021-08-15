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
print(sd(slope)/mean(slope_se))
print(mean(cover_95))

n0 = 20
n1 = 30

y_0 = rnorm(n0,2,5)
y_1 = rnorm(n1,2,5)

meany0 = mean(y_0); sdy0 = sd(y_0)/sqrt(n0)
meany1 = mean(y_1); sdy1 = sd(y_1)/sqrt(n1)

diff = meany0 - meany1

sdcomp = sqrt(sdy0^2+sdy1^2)  ## Combined SE for the difference measurement

