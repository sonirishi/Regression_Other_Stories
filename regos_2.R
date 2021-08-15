rm(list=ls(all=TRUE))

library(rstanarm)

setwd('E:/Documents/Practice/gelman/')

df = read.csv('hills.csv')

residualcv = rep(0,nrow(df))

sigmaest = rep(0,nrow(df))

### LOO CV

for (i in 1:nrow(df)){
  
  model = stan_glm(vote~growth,data = df[-i,],refresh=0)
  
  residualcv[i] = predict(model,newdata=data.frame(growth=c(df[i,"growth"]))) - df[i,"vote"]
  
  sim = as.matrix(model); sigmaest[i] = mean(sim[,3])

}
print(mean(sigmaest))
print(sd(residualcv))

model1 = stan_glm(vote~growth,data = df)

val = posterior_predict(model,newdata=data.frame(growth=c(2)))

print(median(val))

print(sum(val>50)/nrow(val))  ## win probability 

print(sd(val))

x = rnorm(4000,2,0.3)   #uncertainty in growth parameter if at all

val = rnorm(4000,46.3+3*x,4)

print(median(val))

print(sum(val>50)/length(val))

df_earn <- read.csv('earnings.csv')

earnfit <- stan_glm(weight~height,data=df_earn)

summary(earnfit)
## simulate a+b*x - uncertainty of a and b
y1 = posterior_linpred(earnfit,newdata=data.frame(height=c(70)))
## simulate a+b*x+error, uncertainty of a and b and error
y2 = posterior_predict(earnfit,newdata=data.frame(height=c(70)))

## uncertainty in only error
y3 = rnorm(4000,
             earnfit$coefficients[1] + earnfit$coefficients[2]*70,29)
## same as above, above is just the shifted version
y4 = earnfit$coefficients[1] + earnfit$coefficients[2]*70 + 
  rnorm(4000,0,29)

y5 = posterior_linpred(earnfit,newdata=data.frame(height=c(72)))

mean(y1)
sd(y1)
mean(y2)
sd(y2) 
mean(y3)
sd(y3)
mean(y4)
sd(y4)
mean(y5)
sd(y5)