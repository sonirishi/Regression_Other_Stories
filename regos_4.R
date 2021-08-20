rm(list=ls(all=TRUE))
setwd('E:/Documents/Practice/gelman/')
library(rstanarm)

df = read.csv('kidiq.csv')

df$kid_score=df$X

model <- stan_glm(kid_score ~ mom_hs,data=df)

modsim <- as.matrix(model)

## These should match, they are :)
apply(modsim,2,median)

print(model$coefficients)

print(model$ses)

apply(modsim,2,mad)

## sim_disp <- sample(modsim,10)

## This creates a range of plot for all simulated values of coeff

plot(df$kid_score,x=df$mom_hs)
abline(model$coefficients[1],model$coefficients[2])
for(i in 1:nrow(modsim)){
  abline(modsim[i,1],modsim[i,2])
}
model <- stan_glm(kid_score ~ mom_hs+mom_iq,data=df)
mom_bar <- mean(df$mom_hs)
plot(df$mom_iq,df$kid_score)
curve(cbind(1,x,mom_bar) %*% coef(model), add=TRUE)

df <- read.table('introclass.txt',header = TRUE)

model <- stan_glm(final ~ midterm, data=df)

summary(model)

sims = as.matrix(model)

predicted = predict(model)

resid = df$final - predicted

plot(resid,predicted)

plot(resid,df$final)

## fake data simulation

df$final_fake = 64.8 + 0.7*df$midterm + rnorm(nrow(df),15,1.6)

model <- stan_glm(final_fake ~ midterm, data=df)

summary(model)

fakesim = as.matrix(model)

predicted_fake = colMeans(fakesim[,1] + fakesim[,2] %*% t(df$midterm))

new_pred_fake =  fakesim[,1:2] %*% 
  rbind(rep(1,52),as.vector(t(df$midterm)))

new_pred_fake_m = apply(new_pred_fake,2,mean)

resid_fake = df$final_fake - predicted_fake

## plots shouldnt break

plot(df$final_fake,resid_fake)

plot(predicted_fake,resid_fake)

df <- read.table('newcomb.txt',header = TRUE)

model = stan_glm(y ~ 1, data = df)

summary(model)

sims = as.matrix(model)

sim_df = matrix(rep(0,4000*66),4000,66)

### using simulated vals for creating data

for (i in 1:nrow(sims)){
  sim_df[i,] = rnorm(66,sims[i,1],sims[i,2])
}

## or use posterior predict
sim_df_1 = posterior_predict(model)

plot(density(df$y))

for (i in 1:30){
  lines(density(sim_df_1[i,]),col='red')
}

min_data = apply(sim_df_1,1,min)

print(sum(min_data - min(df$y) > 0))
