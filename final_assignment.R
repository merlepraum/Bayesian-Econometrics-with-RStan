
# R code for assignment EBS2043
# Author: Merle Maria Praum
# Student number: --

library(car)
library(readxl)
library(rstan)
library(coda)
library(bayesplot)
source("final_assignment.stan")


#analyse data across Hong Kong to get reasonable priors
HK_data <- read_excel("DATA_Hong_Kong.xlsx")
HK_year <- as.numeric(unlist(HK_data[,1]))
HKinv <- as.numeric(unlist(HK_data[,6]))
HKgdp <- as.numeric(unlist(HK_data[,7]))
HKlog <- log(HKgdp)
HKgov <- as.numeric(unlist(HK_data[,10]))
T <- length(gdp)
plot(HKlog)

#get priors for whole set
lmHK <- lm(HKlog ~ HK_year + HKinv + HKgov)
summary(lmHK)

#get priors for 1995 to 2021
lmHK <- lm(HKlog ~ HK_year + HKinv + HKgov, subset = 26:52)
summary(lmHK)


# data for Singapore
SG_data <- read_excel("DATA_Singapore.xlsx")
SG_data <-SG_data[1:52,]
SG <- as.data.frame(SG_data)
SG_year <- as.numeric(SG$Year)
SGinv <- as.numeric(SG$`Investment as % of GDP`)
SGlog_gdp <- log(as.numeric(SG$`GDP current US$`))
SGgov <-as.numeric(SG$`General government final consumption expenditure (% of GDP)`)
 
gdpchgTS <- ts(SGlog_gdp, start=1, end=51, frequency=1) #time series
plot(gdpchgTS)

#summary statistics
summary(SGinv)
sd(SGinv)
summary(SGgov)
sd(SGgov)
summary(SGlog_gdp)
sd(SGlog_gdp)

T <- length(SGlog_gdp)



#STAN IMPLEMENTATION 

data_SG_stan <- list(N=T, year=SG_year, inv=SGinv, gov=SGgov, log_gdp = SGlog_gdp)

# Run the model and examine results
fit_all <- stan(model_code = stan, #using source
            data = data_SG_stan,
            iter = 8000,
            warmup = 500,
            thin = 10,
            chains = 4)

posterior <- extract(fit) # obtains  full posterior of our parameters to see more than the summary of results.
str(posterior)

# Summary of Bayesian inference results
print(fit_all, pars=c('a', 'b', 'c', 'd', 'sigma'), digits=3) 
# Visualize
monitor(fit_all)

#traces
mcmc_trace(fit_all)


#define coefficients
a <-extract(fit_all)$a
b <- extract(fit_all)$b
c <- extract(fit_all)$c
d <- extract(fit_all)$d


#histograms #most likely signs should be a close to normal distribution with the mean 
#as the most likely estimate
dev.off()
mcmc_hist(fit_all)


#probability that estimate is above 0
p_b2 <- sum(c>0)/length(c) #investments
p_b3 <- sum(d>0)/length(d) #government spending


#autocorrelation
mcmc_acf(fit_all)
mcmc_pairs(fit_all)



#split data in two and repeat process (with new priors)
#create new list for half the years
data_split <- split(as.data.frame(data_SG_stan), f=SG$year>1994)$`TRUE` #from 1995 to 2021
S <- length(data_split$year)
stan_data_split <- list(N=S, year = data_split$year, inv = data_split$inv, gov = data_split$gov, log_gdp = data_split$log_gdp)

# check Stan model to make sure a file is written
stanc("~/Documents/Year3period3 skills/split_set.stan")

# Estimate the model using STAN and report estimated output.
stan_model_split <- "~/Documents/Year3period3 skills/split_set.stan"

# Run the model and examine results
fit_split <- stan(file = stan_model_split,
            data = stan_data_split,
            iter = 8000,
            warmup = 500,
            thin = 10,
            chains = 4)

posterior <- extract(fit_split) # obtains  full posterior of our parameters to see more than the summary of results.
str(posterior)
posterior

# Summary of Bayesian inference results
print(fit_split, pars=c('alpha', 'beta1', 'beta2', 'beta3', 'sigma'), digits=3) 

# Visualize
monitor(fit_split)

#define coefficients
alpha <- extract(fit_split)$alpha
beta1 <- extract(fit_split)$beta1
beta2 <- extract(fit_split)$beta2
beta3 <- extract(fit_split)$beta3


#histograms #most likely signs should be a close to normal distribution with the mean 
#as the most likely estimate
dev.off()
mcmc_hist(fit_split)


#probability that estimate is above 0
p_b2 <- sum(beta2>0)/length(beta2) #investments
p_b3 <- sum(beta3>0)/length(beta3) #government spending

#traces
mcmc_trace(fit_split)

# # Geweke test (did not work)
# betas = extract(fit_split, pars='beta1')$beta1
# betas.mcmc = as.mcmc(betas)
# plot(betas.mcmc)
# mcmcUpgrade(betas.mcmc)
# geweke.diag(betas.mcmc, 0.1, 0.5) 
# 


