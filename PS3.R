############PS_3Q1C#############################
################################################
rm(list=ls())
setwd("C:/Users/Circle/OneDrive - London Business School/courses/Econometrics")
library(forecast)
library(sandwich)
library(lmtest) 
library(bbmle)
library(aod)
library(ivreg)
data <- read.csv('SP500Index.csv', header = TRUE)
sp_0 <- data[1,2]
x_t <- log(data[,2]/sp_0)
T <- nrow(data)
"""

# set the model
model <- function(sigsqr, dta)
  {
  M <- 0
  for (i in 2: T)
    {su <- (x_t[i]-x_t[i-1]- dta)^2
     M <- M + su
        }
  l <- (sigsqr^(-T/2))* ((2 * pi)^(-T/2))* exp((-1/2) * sigsqr^(-1)* M )
  logl <-  log (l)
return(logl)
  }
#
fit_norm <- mle2(model, start = list(sigsqr = 0.015, dta = 0), lower = list(sigsqr = 0.000000001, dta = 0), upper = list(sigsqr = 1, dta = 0.5))

summary(fit_norm)
"""
#Calculate the estimators numerically
theta <- 1/T * x_t[T]

M <- 0
for (i in 2: T)
{su <- (x_t[i]-x_t[i-1]- theta)^2
M <- M + su
}
std <-  sqrt(M/T)
  
  
############PS_3Q5#############################
################################################

###############a###############################

rm(list=ls())
setwd("C:/Users/Circle/OneDrive - London Business School/courses/Econometrics")

df <- read.csv('PS4data.csv', header = TRUE)
c_t <- (df[,3] + df[,4] )/ df[,7]
c_t_1 <- c_t[2:217]
c_t_2 <- c_t[3:218]
c_t_3 <- c_t[4:219]
c_t_4 <- c_t[5:220]
ctlag <- data.frame(c_t_1, c_t_2, c_t_3, c_t_4 )
ctlag <- as.matrix(ctlag) # Create lag variables


fit <- lm(c_t[1:216] ~ ctlag)
summary(fit)
fit$coefficients
wald.test(vcov(fit), b = coef(fit), Terms = 3:5) #pval < 0.01 we reject the null

###############c###############################
rm(list=ls())
setwd("C:/Users/Circle/OneDrive - London Business School/courses/Econometrics")

df <- read.csv('PS4data.csv', header = TRUE)
c_t <- (df[,3] + df[,4] )/ df[,7]
y_t <- df[,6]/df[,7]
y_t_1 <- y_t[2:215]
c_t_1 <- c_t[2:215]
c_t_2 <- c_t[3:216]
c_t_3 <- c_t[4:217]
c_t_4 <- c_t[5:218]
c_t_5 <- c_t[6:219]
c_t_6 <- c_t[7:220]
c_t <- c_t[1:214] #calibrate the length of the data
y_t <- y_t[1:214]

#create independent and dependent variables and iv(calibrate the data length to 220-6)
y <- log(c_t/c_t_1)
x <- log(y_t/y_t_1)

iv_1 <- log(c_t_2/c_t_3)
iv_2 <- log(c_t_3/c_t_4)
iv_3 <- log(c_t_4/c_t_5) 
iv_4 <- log(c_t_5/c_t_6)

fit_1 <- lm( x~iv_1+iv_2+iv_3+iv_4) #1-st stage
summary(fit_1)
x_hat <- fit_1$fitted.values
fit_2 <- lm(y~ x_hat) # 2-nd stage
summary(fit_2)

###############d###############################
epi <- fit_1$residuals #extract residuals from the 1-st stage
exo <- lm(y ~ x + epi)
summary(exo) 

fit_2sls <- ivreg(y~x|iv_1+iv_2+iv_3+iv_4)
summary(fit_2sls)