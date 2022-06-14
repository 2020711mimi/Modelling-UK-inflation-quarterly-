rm(list=ls())
library("earth")
install_git("https://github.com/ccolonescu/PoEdata")
library(PoEdata)
install.packages("earth")

install.packages("formatR", repos = "http://cran.rstudio.com")
library(formatR)

# -------------------------------------------------------------------------


data("food")

N <- 40
sde <- 50
x <- food$income
nrsim <- 1000
b1 <- 100
b2 <- 10
vb2 <- numeric(nrsim) #stores the estimates of b2
for (i in 1:nrsim){
  set.seed(12345+10*i)
  y <- b1+b2*x+rnorm(N, mean=0, sd=sde)
  mod7 <- lm(y~x)
  vb2[i] <- coef(mod7)[[2]]
}
mb2 <- mean(vb2)
seb2 <- sd(vb2)

# -------------------------------------------------------------------------

library(caret)
x = round(rnorm(200, 5, 5))
y= rnorm(200, 2 + 0.4*x, 0.5)
theData <- data.frame(id=1:200,x, y)
# configure caret training parameters to 200 bootstrap samples
fitControl <- trainControl(method = "boot",
                           number = 200)
fit <- train(y ~ x, method="glm",data=theData,
             trControl = fitControl)
# print output object
fit
# print first 10 resamples 
fit$resample[1:10,]

# -------------------------------------------------------------------------

str(iris)
library(AppliedPredictiveModeling)

# -------------------------------------------------------------------------

library(earth)
data(etitanic)
head(model.matrix(survived ~ ., data = etitanic))

dummies <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))


# -------------------------------------------------------------------------

data(mdrr)
data.frame(table(mdrrDescr$nR11))
head(mdrrDescr)

nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)

nzv[nzv$nzv,][1:10,]

nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)
dim(mdrrDescr)

descrCor <-  cor(filteredDescr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)


highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

# -------------------------------------------------------------------------

runs <- 100000
#one.trail simulates a single round of toss 10 coins
#and returns true if the number of heads is > 3
one.trial <- function(){
  sum(sample(c(0,1),10,replace=T)) > 3
}
#now we repeat that trial 'runs' times.
mc.binom <- sum(
  replicate(runs,one.trial())
  )/runs

# -------------------------------------------------------------------------
library(formatR)

tidy_eval("On the predictive content of the PPI on.Rmd")
