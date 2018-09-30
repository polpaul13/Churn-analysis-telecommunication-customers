library(corrplot)
numeric.var <- sapply(churntest2, is.numeric)
corr.matrix <- cor(churntest2[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")



churntest2 <-churn_data
churntest2 <- as.data.frame(churntest2)

churntest2$daychargeperminute <- churntest2$`Day Charge`/churntest2$`Day Mins`
churntest2$evechargeperminute <- churntest2$`Eve Charge`/churntest2$`Eve Mins`
churntest2$nightchargeperminute <-churntest2$`Night Charge`/churntest2$`Night Mins`
churntest2$intchargeperminute <- churntest2$`Intl Charge`/churntest2$`Intl Mins`

churntest2$`Day Charge` <- NULL
churntest2$`Eve Charge` <- NULL
churntest2$`Night Charge` <- NULL
churntest2$`Intl Charge` <- NULL

str(churntest2)

churntest2$group_Account_Length <- NULL
as.data.frame(churntest2)

sapply(churntest2, function(x) sum(is.nan(x)))
churntest2<- churntest2[complete.cases(churntest2), ]
sapply(churntest2, function(x) sum(is.nan(x)))

mylogit4 <- glm(Churn ~ .,data=churntest2, family = "binomial")
summary(mylogit4)

anova(mylogit4,test="Chisq")

mfull <- glm(Churn~.,data=churntest2 , family = "binomial")
mnull <- glm(Churn~1 ,data=churntest2 , family ="binomial")
mylogit4_forward <- step(mnull, scope=list(lower=mnull,upper=mfull), direction='forward')
summary(mylogit4_forward)
mylogit4_forward <-glm(formula = Churn ~ `Int'l Plan` + `CustServ Calls` + `Day Mins` + 
                         `VMail Plan` + `Eve Mins` + `Intl Mins` + `Intl Calls` + 
                         `Night Mins` + `Day Calls`, family = "binomial", 
                       data = churntest2)
summary(mylogit4_forward)

mylogit4_back <- step(mfull,direction="back")
summary(mylogit4_back)

require(glmnet)
x <- model.matrix(Churn~.,data=churntest2)[,-1]
y <- ifelse(churntest2$Churn=="NO",1,0)
cv.out <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse" )
plot(cv.out)
lambda_min <- cv.out$lambda.min
lambda_1se <- cv.out$lambda.1se
coef(cv.out,s=lambda_1se)
plot(cv.out$glmnet.fit, xvar = "lambda")
abline(v=log(c(cv.out$lambda.min, cv.out$lambda.1se)), lty =2)

mylogit4_lasso <- glm(Churn~1 +`CustServ Calls` +`Int'l Plan` +`VMail Plan` +`Intl Charge`
                       +`Day Charge`  +`Eve Charge`+ `Intl Calls` ,
                      data=churntest2,family = "binomial")
summary(mylogit4_lasso)

library(car)
vif(mylogit4_forward)
alias(mylogit4_back)

install.packages("pscl")
library(pscl)
pR2(mylogit4_forward)

#####centre the covariates


index <- sapply(churn_data, class) == "numeric"
churn_data_numeric <- churn_data[,index]

churn_data_numeric <- as.data.frame(scale(churn_data_numeric, center = TRUE, scale = F))

sapply(churn_data_numeric,mean)
sapply(churn_data_numeric,sd)

churn_data_numeric$Churn<-churn_data$Churn
churn_data_numeric$`Intl Plan`<-churn_data$`Int'l Plan`
churn_data_numeric$`VMail Plan`<-churn_data$`VMail Plan`
churn_data_numeric$State<-churn_data$State
churn_data_numeric$Gender<-churn_data$Gender

churn_data_final <- churn_data_numeric
str(churn_data_final)

churn_data_final$`Day Charge` <- NULL
churn_data_final$`Eve Charge` <- NULL
churn_data_final$`Night Charge` <- NULL
churn_data_final$`Intl Charge` <- NULL

mfull <- glm(Churn~.,data=churn_data_final , family = "binomial")
mnull <- glm(Churn~1 ,data=churn_data_final , family ="binomial")
mylogit4_forwardfinal <- step(mnull, scope=list(lower=mnull,upper=mfull), direction='forward')
summary(mylogit4_forwardfinal)
mylogit4_forwardfinal <- glm(formula = Churn ~ `Intl Plan` + `CustServ Calls` + `Day Mins` + 
                               `VMail Plan` + `Eve Mins` + `Intl Mins` + `Intl Calls` + 
                               `Night Mins`, family = "binomial", data = churn_data_final)
summary(mylogit4_forwardfinal)
exp(coef(mylogit4_forwardfinal))
exp(cbind(OR = coef(mylogit4_forwardfinal), confint(mylogit4_forwardfinal)))

newdata1 <- with(churn_data_final,
                 data.frame(CustServCalls  = mean(CustServCalls) , DayMins  = mean(DayMins),
                            EveMins   = mean(EveMins), IntlMins = mean(IntlMins), VMailMessage=mean(VMailMessage),
                            IntlCalls  = mean(IntlCalls), NightMins = mean(NightMins), 
                            IntlPlan= factor(1:2)))


newdata1$levelprop <- predict(mylogit4_forward, newdata = newdata1, type = "response")
newdata1

newdata2 <- with(churn_data_final,
                 data.frame(`CustServ Calls` = rep(seq(from = 1, to = 20, length.out = 20), 2),
                            `VMail Plan` = factor(rep(1:2, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit4_forward, newdata = newdata2, type="link", se=TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

with(mylogit4_forwardfinal, null.deviance - deviance)
with(mylogit4_forwardfinal, df.null - df.residual)
with(mylogit4_forwardfinal, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mylogit4_forwardfinal)

attributes(mylogit4_forwardfinal)
