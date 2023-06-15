

library(car)
data(SLID, package = "carData")
SLID <- SLID[complete.cases(SLID),]

library(GGally)
ggpairs(SLID)

#### Part 2 ####
model1 <- lm(wages ~ education, data = SLID)
print(model1)
summary(model1)

model1_df <- data.frame(obs = model1$model$wages, fit = model1$fitted.values, res = model1$residuals)
ggplot(data = model1_df) + geom_point(mapping = aes(x = fit, y = res)) + 
  labs(x = "Fitted values", y = "Residuals")

sum(model1$residuals^2) # rss
model1$df.residual #df
sum((model1$model$wages - mean(model1$model$wages))^2) # rss_null

(sum((model1$model$wages - mean(model1$model$wages))^2) - sum(model1$residuals^2))/(summary(model1)$sigma^2) # chisq
qchisq(p = 0.05, df = 1, lower.tail = FALSE) # limit

# Critical value for the z-test, upper bound, 5 % (two-sided, så 2.5 % on each side)
c(qnorm(0.025, lower.tail = TRUE), qnorm(0.025, lower.tail = FALSE))  # lower, upper
# Critical value for the chisq-test, upper bound, 5 %
qchisq(0.05, df = 1, lower.tail = FALSE)

anova(model1)

summary(model1)$r.squared

sqrt(summary(model1)$r.squared)


y <- SLID$wages
x <- cbind(rep(1, length(SLID$education)), SLID$education)
sum(t(x-mean(x)) %*% (y-mean(y)))/sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2))


#### Part 3 ####
model2 <- lm(wages ~ education + age, data = SLID)
print(model2)
summary(model2)

# net:
lm(wages ~ education, data = SLID)$coefficients[-1]
lm(wages ~ age, data = SLID)$coefficients[-1]
# gross:
model2$coefficients[-1]


model3 <- lm(wages ~ sex + language + age + education, data = SLID)

summary(model3)
anova(model3)


#### Part 4 ####

model3a <- lm(wages ~ sex + language, data = SLID)
print(model3a)
summary(model3a)
anova(model3a)

model3b <- lm(wages ~ sex + language, data = SLID, contrasts = list(language = "contr.sum", sex = "contr.sum"), x = TRUE)
print(model3b)
summary(model3b)
anova(model3b)

model4a <- lm(wages ~ sex + age + sex*age, data = SLID, x = TRUE)
print(model4a)
summary(model4a)
anova(model4a)

model4a_df <- data.frame(age = range(model4a$model$age), 
                        female = model4a$coefficients[1] + model4a$coefficients[3]*range(model4a$model$age),
                        male = model4a$coefficients[1] + model4a$coefficients[3]*range(model4a$model$age) + 
                          model4a$coefficients[2] + model4a$coefficients[4]*range(model4a$model$age))
ggplot(data = model4a_df) + geom_line(mapping = aes(x = age, y = female, col = "Female")) + 
  geom_line(mapping = aes(x = age, y = male, col = "Male")) + labs(x = "Age in years", y = "Wages") +
  theme(legend.title = element_blank())

model4b <- lm(wages ~ sex + age + sex*age, data = SLID, contrasts = list(sex = "contr.sum"), x = TRUE)
print(model4b)
summary(model4b)
anova(model4b)

model4b_df <- data.frame(age = range(model4b$model$age), 
                         female = model4b$coefficients[1] + model4b$coefficients[3]*range(model4b$model$age) +
                           model4b$coefficients[2] + model4b$coefficients[4]*range(model4b$model$age),
                         male = model4b$coefficients[1] + model4b$coefficients[3]*range(model4b$model$age) - 
                           model4b$coefficients[2] - model4b$coefficients[4]*range(model4b$model$age))
ggplot(data = model4b_df) + geom_line(mapping = aes(x = age, y = female, col = "Female")) + 
  geom_line(mapping = aes(x = age, y = male, col = "Male")) + labs(x = "Age in years", y = "Wages") +
  theme(legend.title = element_blank())
  
  
#### Part 5 ####
model5 <- lm(wages ~ sex + age + language + I(education^2), data = SLID)
summary(model5)
model6 <- lm(wages ~ age + language + language*age, data = SLID)
summary(model6)
model7 <- lm(wages ~ education -1, data = SLID)
summary(model7)









########## med myglm ###############

library(car)
data(SLID, package = "car")
SLID <- SLID[complete.cases(SLID),]

library(GGally)
ggpairs(SLID)

#### Part 2 ####
model1 <- myglm(wages ~ education, data = SLID)
print(model1)
summary(model1)

model1_df <- data.frame(obs = model1$model$wages, fit = model1$fitted.values, res = model1$residuals)
ggplot(data = model1_df) + geom_point(mapping = aes(x = fit, y = res)) + 
  labs(x = "Fitted values", y = "Residuals")

sum(model1$residuals^2) # rss
model1$df.residual #df
sum((model1$model$wages - mean(model1$model$wages))^2) # rss_null

(sum((model1$model$wages - mean(model1$model$wages))^2) - sum(model1$residuals^2))/(summary(model1)$sigma^2) # chisq
qchisq(p = 0.05, df = 1, lower.tail = FALSE) # limit

# Critical value for the z-test, upper bound, 5 % (two-sided, så 2.5 % on each side)
c(qnorm(0.025, lower.tail = TRUE), qnorm(0.025, lower.tail = FALSE))  # lower, upper
# Critical value for the chisq-test, upper bound, 5 %
qchisq(0.05, df = 1, lower.tail = FALSE)

anova(model1)

summary(model1)$r.squared

sqrt(summary(model1)$r.squared)



#### Part 3 ####
model2 <- myglm(wages ~ education + age, data = SLID)
print(model2)
summary(model2)

# net:
myglm(wages ~ education, data = SLID)$coefficients[-1]
myglm(wages ~ age, data = SLID)$coefficients[-1]
# gross:
model2$coefficients[-1]


model3 <- myglm(wages ~ sex + language + age + education, data = SLID)

summary(model3)
anova(model3)


#### Part 4 ####
model5 <- myglm(wages ~ sex + age + language + I(education^2), data = SLID)
summary(model5)
model6 <- myglm(wages ~ age + language + language*age, data = SLID)
summary(model6)
model7 <- myglm(wages ~ education -1, data = SLID)
summary(model7)



#### Part OPTIONAL ####

model3a <- myglm(wages ~ sex + language, data = SLID)
print(model3a)
summary(model3a)
anova(model3a)

model3b <- myglm(wages ~ sex + language, data = SLID, contrasts = list(language = "contr.sum", sex = "contr.sum"))
print(model3b)
summary(model3b)
anova(model3b)

model4a <- myglm(wages ~ sex + age + sex*age, data = SLID, x = TRUE)
print(model4a)
summary(model4a)
anova(model4a)

model4a_df <- data.frame(age = range(model4a$model$age), 
                         female = model4a$coefficients[1] + model4a$coefficients[3]*range(model4a$model$age),
                         male = model4a$coefficients[1] + model4a$coefficients[3]*range(model4a$model$age) + 
                           model4a$coefficients[2] + model4a$coefficients[4]*range(model4a$model$age))
ggplot(data = model4a_df) + geom_line(mapping = aes(x = age, y = female, col = "Female")) + 
  geom_line(mapping = aes(x = age, y = male, col = "Male")) + labs(x = "Age in years", y = "Wages") +
  theme(legend.title = element_blank())

model4b <- myglm(wages ~ sex + age + sex*age, data = SLID, contrasts = list(sex = "contr.sum"), x = TRUE)
print(model4b)
summary(model4b)
anova(model4b)

model4b_df <- data.frame(age = range(model4b$model$age), 
                         female = model4b$coefficients[1] + model4b$coefficients[3]*range(model4b$model$age) +
                           model4b$coefficients[2] + model4b$coefficients[4]*range(model4b$model$age),
                         male = model4b$coefficients[1] + model4b$coefficients[3]*range(model4b$model$age) - 
                           model4b$coefficients[2] - model4b$coefficients[4]*range(model4b$model$age))
ggplot(data = model4b_df) + geom_line(mapping = aes(x = age, y = female, col = "Female")) + 
  geom_line(mapping = aes(x = age, y = male, col = "Male")) + labs(x = "Age in years", y = "Wages") +
  theme(legend.title = element_blank())























