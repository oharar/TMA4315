setwd("~/Dropbox/Teaching/Exams/TMA4315h24/")

Data <- read.csv("Exams/logitprobit.csv")

Data <- data.frame(
  Beginyear = seq(1935, 1990, by=5),
  probit = c(6, 3, 22, 50, 53, 41, 43, 48, 45, 93, 98, 127),
  logit = c(0, 1, 6, 15, 23, 27, 41, 61, 72, 147, 215, 311)
)
Data$PropProbit <- Data$probit/(Data$probit+Data$logit)

png("~/Dropbox/Teaching/Exams/probitlogitData.png")
plot(Data$Year, Data$PropProbit)
dev.off()

m1 <- glm(cbind(Data$probit, Data$logit) ~ Data$Year, 
           family=binomial("logit"))
m2 <- glm(cbind(Data$probit, Data$logit) ~ Data$Year, 
           family=binomial("probit"))
predL <- predict(m1, type = "response")
predP <- predict(m2, type = "response")

plot(fitted(m1), resid(m1))
plot(Data$Year, resid(m1))

m1q <- glm(cbind(Data$probit, Data$logit) ~ Data$Year + I(Data$Year^2), 
          family=binomial("logit"))
m2q <- glm(cbind(Data$probit, Data$logit) ~ Data$Year + I(Data$Year^2), 
          family=binomial("probit"))
predLq <- predict(m1q, type = "response")
predPq <- predict(m2q, type = "response")

plot(Data$Year, Data$PropProbit)
lines(Data$Year, predLq)
lines(Data$Year, predPq, col=2)


plot(fitted(m1), resid(m1))


#.  Questions 1.

# We can't do a formal test to get p-values from these results, 
#. but what statistics could you use to decide which model is better?

# which model do you prefer, and why?
# (can give multiple choice)


# If for every logit paper we have P probits, show that 
# logit(Prop) = logit(p0) + a1 t

# What is the "fitness" of the logit model?

# What propotion of papers will use the logit in 2024?

#.   1/(1+exp(-58.477740 + 2024*0.029692))


# Is there any evidence of over-dispersion?

# For the probit, what is the score?

#




xx <- rnorm(1e4)


sqrt((2*0.007^2)/4623)

