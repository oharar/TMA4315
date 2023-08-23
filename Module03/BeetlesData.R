Beetles <- data.frame(
  Dose = rep(c(49.06, 52.99, 56.91, 60.84, 64.76, 68.69, 72.61, 76.54),2),
 Replicate = factor(rep(1:2, each=8)),
 NSurv = c(27,23,19,13, 7, 2, 1, 0,  26,24,25,15, 4, 4, 0, 0),
 NDied = c( 2, 7, 9,14,23,29,29,29,   4, 6, 9,14,29,24,32,31)
 )

b1 <- glm(cbind(NSurv, NDied) ~ log(Dose)*Replicate, data=Beetles, family="binomial")
