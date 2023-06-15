
s=function(par,args)
  y <- args$y
  x <- args$x
  n <- args$n
  
  res <- y %*% x - t(t(n * x) %*% ((1 + exp(-x %*% par))^(-1)))
  return(res)
  }
F=function(par,args)
  y <- args$y
x <- args$x

res <- y %*% x - t(t(n * x) %*% ((1 + exp(-x %*% par))^(-1)))
return(res)
}
ds=list(y=crab$Sa,x=model.matrix(model2a))
testobsScore3a=s(model2a$coefficients,ds)%*%solve(F(model2a$coefficients,ds))%*%s(model2a$coefficients,ds)
1-pchisq(testobsScore3a,)

