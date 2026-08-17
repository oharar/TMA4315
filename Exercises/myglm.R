
# Select Build, Build and reload to build and load into the R-session.

myglm <- function(formula, data = list(), contrasts = NULL, ...){
  # Extract model matrix & responses
  mf <- model.frame(formula = formula, data = data)
  X  <- model.matrix(attr(mf, "terms"), data = mf, contrasts.arg = contrasts)
  y  <- model.response(mf)
  terms <- attr(mf, "terms")
  
  
  # Add code here to calculate coefficients, residuals, fitted values, etc...
  # and store the results in the list est
  est <- list(terms = terms, model = mf)
  
  # Store call and formula used
  est$call <- match.call()
  est$formula <- formula
  
  # Set class name. This is very important!
  class(est) <- 'myglm'
  
  # Return the object with all results
  return(est)
}

print.myglm <- function(object, ...){
  # Code here is used when print(object) is used on objects of class "myglm"
  # Useful functions include cat, print.default and format
  cat('Info about object\n')
}

summary.myglm <- function(object, ...){
  # Code here is used when summary(object) is used on objects of class "myglm"
  # Useful functions include cat, print.default and format
  cat('Summary of object\n')
}

plot.myglm <- function(object, ...){
  # Code here is used when plot(object) is used on objects of class "myglm"
  
  library(ggplot2)
  # ggplot requires that the data is in a data.frame, this must be done here
  ggplot() + geom_point()
  
  # if you want the plot to look nice, you can e.g. use "labs" to add labels, and add colors in the geom_point-function
  
}

anova.myglm <- function(object, ...){
  # Code here is used when anova(object) is used on objects of class "myglm"
  
  # Components to test
  comp <- attr(object$terms, "term.labels")
  
  # Name of response
  response <- deparse(object$terms[[2]])
  
  # Fit the sequence of models
  txtFormula <- paste(response, "~", sep = "")
  model <- list()
  for(numComp in 1:length(comp)){
    if(numComp == 1){
      txtFormula <- paste(txtFormula, comp[numComp])
    }
    else{
      txtFormula <- paste(txtFormula, comp[numComp], sep = "+")
    }
    formula <- formula(txtFormula)
    model[[numComp]] <- myglm(formula = formula, data = object$model)
  }
  
  # Print Analysis of Variance Table
  cat('Analysis of Variance Table\n')
  cat(c('Response: ', response, '\n'), sep = '')
  cat('          Df  Sum sq X2 value Pr(>X2)\n')
  for(numComp in 1:length(comp)){
    # Add code to print the line for each model tested
  }
  
  invisible(model)
  
}


# You can put these functions somewhere else, but it is practical to keep them with the other functions you create
# optim requires thtat the first argument of the function optim shall optimize is the parameters over which minimization is to take place (par)
# it is common to include all other necessary arguments in a list called "args", but this can be done other ways as well
loglik_poi <- function(par, args = list()){
  
  # the Poisson log-likelihood
  
}

