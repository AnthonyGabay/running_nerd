#'Produces reporting stats for rlmer objects.
#'
#'@description Produces a table of betas, 95\% confidence intervals and p-values
#'for rlmer objects
#'
#'To calculate Wald confidence intervals it uses code adapted from Ben Bolker at
#'\url{https://gist.github.com/kamermanpr/aaa598485b6e990017375359ff5f4533)}
#'
#'
#'It then calculates z-scores and p-values based on these confidence intervals
#'
#'@param  object An rlmer object.
#'@param level Confidence levels; default setting is 0.95
#'@return Table of betas, upper and lower CIs, SE, Z, p-values
#'@examples
#'# example data
#'df <- data.frame(ID = rep(1:5, 5), dv = rnorm(25, 0, 1), x = rnorm(25, 3, 0.5))
#'  # run rlmer model
#'mod <- robustlmm::rlmer(dv ~ x + (1|ID), data = df)
#'  # create output
#'output_rlmerMod(mod)



output_rlmerMod <- function(object, level = 0.95) {

  # Extract beta coefficients
  beta <- lme4::fixef(object)

  # Extract names of coefficients
  parm <- names(beta)

  # Extract standard errors for the coefficients
  se <- sqrt(diag(vcov(object)))

  # Set level of confidence interval
  conf.level <- qnorm((1 + level) / 2)

  # Calculate z value
  z = beta/se

  # Calculate CI and create table
  ctab <- cbind(beta,
                beta - (conf.level * se),
                beta + (conf.level * se),
                se,
                z,
                2*pnorm(-abs(z)))


  # label column names
  colnames(ctab) <- c('beta',
                      paste(100 * ((1 - level) / 2), '%'),
                      paste(100 * ((1 + level) / 2), '%'),
                      'SE',
                      'z',
                      'p')


  # Output
  return(round(ctab[parm, ],3))


}

