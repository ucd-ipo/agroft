# function for plotting the effects seen in the analysis
source('plot_effects.R')

# function to take a t.test/aov/glm function call and return only the aruments
# that make sense in the model, then convert it back into a call to be run.
# the call to the function can then be printed in the app (via. deparse()) to
# show the correct code
# we need this function because I am taking input$analysis nd directly sending
# that to the call() function. This way I don't have to recode a bunch of stuff,
# I can have this function do it for me. It also prevents a bunch of different
# custom analyses for each analysis type. I don't have to have something like
# switch(input$analysis, ...) and do a separate analysis for each input.
# Instead, it is just, call(input$analysis, args_to_function). Any unnecissary
# arguments are pulled out of the function call with this function.
strip.args <- function(x){
  # x is a call that needs to be striped of arguments
  x <- as.list(x)
  aov.calls    <- c('formula', 'data')
  lm.calls     <- c('formula', 'data')
  glm.calls    <- c('formula', 'family', 'data')
  t.test.calls <- c('formula', 'data')

  # if a distribution family is sellected AND if that family is gaussian, make
  # the call "lm" rather than "glm"
  if(length(x$family) > 0 && x$family=='gaussian'){
    x[[1]] <- as.name('lm')
  }

  # convert the function being called into a string (i.e. "glm", "lm", "aov", or
  # "t.test")
  the.call <- deparse(x[[1]])

  # if the call is "rcbd" as pulled from the input$select_analysis, make it an
  # ANOVA because RCBD is an anova, people just seemto want a separate table for
  # that analysis.
  if(the.call=='rcbd'){
    x[[1]] <- as.name('aov')
  }

  # get rid of whatever calls are unnecissary ("family" in all but glm) for the
  # analysis so evaluating the calls isn't weird.
  # all analysis functions for now have "..." so this won't affect the analysis,
  # but we don't want "family" showing up in t.test because that is ridiculous.
  # this might need to be edited when other analyses are added (mixed models?)
  x <- switch(the.call,
              rcbd   = x[c(1, which(names(x) %in% aov.calls))],
              lm     = x[c(1, which(names(x) %in% lm.calls))],
              aov    = x[c(1, which(names(x) %in% aov.calls))],
              glm    = x[c(1, which(names(x) %in% glm.calls))],
              t.test = x[c(1, which(names(x) %in% t.test.calls))])

  x <- as.call(x)
  return(x)
}

# test whether variable is numeric and has > 9 unique values
# used to determine if a histogram should be run
is.numeric2 <- function(x){
  is.numeric(x) & length(unique(x)) > 9
}

# function for formatting p-values - used in xtable.htest so that p-values
# displayed in the table are beautiful.
p.val <- function(x, sig.indicator=FALSE, pass.chars=FALSE){
  if(pass.chars==FALSE){
    x <- as.numeric(x)
    p.val <- ifelse(x >= .05, paste0('p=', format(round(x, 2), nsmall=2)),
                    ifelse(x < .05 & x >= .01, 'p<.05',
                           ifelse(x < .01 & x >= .001, 'p<.01',
                                  ifelse(x< .001, 'p<.001', NA))))
  }
  else{
    p.val <- ifelse(is.na(as.numeric(x)), x,
                    ifelse(as.numeric(x) >= .05,
                           paste0('p=', format(round(as.numeric(x), 2),
                                               nsmall=2)),
                           ifelse(as.numeric(x) < .05 & as.numeric(x) >= .01, 'p<.05',
                                  ifelse(as.numeric(x) < .01 & as.numeric(x) >= .001, 'p<.01',
                                         ifelse(as.numeric(x)< .001, 'p<.001', NA)))))
  }
  if(sig.indicator==TRUE){
    p.val <- ifelse(p.val=='p<.05', paste0(p.val, '*'),
                    ifelse(p.val=='p<.01', paste0(p.val, '**'),
                           ifelse(p.val=='p<.001', paste0(p.val, '***'),
                                  p.val))) }
  return(gsub('0\\.', '.', as.character(p.val)))
}

# generic function for xtable with t-tests in the report since xtable doesn't
# have a generic function for class "htest" (i.e. a t-test object)
xtable.htest <- function(x){
  tstat <- round(x$statistic, 2)
  df    <- round(x$parameter, 2)
  pval  <- p.val(x$p.value)
  estimate <- round(x$estimate,2)
  grps <- paste0(attributes(estimate)$names, ' estimate')
  caption=paste(x$method, 'for', x$data.name)

  tbl <- data.frame('t-value'=c(tstat),
                    df=c(df),
                    estimate1 = estimate[1],
                    estimate2 = estimate[2],
                    'p-value'=pval)
  names(tbl)[c(1,3:5)] <- c('t-value', gsub('mean in group ','', grps),
                            'p-value')

  xtable(tbl, caption=caption)
}

# function for pasting the formula together
# this is different from the typical paste function because we need to strip out
# NA values from the call to paste. Needed so when interactions are NA, we dont
# have a formula that looks like  dv ~ iv1 + iv2 + iv1 * NA
paste_na <- function(...,sep="; ", collapse=NULL) {
  L <- list(...)  # make a list of things to paste
  L <- L[!is.na(L)] # remove the NA values
  # paste them all together
  ret <- do.call(paste,c(L,list(sep=sep, collapse=collapse)))
  # remove leading or trailing separators
  ret <- gsub(paste0('^(',sep,'){1,}|(', sep, '){1,}$'), '', ret, fixed=TRUE)
  # remove duplicated separators
  ret <- gsub(paste0('(',sep,'){2,}'), sep, ret, fixed=TRUE)
  is.na(ret) <- ret==""
  ret
}

### test whether data are poisson ######################################
# if is.pois returns TRUE, give a warning to the user that their DV is poisson
# distributed and they should be using glm with a poisson distribution.
# if data are integer and
# skewness > threshold(default 1) and
# mean of dist ~= var of dist then
# distribution is probably poisson and user should be warned
is.pois <- function(x, skewThreshold=.65, poisThreshold=1){
  x <- x[!is.na(x)]
  is.num <- is.numeric(x)
  if(is.num){
    is.int <- all(x %% 1 == 0)
    is.rskew <- skew(x) > skewThreshold
    mean.is.var <- abs(mean(x) - var(x)) < poisThreshold
    res <- all(is.int, is.rskew, mean.is.var)
  } else {
    res <- FALSE
  }
  return(res)
}

# function to test skew of the data (positive value == right skewed)
# value greater than 1 is skewed enough that it might be poisson
skew <- function(x){
  x  <- x[!is.na(x)]
  n  <- length(x)
  sk <- sum((x - mean(x))^3/sqrt(as.numeric(var(x)))^3)/length(x)
  return(sk)
}
########################################################################
