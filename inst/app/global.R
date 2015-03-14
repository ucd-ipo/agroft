
source('plot_effects.R')

strip.args <- function(x){
  # x is a call that needs to be striped of arguments
  x <- as.list(x)
  aov.calls <- c('formula', 'data')
  glm.calls <- c('formula', 'family', 'data')
  t.test.calls <- c('formula', 'data')
  
  the.call <- deparse(x[[1]])
  
  x <- switch(the.call, 
              aov=x[c(1, which(names(x) %in% aov.calls))],
              glm=x[c(1, which(names(x) %in% glm.calls))],
              t.test=x[c(1, which(names(x) %in% t.test.calls))])
  
  x <- as.call(x)
  return(x)
}

is.numeric2 <- function(x){
  is.numeric(x) & length(unique(x)) > 5
}
  

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
                    ifelse(as.numeric(x) >= .05, paste0('p=', format(round(as.numeric(x), 2), nsmall=2)), 
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
  names(tbl)[c(1,3:5)] <- c('t-value', gsub('mean in group ','', grps), 'p-value')
  
  xtable(tbl, caption=caption)
}

paste_na <- function(...,sep="; ", collapse=NULL) {
  L <- list(...)
  L <- L[!is.na(L)]
  ret <- do.call(paste,c(L,list(sep=sep, collapse=collapse)))
  ret <- gsub(paste0('^(',sep,'){1,}|(', sep, '){1,}$'), '', ret, fixed=TRUE)
  ret <- gsub(paste0('(',sep,'){2,}'), sep, ret, fixed=TRUE)
  is.na(ret) <- ret==""
  ret
}
