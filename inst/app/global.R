# Loads in the help and information text.
# The shinyBS popovers can't handle line returns in the strings so a special
# handler is needed.
library(yaml)
str.handler <- function(x) { gsub("[\r\n]", "", x) }
help.text <- yaml.load_file('help-text.yaml', handlers = list(str = str.handler))


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

