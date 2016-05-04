# Loads in the help and information text.
# The shinyBS popovers can't handle line returns in the strings so a special
# handler is needed.
library(yaml)
str.handler <- function(x) { gsub("[\r\n]", "", x) }
help.text <- yaml.load_file('help-text.yaml', handlers = list(str = str.handler))

MakePostHocPlot <- function(fit, dep.var, ind.var) {
  if (length(ind.var) == 2) {
    x.label = paste(ind.var, collapse = ":")
  } else {
    x.label = ind.var
  }

  f <- as.formula(paste0('~', paste0(ind.var, collapse='+')))
  lsd.results <- cld(lsmeans(fit, f), Letters=letters)
  lsd.results <- lsd.results[order(lsd.results[,1], lsd.results[,2]),]
  dat <- lsd.results$lsmean
  if (length(ind.var) == 2) {
    names(dat) <- paste0(as.character(lsd.results[,ind.var[1]]), ":", 
                         as.character(lsd.results[,ind.var[2]]))
  } else {
    names(dat) <- as.character(lsd.results[,ind.var]) 
  }
  b <- barplot(dat, xlab = x.label, ylab = dep.var,
               ylim = c(0, max(lsd.results$upper.CL)+max(lsd.results$upper.CL)*.10),
               cex.names = .8)
  abline(h=0)
  arrows(x0 = b, 
         y0 = lsd.results$lower.CL, 
         y1 = lsd.results$upper.CL,
         code = 3, angle = 90, length=.1)
  
  text(x=b, y=lsd.results$upper.CL*1.08,
       labels = gsub('^\\s*|\\s*$', '', lsd.results$.group))
}

