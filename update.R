# helper file for me. Not necessary for things

setwd('~/drat')

rmarkdown::render('index.Rmd')
system('git commit -am "update gh-pages"')
system('git push')
# bash alias for this called "udrat"