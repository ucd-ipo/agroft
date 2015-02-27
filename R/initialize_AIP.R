initialize_AIP <- function(){

if(!require(devtools, quietly=TRUE)){
  install.packages('devtools', repos="http://cran.rstudio.com/")
  library(devtools)
}

if(!require(shiny, quietly=TRUE)){
  install.packages('shiny', repos="http://cran.rstudio.com/")
}

if(!require(knitr, quietly=TRUE)){
  install.packages('knitr', repos="http://cran.rstudio.com/")
}

if(!require(gridExtra, quietly=TRUE)){
  install.packages('gridExtra', repos="http://cran.rstudio.com/")
}

if(!require(agricolae, quietly=TRUE)){
  install.packages('agricolae', repos="http://cran.rstudio.com/")
}

if(!require(lattice, quietly=TRUE)){
  install.packages('lattice', repos="http://cran.rstudio.com/")
}

if(!require(effects, quietly=TRUE)){
  install.packages('effects', repos="http://cran.rstudio.com/")
}

if(!require(lsmeans, quietly=TRUE)){
  install.packages('lsmeans', repos="http://cran.rstudio.com/")
}

if(!require(xtable, quietly=TRUE)){
  install.packages('xtable', repos="http://cran.rstudio.com/")
}

if(!require(shinyAce, quietly=TRUE)){
  install_github('trestletech/shinyAce')
}

if(!require(shinyBS, quietly=TRUE)){
  install_github("ebailey78/shinyBS", ref = "shinyBS3")
}
}



