initialize_AIP <- function(){

  if(!require(devtools, quietly=TRUE)){
    install.packages('devtools', repos="http://cran.rstudio.com/")
  }

  if(!require(shiny, quietly=TRUE)){
    install.packages('shiny', repos="http://cran.rstudio.com/")
  }

  if(!require(shinyBS, quietly=TRUE)){
    install.packages('shinyBS', repos="http://cran.rstudio.com/")
  }

  if(!require(shinyAce, quietly=TRUE)){
    devtools::install_github('trestletech/shinyAce')
  }

  if(!require(knitr, quietly=TRUE)){
    install.packages('knitr', repos="http://cran.rstudio.com/")
  }

  if(!require(agricolae, quietly=TRUE)){
    install.packages('agricolae', repos="http://cran.rstudio.com/")
  }

  if(!require(car, quietly=TRUE)){
    install.packages('car', repos="http://cran.rstudio.com/")
  }

#   if(!require(readxl, quietly=TRUE)){
#     install.packages("readxl", repos="http://cran.rstudio.com/")
#   }

}



