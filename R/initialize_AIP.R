initialize_AIP <- function(){

  if(!require('shiny', quietly=TRUE)){
    install.packages('shiny', repos="http://cran.rstudio.com/")
  }

  if(!require('shinyBS', quietly=TRUE)){
    install.packages('shinyBS', repos="http://cran.rstudio.com/")
  }

  if(!require('shinyAce', quietly=TRUE)){
    install.packages('shinyAce', repos="http://cran.rstudio.com/")
  }

  if(!require('knitr', quietly=TRUE)){
    install.packages('knitr', repos="http://cran.rstudio.com/")
  }

  if(!require('agricolae', quietly=TRUE)){
    install.packages('agricolae', repos="http://cran.rstudio.com/")
  }

  if(!require('car', quietly=TRUE)){
    install.packages('car', repos="http://cran.rstudio.com/")
  }

  if(!require('yaml', quietly=TRUE)){
    install.packages('yaml', repos="http://cran.rstudio.com/")
  }

  if(!require('xtable', quietly=TRUE)){
    install.packages('xtable', repos="http://cran.rstudio.com/")
  }

  if(!require('Rmisc', quietly=TRUE)){
    install.packages('Rmisc', repos="http://cran.rstudio.com/")
  }

  if(!require('ggplot2', quietly=TRUE)){
    install.packages('ggplot2', repos="http://cran.rstudio.com/")
  }

#   if(!require('readxl', quietly=TRUE)){
#     install.packages("readxl", repos="http://cran.rstudio.com/")
#   }

}



