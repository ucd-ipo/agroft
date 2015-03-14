inst.pkgs <- installed.packages()[,'Package']

need.pkgs <- c('shiny',
              'shinyAce',
              'shinyBS',
              'knitr',
              'gridExtra',
              'agricolae', 
              'lattice',
              'effects',
              'lsmeans',
              'devtools')

if(!all(need.pkgs %in% inst.pkgs)){
  stop()
  cat('Not all the required packages are installed.\n\nPress escape to exit the app and  run initialise_AIP() to install dependencies')
}
