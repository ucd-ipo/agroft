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
          #    'readxl',
              'devtools')

if(!all(need.pkgs %in% inst.pkgs)){
  cat('Not all the required packages are installed.\n\nPress escape to exit the app and  run initialise_AIP() to install dependencies')
  stop()
}
