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
  stop('Not all the required packages are installed. \nPlease run initialise_AIP() to install them')
}
