inst.pkgs <- installed.packages()[,'Package']

need.pkgs <- c('shiny',
               'shinyBS',
               'shinyAce',
               #'readxl',
               'agricolae',
               'car',
               'devtools',
               'knitr',
               'yaml')

if(!all(need.pkgs %in% inst.pkgs)){
  cat(paste('Not all the required packages are installed.\n\nPress escape to',
            'exit the app and  run initialise_AIP() to install dependencies',
            sep=''))
  stop()
}
