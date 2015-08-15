inst.pkgs <- installed.packages()[,'Package']

need.pkgs <- c('shiny',
               'shinyBS',
               'shinyAce',
               #'readxl',
               'agricolae',
               'car',
               'devtools',
               'knitr',
               'yaml',
               'xtable')

for (pkg in need.pkgs) {
  if (!pkg %in% inst.pkgs) {
    cat(paste0(pkg, ' is not installed.\n\nPress escape to',
            'exit the app and run initialise_AIP() to install dependencies.'))
    stop()
  }
}
