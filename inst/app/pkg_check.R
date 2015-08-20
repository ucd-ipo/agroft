inst.pkgs <- installed.packages()[,'Package']

need.pkgs <- c('shiny',
               'shinyBS',
               'shinyAce',
               #'readxl',
               'agricolae',
               'car',
               'knitr',
               'yaml',
               'xtable',
               'Rmisc',
               'ggplot2')

for (pkg in need.pkgs) {
  if (!pkg %in% inst.pkgs) {
    cat(paste0(pkg, ' is not installed.\n\nPress escape to exit the app and ',
               'run `initialize_AIP()` to install all of the dependencies.'))
    stop()
  }
}
