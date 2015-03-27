# aip-analysis - An analysis interface for the Agriculture Innovation Project

The shiny app for AIP has been built into a package so it can be installed from github via `devtools::install_github('ikkyle/aip-analysis')`. 

The required packages are listed in "Enhances" rather than "Depends" or "Imports" because `install_github` won't install dependencies from github. In the package, there is a function for installing all the packages called `initialize_AIP`. This will check if required packages are on the search path, and if not, it will install them. All the required packages are on CRAN, but shinyACE and shinyBS need version 0.2-1 and 0.5, respectively, so until those are on CRAN, this annoying workaround is required. Once they are on CRAN, this workaround can be removed and the packages can be moved to "Imports" rather than "Enhances" in the "DESRIPTION" file. 

The function `AIP` will run the app (i.e., just run `AIP()` in the console). If all packages aren't installed, a warning will be generated telling you to stop the app and run `initialize_AIP`. 




