.onLoad <- function(libname, pkgname){
  packageStartupMessage('You have loaded Agroft: An Agriculture Field Trial Statistics Package.
  Start an Agroft session by typing "launch()" at the R prompt (>) and pressing enter.
  More info on how to use Agroft can be found online at: https://github.com/ucd-ipo/agroft
  Detailed materials can be found on the about tab within Agroft.\n
Now launching Agroft...')
  if (interactive()) {
    launch() 
  }
}