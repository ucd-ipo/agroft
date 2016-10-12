.onLoad <- function(libname, pkgname){
  packageStartupMessage('You have loaded Agroft: An Agriculture Field Trial Statistics Package.\n
You can start an Agroft session by typing "launch()" at the R prompt (>) and pressing enter.
More info on how to use Agroft can be found online at: http://ucd-ipo.github.io/agroft \n
To end your Agroft session, close the browser tab or window Agroft is running in.\n
Important!\n- Agroft does not save your work between sessions.
- Use the report feature in the app to save your progress.\n')

	Sys.sleep(1.5)

	cat('Now launching Agroft...')

  if (interactive()) {
    launch()
  }
}