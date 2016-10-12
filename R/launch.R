launch <- function(){
	if (!interactive())
		stop('The Agroft package is designed for interactive use only', call.=TRUE)

  shiny::runApp(system.file('app', package='agroft'))
}