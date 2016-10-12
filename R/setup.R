setup <- function(autostart=NULL){

	if(!interactive() & is.null(autostart)) {
		stop('setup function is for interactive use', call.=TRUE)
	}

	if (interactive() && is.null(autostart)) {

  cat('\nBegining Agroft Analysis app setup...\n')
  Sys.sleep(.5)
  cat('\nYou can have an Agroft session start automatically when you open R.')
  cat('\n\nChoose "yes" if you want Agroft to open automatically when you open the R application\n (recommended for beginning users)')
  cat("\nThis option is designed for people who will only be using the Agroft software, and do not plan to learn R programming.")
  cat('\n\nChoose "no" if you want Agroft to load only when you tell it to.')
  cat('\nThis option is designed for people who use or plan to use R and its programming capabilities for purposes other than running the Agroft software.\n\n')
  cat('Do you want Agroft to open automatically when you start R?\n')

  autostart <- readline(prompt = 'Type "yes", "no", or "cancel": ')
	}

  code <- 'local({old <- getOption("defaultPackages"); options(defaultPackages = c(old, "agroft"))})'

  rpf <- Sys.getenv('R_PROFILE_USER')

  if (grepl('yes', autostart, ignore.case=TRUE)) {
    if (rpf == '') {
      if (!file.exists('~/.Rprofile')) {
        cat(code, file = '~/.Rprofile')
      } else {
        rp <- readLines('~/.Rprofile', warn=FALSE)
        cat(rp, code, sep='\n',file = '~/.Rprofile')
      }
    } else {
      rp <- readLines(rpf, warn=FALSE)
      cat(rp, code, sep='\n', file = rpf)
    }
  	y <- TRUE
  }

  if (grepl('no', autostart, ignore.case=TRUE)) {
    if (rpf == '') {
      if (file.exists('~/.Rprofile')) {
        rp <- readLines('~/.Rprofile', warn=FALSE)
        rp2 <- setdiff(rp, code)
        cat(rp2, file='~/.Rprofile')
      }
    } else {
      if (file.exists(rpf)) {
        rp <- readLines(rpf, warn=FALSE)
        rp2 <- setdiff(rp, code)
        cat(rp2, rpf, sep='\n', file = rpf)
      }
    }
  	red <- length(rp) > length(rp2)
  	y <- FALSE
  }

  n1 <- 'An Agroft session will no longer automatically start when R starts'
  n2 <- 'An Agroft session will not start automatically when R starts'
  y1 <- 'An Agroft session will now run start automatically when you start R'
  c1 <- 'Action cancelled'

  msg <- if (exists('y', inherits=FALSE)) {if (y) y1 else if (red) n1 else n2} else c1
  message(msg)
}











