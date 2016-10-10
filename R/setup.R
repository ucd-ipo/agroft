setup <- function(){
  cat('\nBegining Agroft Analysis app setup...\n')
  Sys.sleep(.5)
  cat('\nYou can have the Agroft app start automatically when you open R.')
  Sys.sleep(.25)
  cat('\n\nChoose "yes" if you want Agroft to open automatically when you open the R application (recommended)')
  cat("\nThis option is designed for people who will only be using the Agroft software, and do not plan to learn R programming.")
  cat('\n\nChoose "no" if you want Agroft to load only when you tell it to.')
  cat('\nThis option is designed for people who use or plan to use R and its programming capabilities for purposes other than running the Agroft software.\n\n')
  Sys.sleep(.5)
  cat('Do you want Agroft to open automatically when you start R?\n')
  
  autostart <- readline(prompt = 'Type "yes" or "no": ')
  
  code <- '
local({
old <- getOption("defaultPackages")
options(defaultPackages = c(old, "agroft"))
})
'
  
  rpf <- Sys.getenv('R_PROFILE_USER')
  
  if (grepl('yes', autostart, ignore.case=TRUE)) {
    if (rpf == '') {
      if (!file.exists('~/.Rprofile')) {
        cat(code, file = '~/.Rprofile')
      } else {
        rp <- readLines('~/.Rprofile')
        cat(rp, code, sep='\n',file = '~/.Rprofile')
      }
    } else {
      rp <- readLines(rpf)
      cat(rp, code, sep='\n', file = rpf)
    }
  }
  
  if (grepl('no', autostart, ignore.case=TRUE)) {
    if (rpf == '') {
      if (file.exists('~/.Rprofile')) {
        rp <- readLines('~/.Rprofile')
        rp2 <- setdiff(rp, strsplit(code, '\n')[[1]])
        cat(rp2, file='~/.Rprofile')
      }
    } else {
      if (file.exists(rpf)) {
        rp <- readLines(rpf)
        rp2 <- setdiff(rp, strsplit(code, '\n')[[1]])
        cat(rp2, rpf, sep='\n', file = rpf)
      }
    }
  }
  cat('\n\nAgroft setup is complete and ready to use. You can launch Agroft at anytime by typing launch() in the console.')
}











