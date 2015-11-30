# Loads in the help and information text.
# The shinyBS popovers can't handle line returns in the strings so a special
# handler is needed.
library(yaml)
str.handler <- function(x) { gsub("[\r\n]", "", x) }
help.text <- yaml.load_file('help-text.yaml', handlers = list(str = str.handler))


