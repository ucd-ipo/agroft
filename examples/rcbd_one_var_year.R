# This is an example of a single variate RCBD that has an addition random
# effect.
#
# To run the script type:
# source('rcbd_one_var_year.R', print.eval = TRUE)
#
# The code that will be displayed to the user is bounded by this separator:
#-----------------------------------------------------------------------------#

# Load the necessary libraries.
#-----------------------------------------------------------------------------#
library('nlme')  # for lme()
#-----------------------------------------------------------------------------#

sep <- function(n){
  # This function simply prints a line of "=" to the screen as a separator and
  # will not show up in the app. It is only here for nice printing.
  line <- paste0(paste(replicate(n, "="), collapse = ""), '\n')
  cat(line)
}

# Load the data.
#-----------------------------------------------------------------------------#
my.data <- read.csv("wheat_yield_data_with_year.csv")
#-----------------------------------------------------------------------------#

# Set Block, Treatment, and Year as factors.
#-----------------------------------------------------------------------------#
my.data$Block <- as.factor(my.data$Block)
my.data$Treatment <- as.factor(my.data$Treatment)
my.data$Year <- as.factor(my.data$Year)
#-----------------------------------------------------------------------------#

# Construct a mized effects model and print the summary results.
cat('Linear mixed effect model results\n')
sep(79)
#-----------------------------------------------------------------------------#
model <- lme(fixed = Yield ~ Treatment, random = ~1|Year/Block, data = my.data)
summary(model)
#-----------------------------------------------------------------------------#
sep(79)
