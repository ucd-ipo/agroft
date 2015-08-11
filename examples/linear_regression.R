# This script runs a simple single variate linear regression.
#
# To run the script with full output:
#
# source('linear_regression.R', print.eval = TRUE)
#
# NOTE : All lines shown to the user in the app are surrounded by this
# separator: #-----------------------------#

sep <- function(n){
  # This function simply prints a line of "=" to the screen as a separator and
  # will not show up in the app. It is only here for nice printing.
  line <- paste0(paste(replicate(n, "="), collapse = ""), '\n')
  cat(line)
}

# Generate some data.
year <- c(2000 ,   2001  ,  2002  ,  2003 ,   2004)
rate <- c(9.34 ,   8.50  ,  7.62  ,  6.93  ,  6.60)
my.data <- data.frame(rate, year)
write.csv(my.data, 'linear_regression.csv', quote = FALSE, row.names = FALSE)

# Load the data.
#-----------------------------------------------------------------------------#
my.data <- read.csv('linear_regression.csv')
#-----------------------------------------------------------------------------#

# Fit the model.
#-----------------------------------------------------------------------------#
model <- lm(formula = rate ~ year, data = my.data)
#-----------------------------------------------------------------------------#

# Plot a scatter plot with a best fit line.
dev.new()
#-----------------------------------------------------------------------------#
plot(formula = rate ~ year, data = my.data)
abline(model)
#-----------------------------------------------------------------------------#
dev.copy(png, 'lin-reg-scatter-plot.png')
dev.off()

# Plot two standard fit plots: residuals vs predicted, Normal Q-Q plot of the
# residuals.
dev.new()
#------------------------------------------------------------------------------#
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))
plot(model, c(1, 2))
#------------------------------------------------------------------------------#
dev.copy(png, 'lin-reg-fit-plots.png')
dev.off()

# Make sure the residuals are normal (this can also be seen in the Q-Q plot).
cat('Shapiro-Wilk Normality Test\n')
sep(79)
#------------------------------------------------------------------------------#
shapiro.test(residuals(model))
#------------------------------------------------------------------------------#
sep(79)

# Print the summary of the model.
cat('Linear Regression Summary\n')
sep(79)
#------------------------------------------------------------------------------#
summary(model)
#------------------------------------------------------------------------------#
sep(79)
