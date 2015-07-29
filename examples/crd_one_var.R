# This script runs a single variate ANOVA based on one dependent continuous
# variable and one independent factor. It is based on the example given on page
# 27 of the agricolae tutorial [1]. Everything displayed to the terminal and the
# two plots will be displayed in the app. To run the script with full output:
#
# source('crd_one_var.R', print.eval = TRUE)
#
# [1] https://cran.r-project.org/web/packages/agricolae/vignettes/tutorial.pdf
#
# NOTE : All lines shown to the user in the app are surrounded by this
# seperator:
#        #-----------------------------#

#------------------#
library('agricolae')
library('car')
library('effects')
#------------------#

sep <- function(n){
  # This function simply prints a line of "=" to the screen as a seperator and
  # will not show up in the app. It is only here for nice printing.
  line <- paste0(paste(replicate(n, "="), collapse = ""), '\n')
  cat(line)
}

# The sweetpotato data set is a completely random experiment design with plots
# of 79 sweet potatoes where there are three viruses and a control applied in 3
# reps, i.e. 3 plots for each of four virus levels.
#---------------#
data(sweetpotato)
my.data <- sweetpotato  # rename the data frame
#---------------#

# Construct the model.
#---------------------------------------#
model <- aov(yield ~ virus, data=my.data)
#---------------------------------------#

# Plot the four standard fit plots: residuals vs predicted, sqrt of residuals vs
# fitted, Normal Q-Q plot of the residuals, residuals vs leverages.
dev.new()
#---------------------------------------#
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))  # plots as subplots of single graph
plot(model)
#---------------------------------------#

# Make sure the residuals are normal (this can also be seen in the Q-Q plot).
cat('Shapiro-Wilk Normality Test\n')
sep(79)
#----------------------------#
shapiro.test(residuals(model))
#----------------------------#
sep(79)

# Run Levene's Test.
cat("Levene's Test\n")
sep(79)
#---------------------------------------#
leveneTest(yield ~ virus, data = my.data)
#---------------------------------------#
sep(79)

# User selected alpha level.
#-----------#
alpha <- 0.05
#-----------#

# Print the ANOVA table of the fit. The user should will have to note the
# significant factors. In this case the single virus factor is significant.
cat('ANOVA Table\n')
sep(79)
#-----------#
anova(model)
#-----------#
sep(79)

# Show the confidence intervals for the intercept and each virus level.
cat('Confidence Intervals\n')
sep(79)
#-------------------------------#
confint(model, level=1.0 - alpha)
#-------------------------------#
sep(79)

# Plot the mean yield with respect to each virus level.
dev.new()
#---------------------#
plot(allEffects(model))
#---------------------#

# The ANOVA table shows that the virus factor is significant, so we then see
# which levels are significant with respect to each other using least
# significant difference.
cat('Least Significant Difference\n')
sep(79)
#-------------------------------------------------#
LSD.test(model, "virus", alpha=alpha, console=TRUE)
#-------------------------------------------------#
sep(79)
