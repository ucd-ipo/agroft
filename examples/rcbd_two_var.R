# This script runs a multivariate ANOVA based on one dependent continous
# variable, two factors, and a factor that represents the block in a Random
# Complete Block Design. It is based on the example given on page 36 of the
# agricolae tutorial[1].
#
# [1] https://cran.r-project.org/web/packages/agricolae/vignettes/tutorial.pdf

library('agricolae')
library('effects')

sep <- function(n){
  # This function simply prints a line of "=" to the screen as a separator and
  # will not show up in the app. It is only here for nice printing.
  line <- paste0(paste(replicate(n, "="), collapse = ""), '\n')
  cat(line)
}

# Create some data. This represents an RCBD design with a factorial clone x
# nitrogen with yield as the response variable.
yield <- c(6, 7, 9, 13, 16, 20, 8, 8, 9,
           7, 8, 8, 12, 17, 18, 10, 9, 12,
           9, 9, 9, 14, 18, 21, 11, 12, 11,
           8, 10, 10, 15, 16, 22, 9, 9, 9)
block <- gl(4, 9)
clone <- rep(gl(3, 3, labels = c('c1', 'c2', 'c3')), 4)
nitrogen <- rep(gl(3, 1, labels = c('n1', 'n2', 'n3')), 12)
data <- data.frame(block, clone, nitrogen, yield)

model <- aov(yield ~ block + clone + nitrogen + clone:nitrogen, data=data)

# Plot the four standard fit plots: residuals vs predicted, sqrt of residuals vs
# fitted, Normal Q-Q plot of the residuals, residuals vs leverages.
dev.new()
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model)

# Make sure the residuals are normal (this can also be seen in the Q-Q plot).
cat('Shapiro-Wilk Normality Test\n')
sep(50)
shapiro.test(residuals(model))
sep(50)

# User selected alpha level.
alpha <- 0.05

# Print the ANOVA table of the fit. The user should will have to note the
# significant factors. In this case the single virus factor is significant.
cat('ANOVA Table\n')
sep(50)
anova(model)
sep(50)

# Show the confidence intervals for the intercept and each virus level.
cat('Confidence Intervals\n')
sep(50)
confint(model, level=1.0 - alpha)
sep(50)

# Plot the mean yield with respect to each virus level.
# TODO : Is the default plot ok here, or is something different desired?
dev.new()
plot(allEffects(model))

# The ANOVA table shows that each variable and the interaction are significant,
# so we then see which levels of both clone and nitrogen are significant with
# respect to each other using least significant difference.
# TODO : It isn't clear to me why the "block" variable wouldn't be checked here.
cat('Least Significant Difference\n')
sep(50)
LSD.test(model, c('clone', 'nitrogen'), alpha=alpha, console=TRUE)
sep(50)
