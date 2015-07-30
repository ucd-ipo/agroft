# This is an example of a split-plot RCBD experimental design based off of the first
# example in [1] and further analysis in [2]. The data can be downloaded from
# [3].
#
# To run the script type:
#
# source('split_plot_rcbd.R', print.eval = TRUE)
#
# [1] http://www.unh.edu/halelab/BIOL933/labs/lab9.pdf
# [2] http://www.personal.psu.edu/mar36/stat_461/split_plot/split_plot.html
# [3] http://www.unh.edu/halelab/BIOL933/schedule.htm
#
# TODO : Maybe I should use the `data('plots')` from the agricolae package
# instead?
#
# NOTE : The code that will be shown to the user in the app is bounded by these
# separators: #------------#.

# Load the necessary libraries.
#-----------------------------------------------------------------------------#
library('agricolae')
library('effects')  # for allEffects()
#-----------------------------------------------------------------------------#

sep <- function(n){
  # This function simply prints a line of "=" to the screen as a separator and
  # will not show up in the app. It is only here for nice console printing.
  line <- paste0(paste(replicate(n, "="), collapse = ""), '\n')
  cat(line)
}

# Load the data.
# repetitions: block
# main plots: SeedLotA (four plots in a block)
# subplots: TrtmtB (four subplots in a main plot)
#-----------------------------------------------------------------------------#
my.data <- read.csv('Lab9ex1.csv')
#-----------------------------------------------------------------------------#

# The factors in this experiment are recorded with numerical values so they need
# to be recoded as factors. The user will have to set this manually in the app.
# NOTE : This is not necessary if `aov()` is used instead of `lm()`.
#-----------------------------------------------------------------------------#
my.data$SeedLotA <- as.factor(my.data$SeedLotA)
my.data$Block <- as.factor(my.data$Block)
my.data$TrtmtB <- as.factor(my.data$TrtmtB)
#-----------------------------------------------------------------------------#

# This is the standard model for a split plot RCBD.
# NOTE : I'm getting this warning message:
# Warning message:
# In aov(formula = Yield ~ SeedLotA + Block + Error(SeedLotA:Block) +  :
#   Error() model is singular
#-----------------------------------------------------------------------------#
model <- aov(formula = Yield ~ SeedLotA + Block + Error(SeedLotA:Block) +
             TrtmtB + SeedLotA:TrtmtB, data = my.data)
#-----------------------------------------------------------------------------#
summary(model)
# Following the advice in [2], I create the same model without the error term
# for assumption testing (and post hoc?). The F values and P values are not
# correct for this model so the ANOVA table should not be shown to the user.
#-----------------------------------------------------------------------------#
model.tmp <- aov(formula = Yield ~ Block + SeedLotA + TrtmtB + SeedLotA:Block +
                 SeedLotA:TrtmtB, data = my.data)
#-----------------------------------------------------------------------------#

# Plot the four standard fit plots: residuals vs predicted, sqrt of residuals vs
# fitted, Normal Q-Q plot of the residuals, residuals vs leverages.
dev.new()
#-----------------------------------------------------------------------------#
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.tmp)
#-----------------------------------------------------------------------------#

# Make sure the residuals are normal (this can also be seen in the Q-Q plot).
cat('Shapiro-Wilk Normality Test\n')
sep(50)
#-----------------------------------------------------------------------------#
shapiro.test(residuals(model.tmp))
#-----------------------------------------------------------------------------#
sep(50)

# TODO : Add Levene's Test.

# User selected alpha level.
#-----------------------------------------------------------------------------#
alpha <- 0.05
#-----------------------------------------------------------------------------#

# Print the ANOVA table of the fit. The user should will have to note the
# significant factors. In this case the single virus factor is significant.
# NOTE : `anova(model)` fails here because the `model` variable contains a list
# of anova results due to the `Error()` term.
cat('ANOVA Table\n')
sep(50)
#-----------------------------------------------------------------------------#
summary(model)
#-----------------------------------------------------------------------------#
sep(50)

# Show the confidence intervals of the model.
cat('Confidence Intervals\n')
sep(50)
#-----------------------------------------------------------------------------#
confint(model.tmp, level=1.0 - alpha)
#-----------------------------------------------------------------------------#
sep(50)

# Plot the mean yield with respect to each factor.
dev.new()
#-----------------------------------------------------------------------------#
plot(allEffects(model.tmp))
#-----------------------------------------------------------------------------#

# TODO : Do we simply check for the SeedLotA in this example? Can we use the
# LSD.test() on the model without the error term for the correct results?
cat('Least Significant Difference\n')
sep(50)
#-----------------------------------------------------------------------------#
LSD.test(model.tmp, 'SeedLotA', alpha=alpha, console=TRUE)
#-----------------------------------------------------------------------------#
sep(50)
