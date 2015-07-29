# This is an example of a split RCBD experiemental design based off of the first
# example in [1] and further analysis in [2]. The data can be downloaded from
# [3].
#
# [1] http://www.unh.edu/halelab/BIOL933/labs/lab9.pdf
# [2] http://www.personal.psu.edu/mar36/stat_461/split_plot/split_plot.html
# [3] http://www.unh.edu/halelab/BIOL933/schedule.htm
#
# TODO : Maybe I should use the `data('plots')` from the agricolae package instead?

library('agricolae')
library('effects')

sep <- function(n){
  # This function simply prints a line of "=" to the screen as a separator and
  # will not show up in the app. It is only here for nice printing.
  line <- paste0(paste(replicate(n, "="), collapse = ""), '\n')
  cat(line)
}

# Load the data.
# repititions: block
# main plots: SeedLotA (four plots in a block)
# subplots: TrtmtB (four subplots in a main plot)
oats.data <- read.csv('Lab9ex1.csv')

# The factors in this experiment are recorded with numerical values so they need
# to be recoded as factors. The user will have to set this manually in the app.
oats.data$SeedLotA <- as.factor(oats.data$SeedLotA)
oats.data$Block <- as.factor(oats.data$Block)
oats.data$TrtmtB <- as.factor(oats.data$TrtmtB)

# This is the standard model for a split plot RCBD.
# NOTE : I'm getting this warning message:
# Warning message:
# In aov(formula = Yield ~ SeedLotA + Block + Error(SeedLotA:Block) +  :
#   Error() model is singular
model <- aov(formula = Yield ~ SeedLotA + Block + Error(SeedLotA:Block) + TrtmtB
             + SeedLotA:TrtmtB, data = oats.data)

# At this point we find that SeedLotA is significant along with the interaction.
# NOTE : If the interaction was not significant you can analyze the main effects
# of the main plot and sub plot.

# Following the advice in [2] I create the same model without the error term for
# assumption testing (and post hoc?). The F values and P values are not correct
# for this model so the ANOVA table should not be shown to the user.
model.tmp <- aov(formula = Yield ~ Block + SeedLotA + TrtmtB + SeedLotA:Block +
                 SeedLotA:TrtmtB, data = oats.data)

# Plot the four standard fit plots: residuals vs predicted, sqrt of residuals vs
# fitted, Normal Q-Q plot of the residuals, residuals vs leverages.
dev.new()
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.tmp)

# Make sure the residuals are normal (this can also be seen in the Q-Q plot).
cat('Shapiro-Wilk Normality Test\n')
sep(50)
shapiro.test(residuals(model.tmp))
sep(50)

# User selected alpha level.
alpha <- 0.05

# Print the ANOVA table of the fit. The user should will have to note the
# significant factors. In this case the single virus factor is significant.
# NOTE : `anova(model)` fails here because the `model` variable contains a list
# of anova results due to the `Error()` term.
cat('ANOVA Table\n')
sep(50)
summary(model)
sep(50)

# Show the confidence intervals for the intercept and each virus level.
cat('Confidence Intervals\n')
sep(50)
confint(model.tmp, level=1.0 - alpha)
sep(50)

# Plot the mean yield with respect to each virus level.
dev.new()
plot(allEffects(model.tmp))

# TODO : Do we simply check for the SeedLotA in this example? Can we use the
# LSD.test() on the model without the error term for the correct results?
cat('Least Significant Difference\n')
sep(50)
LSD.test(model.tmp, 'SeedLotA', alpha=alpha, console=TRUE)
sep(50)
