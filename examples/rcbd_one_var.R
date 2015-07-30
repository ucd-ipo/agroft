# This is an example of a single variate RCBD.
#
# To run the script type:
# source('rcbd_one_var.R', print.eval = TRUE)
#
# The code that will be displayed to the user is bounded by this separator:
#-----------------------------------------------------------------------------#

# Load the necessary libraries.
#-----------------------------------------------------------------------------#
library(car)  # for leveneTest()
library(agricolae)  # for LSD.test()
library(effects)  # for allEffects()
#-----------------------------------------------------------------------------#

sep <- function(n){
  # This function simply prints a line of "=" to the screen as a separator and
  # will not show up in the app. It is only here for nice printing.
  line <- paste0(paste(replicate(n, "="), collapse = ""), '\n')
  cat(line)
}

# Load the data.
#-----------------------------------------------------------------------------#
my.data <- read.csv("Wheat_yield_data.csv")
#-----------------------------------------------------------------------------#

# Set Block and Treatment as factors.
#-----------------------------------------------------------------------------#
my.data$Block <- as.factor(my.data$Block)
my.data$Treatment <- as.factor(my.data$Treatment)
#-----------------------------------------------------------------------------#

# Construct the model.
# NOTE : aov() can be used here and the as.factor() calls would be unnecessary.
#-----------------------------------------------------------------------------#
model <- lm(Yield ~ Treatment + Block, my.data)
#-----------------------------------------------------------------------------#

# Print the ANOVA table for the model.
cat('ANOVA Table\n')
sep(79)
#-----------------------------------------------------------------------------#
anova(model)
#-----------------------------------------------------------------------------#
sep(79)

# User sets the alpha level.
#-----------------------------------------------------------------------------#
alpha <- 0.05
#-----------------------------------------------------------------------------#

# Show the confidence intervals for the intercept and each virus level.
cat('Confidence Intervals\n')
sep(79)
#-----------------------------------------------------------------------------#
confint(model, level=1.0 - alpha)
#-----------------------------------------------------------------------------#
sep(79)
# Plot the four standard fit plots: residuals vs predicted, sqrt of residuals vs
# fitted, Normal Q-Q plot of the residuals, residuals vs leverages.
dev.new()
#-----------------------------------------------------------------------------#
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))  # plots as subplots of single graph
plot(model)
#-----------------------------------------------------------------------------#

# Plot a kernel density plot of the residuals.
dev.new()
#-----------------------------------------------------------------------------#
plot(density(residuals(model)))
#-----------------------------------------------------------------------------#

# Create Box Plot of treatments
dev.new()
#-----------------------------------------------------------------------------#
boxplot(Yield ~ Treatment, data = my.data,
        main = "Effect of wheat variety on yield",
        xlab = "Wheat cultivar", ylab = "Yield (ton/ha)")
#-----------------------------------------------------------------------------#

# Perform a Shapiro-Wilk test for normality of residuals
cat('Shapiro-Wilk Normality Test\n')
sep(79)
#-----------------------------------------------------------------------------#
shapiro.test(residuals(model))
#-----------------------------------------------------------------------------#
sep(79)

# Perform Levene's Test for homogenity of variances among treatments
cat("Levene's Test\n")
sep(79)
#-----------------------------------------------------------------------------#
leveneTest(Yield ~ Treatment, data = my.data)
#-----------------------------------------------------------------------------#
sep(79)

# Perform a Tukey 1-df Test for Non-additivity
cat("Tukey's 1-df Test for Non-additivity\n")
sep(79)
#-----------------------------------------------------------------------------#
my.data$sq_preds <- predict(model)^2
eat_1df.mod<-lm(Yield ~ Treatment + Block + sq_preds, my.data)
anova(wheat_1df.mod)
#-----------------------------------------------------------------------------#
sep(79)

# Perform LSD mean separation of treatments
cat('Least Significant Difference\n')
sep(79)
#-----------------------------------------------------------------------------#
LSD <- LSD.test(model, "Treatment", console = TRUE)
#-----------------------------------------------------------------------------#
sep(79)
