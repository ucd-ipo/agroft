# This is an example of a split-plot RCBD experimental design. 
#
# To run the script type:
#
# source('split_plot_rcbd.R', print.eval = TRUE)
#
#
# NOTE : The code that will be shown to the user in the app is bounded by these
# separators: #------------#.

# Load the necessary packages.
#-----------------------------------------------------------------------------#
library(nlme)
library(agricolae)  # for LSD.test()
library(car)  # for leveneTest()
library(HH)   # for intxplot()
library(lsmeans) #for Tukey or LSD test
library(multcompView) #for cld (compact letter display) methods 
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
#-----------------------------------------------------------------------------#
my.data$SeedLotA <- as.factor(my.data$SeedLotA)
my.data$Block <- as.factor(my.data$Block)
my.data$TrtmtB <- as.factor(my.data$TrtmtB)
#-----------------------------------------------------------------------------#

# This is the standard model for a split-plot RCBD, where 'SeedLotA' is mainplot
# within 'block' in RCBD, and 'TrtmtB' is subplot within mainplot.
#-----------------------------------------------------------------------------#
model <- lme(fixed = Yield ~ SeedLotA*TrtmtB,
             random = ~1|Block/SeedLotA,
              data = my.data)
#-----------------------------------------------------------------------------#

# Plot the four standard fit plots: residuals vs predicted and the Normal Q-Q
# plot of the residuals.
dev.new()
#-----------------------------------------------------------------------------#
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))
plot(model, which = c(1, 2))
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'split-plot-rcbd-fit-plots.png'))
invisible(dev.off())

# NOTE: Do not do the Shapiro-Wilk Normality Test because it is not straight
# forward for a split-plot design.

# Levene's Test.
cat("Levene's Test\n")
sep(79)
#-----------------------------------------------------------------------------#
leveneTest(Yield ~ SeedLotA, data = my.data)
leveneTest(Yield ~ TrtmtB, data = my.data)
#-----------------------------------------------------------------------------#
sep(79)

cat('ANOVA Table\n')
sep(79)
#-----------------------------------------------------------------------------#
anova(model)
#-----------------------------------------------------------------------------#
sep(79)

# Plot the mean yield with respect to each factor.
dev.new()
#-----------------------------------------------------------------------------#
intxplot(Yield ~ SeedLotA, groups = TrtmtB, data = my.data, se = TRUE,
         ylim = range(my.data$Yield), offset.scale = 500)
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'split-plot-rcbd-main-plot-interaction-plot.png'))
invisible(dev.off())

dev.new()
#-----------------------------------------------------------------------------#
intxplot(Yield ~ TrtmtB, groups = SeedLotA, data = my.data, se = TRUE,
         ylim = range(my.data$Yield), offset.scale = 500)
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'split-plot-rcbd-split-plot-interaction-plot.png'))
invisible(dev.off())

# If the interaction between main-plot : subplot is NOT significant AND the
# main-plot IS sig in ANOVA, do the following:
cat('Tukey method for multiple mean comparison of mainplot: Interaction Insignficant\n')
sep(79)
#-----------------------------------------------------------------------------#
# Comparisons among main plot levels
cld(lsmeans(model, tukey ~ SeedLotA)) #does number groupings - can we switch to letters?
#-----------------------------------------------------------------------------#
sep(79)
# If the interaction between main-plot : subplot is NOT significant AND the
# sub-plot IS sig in ANOVA, do the following:
cat('Tukey method for multiple mean comparison of sub-plot: Interaction Insignficant\n')
sep(79)
#-----------------------------------------------------------------------------#
# Comparisons among sub-plot levels
cld(lsmeans(model, tukey ~ TrtmtB)) #does number groupings - can we switch to letters?
#-----------------------------------------------------------------------------#
sep(79)
# ANOVA table shows significant interaction ==> must do Tukey on simple effects
# (all 16 combinations of SeedLotA and TrtmtB)
cat('Least Significant Difference: Interaction Signficant\n')
sep(79)
#-----------------------------------------------------------------------------#
# Comparisons among all combinations of mainplot and subplot levels
cld(lsmeans(model, tukey ~ SeedLotA + TrtmtB))


