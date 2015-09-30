# This is an example of a split-plot RCBD experimental design. 
#
# To run the script type:
#
# source('split_plot_rcbd.R', print.eval = TRUE)
#
#
# NOTE : The code that will be shown to the user in the app is bounded by these
# separators: #------------#.

# Load the necessary libraries.
#-----------------------------------------------------------------------------#
library('agricolae')  # for LSD.test()
library('lmerTest') # for lmer()
library('car')  # for leveneTest()
library('HH')  # for intxplot()
library('lsmeans') #for Tukey or LSD test
library('multcompView') #for cld (compact letter display) methods 
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
model <- lmer(Yield ~ SeedLotA*TrtmtB + (1|Block) + (1|Block:SeedLotA), 
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
# main-plot is sig in ANOVA, do the following:
cat('Tukey method for multiple mean comparison: Interaction Insignficant\n')
sep(79)
#-----------------------------------------------------------------------------#
# Comparisons among main plot levels
cld(lsmeans(model, tukey ~ SeedLotA)) #does number groupings - can we switch to letters?


model.summary <- summary(model)[[1]]
trim <- function (x) sub("\\s+$", "", x)  # trims trailing whitespace
row.names(model.summary) <- trim(row.names(model.summary))

# Comparisons among main plot levels
main.plot.error <- model.summary["SeedLotA", "Mean Sq"]
main.plot.dof <- model.summary["SeedLotA", "Df"]
main.plot.lsd.results <- LSD.test(my.data$Yield, my.data$SeedLotA, DFerror =
                                  main.plot.dof, MSerror = main.plot.error,
                                  console = TRUE)

# Comparisons among subplot levels
# NOTE : To compare subplots, it is not necessary to specify subplot error
# because it is the residual error (the default MSE for all F tests).
split.plot.error <- model.summary["TrtmtB", "Mean Sq"]
split.plot.dof <- model.summary["TrtmtB", "Df"]
split.plot.lsd.results <- LSD.test(my.data$Yield, my.data$TrtmtB, DFerror =
                                   split.plot.dof, MSerror = split.plot.error,
                                   console = TRUE)
#-----------------------------------------------------------------------------#
sep(79)

# ANOVA table shows significant interaction ==> must do LSD for simple effects
# (all 12 combinations of SeedLotA and TrtmtB)
cat('Least Significant Difference: Interaction Signficant\n')
sep(79)
#-----------------------------------------------------------------------------#
# (a) Comparisons among subplot levels within a main plot level.
main.plot.levels <- c(1:nlevels(my.data$SeedLotA))
for (i in main.plot.levels) {
  with(subset(my.data, SeedLotA == main.plot.levels[i]),
       {
        sep(79)
        print(paste('SeedLotA = ', main.plot.levels[i]))
        model.i <- aov(Yield ~ Block + TrtmtB)
        summary(model.i)
        # TODO : Ensure this is always grabbed from the correct row.
        p_value <- summary(model.i)[[1]][["Pr(>F)"]][2]
        if(p_value < 0.05) {
          LSD.test(model.i, 'TrtmtB', console = TRUE)
        } else {
          print('Treatment effect not significant, thus no LSD is performed')
        }
       }
      )
}
# (b) Comparisons among main plot levels within a subplot level.
split.plot.levels <- c(1:nlevels(my.data$TrtmtB))
for (i in split.plot.levels) {
  with(subset(my.data, TrtmtB == split.plot.levels[i]),
       {
        sep(79)
        print(paste('TrtmtB =', split.plot.levels[i]))
        model.i <- aov(Yield ~ Block + SeedLotA)
        summary(model.i)
        # TODO : Ensure this is always grabbed from the correct row.
        p_value <- summary(model.i)[[1]][["Pr(>F)"]][2]
        if(p_value < 0.05) {
          LSD.test(model.i, 'SeedLotA', console = TRUE)
        } else {
          print('Treatment effect not significant, thus no LSD is performed')
        }
       }
      )
}
# (c) Comparisons between subplot levels across different main plot levels
# TODO :Trying to figure out how to code for this. It's more complicated.
#-----------------------------------------------------------------------------#
sep(79)
