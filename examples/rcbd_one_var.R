# This is an example of a single variate RCBD.
#
# To run the script type:
# source('rcbd_one_var.R', print.eval = TRUE)
#
# The code that will be displayed to the user is bounded by this separator:
#-----------------------------------------------------------------------------#

# Load the necessary libraries.
#-----------------------------------------------------------------------------#
library('car')  # for leveneTest()
library('agricolae')  # for LSD.test()
library('Rmisc')  # for summarSE()
library('ggplot2')  # for ggplot(), etc.
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
#-----------------------------------------------------------------------------#
model <- aov(Yield ~ Treatment + Block, my.data)
#-----------------------------------------------------------------------------#

# Print the ANOVA table for the model.
cat('ANOVA Table\n')
sep(79)
#-----------------------------------------------------------------------------#
anova(model)
#-----------------------------------------------------------------------------#
sep(79)

# Plot two standard fit plots: residuals vs predicted and Normal Q-Q plot of the
# residuals.
dev.new()
#-----------------------------------------------------------------------------#
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))  # plots as subplots of single graph
plot(model, which = c(1, 2))
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'rcbd-one-var-fit-plots.png'))
invisible(dev.off())

# Plot a kernel density plot of the residuals.
dev.new()
#-----------------------------------------------------------------------------#
plot(density(residuals(model)))
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'rcbd-one-var-density-plot.png'))
invisible(dev.off())

# Create Box Plot of treatments
dev.new()
#-----------------------------------------------------------------------------#
boxplot(Yield ~ Treatment, data = my.data,
        main = "Effect of wheat variety on yield",
        xlab = "Wheat cultivar", ylab = "Yield (ton/ha)")
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'rcbd-one-var-box-plot.png'))
invisible(dev.off())

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
one.dof.mod <- lm(Yield ~ Treatment + Block + sq_preds, data = my.data)
anova(one.dof.mod)
#-----------------------------------------------------------------------------#
sep(79)

# Perform LSD mean separation of treatments
cat('Least Significant Difference\n')
sep(79)
#-----------------------------------------------------------------------------#
lsd.results <- LSD.test(model, "Treatment", console = TRUE)
#-----------------------------------------------------------------------------#
sep(79)

# Create Post-hoc Bar Graph
dev.new()
#-----------------------------------------------------------------------------#
summary.stats <- summarySE(data = my.data, "Yield", groupvars = "Treatment")
merged.table <- merge(summary.stats, lsd.results$groups, by.x = 'Treatment',
                      by.y = 'trt')
ggplot(merged.table, aes(x = Treatment, y = means,)) +
  geom_bar(stat = "identity", fill = "gray50", colour = "black", width = 0.7) +
  geom_errorbar(aes(ymax = means + se, ymin = means - se), width = 0.0,
                size = 0.5, color = "black") +
  geom_text(aes(label = M, y = means + se / 1.8, vjust = -2.5), size = 6) +
  labs(x = "Treatment", y = "Yield") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5),
        panel.border = element_rect(colour = "black"),
        text = element_text(size = 20)
  )
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'rcbd-one-var-bar-graph.png'))
invisible(dev.off())
