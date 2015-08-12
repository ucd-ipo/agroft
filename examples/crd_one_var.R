# This script runs a one-way ANOVA based on one dependent continuous variable
# and one independent factor which come from a completely randomized experiment
# design. It is based on the example given on page 27 of the agricolae tutorial
# [1]. Everything displayed to the terminal and the two plots will be displayed
# in the app. To run the script with full output:
#
# source('crd_one_var.R', print.eval = TRUE)
#
# [1] https://cran.r-project.org/web/packages/agricolae/vignettes/tutorial.pdf
#
# NOTE : All lines shown to the user in the app are surrounded by this
# separator: `#-----------#`

#-----------------------------------------------------------------------------#
library('agricolae')  # for sweetpotato, LSD.test()
library('car')  # for leveneTest()
library('ggplot2')  # for ggplot()
library('Rmisc')  # for summarySE()
#-----------------------------------------------------------------------------#

sep <- function(n){
  # This function simply prints a line of "=" to the screen as a separator and
  # will not show up in the app. It is only here for nice printing.
  line <- paste0(paste(replicate(n, "="), collapse = ""), '\n')
  cat(line)
}

# The sweetpotato data set is a completely random experiment design with plots
# of 79 sweet potatoes where there are three viruses and a control applied in 3
# reps, i.e. 3 plots for each of four virus levels.
#-----------------------------------------------------------------------------#
data(sweetpotato)  # load data from agricolae
my.data <- sweetpotato  # rename the data frame
#-----------------------------------------------------------------------------#

# Construct the model.
#-----------------------------------------------------------------------------#
model <- aov(yield ~ virus, data = my.data)
#-----------------------------------------------------------------------------#

# Box plot of the effect of virus on yield. The user will need to be able to set
# the axes labels in the app.
dev.new()
#-----------------------------------------------------------------------------#
boxplot(yield ~ virus, data = my.data, main = "Effect of virus on yield",
        xlab = "virus", ylab = "yield")
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'crd-one-var-box-plots.png'))
invisible(dev.off())

# Plot two standard fit plots: residuals vs predicted and Normal Q-Q plot of the
# residuals.
dev.new()
#-----------------------------------------------------------------------------#
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))  # plots as subplots of single graph
plot(model, which = c(1, 2))
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'crd-one-var-fit-plots.png'))
invisible(dev.off())

# Make sure the residuals are normal (this can also be seen in the Q-Q plot).
cat('Shapiro-Wilk Normality Test\n')
sep(79)
#-----------------------------------------------------------------------------#
shapiro.test(residuals(model))
#-----------------------------------------------------------------------------#
sep(79)

# Run Levene's Test.
cat("Levene's Test\n")
sep(79)
#-----------------------------------------------------------------------------#
leveneTest(yield ~ virus, data = my.data)
#-----------------------------------------------------------------------------#
sep(79)

# Print the ANOVA table of the fit. The user should will have to note the
# significant factors. In this case the single virus factor is significant.
cat('ANOVA Table\n')
sep(79)
#-----------------------------------------------------------------------------#
anova(model)
#-----------------------------------------------------------------------------#
sep(79)

# The ANOVA table shows that the virus factor is significant, so we then see
# which levels are significant with respect to each other using least
# significant difference.
cat('Least Significant Difference\n')
sep(79)
#-----------------------------------------------------------------------------#
lsd.results <- LSD.test(model, "virus", console=TRUE)
#-----------------------------------------------------------------------------#
sep(79)

# Create Post-hoc Bar Graph
#-----------------------------------------------------------------------------#
summary.stats <- summarySE(data = my.data, "yield", groupvars = "virus")
merged.table <- merge(summary.stats, lsd.results$groups, by.x = 'virus',
                      by.y = 'trt')
dev.new()
ggplot(merged.table, aes(x = virus, y = means, ymax = 50, ymin = 0.0)) +
  geom_bar(stat = "identity", fill = "gray50", colour = "black", width = 0.7) +
  geom_errorbar(aes(ymax = means + se, ymin = means - se), width = 0.0,
                size = 0.5, color = "black") +
  geom_text(aes(label = M, y = means + se / 1.8, vjust = -2.5), size = 6) +
  labs(x = "Virus", y = "Yield") +
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
invisible(dev.copy(png, 'crd-one-var-bar-graph.png'))
invisible(dev.off())
