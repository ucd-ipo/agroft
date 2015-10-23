# This script runs a multivariate ANOVA based on one dependent continous
# variable, two factors, and a factor that represents the block in a Random
# Complete Block Design. It is based on the example given on page 36 of the
# agricolae tutorial[1].
#
# To run the script with full output:
#
# source('rcbd_two_var.R', print.eval = TRUE)
#
# [1] https://cran.r-project.org/web/packages/agricolae/vignettes/tutorial.pdf
#
# NOTE : All lines shown to the user in the app are surrounded by this
# separator: #-----------------------------#

#-----------------------------------------------------------------------------#
library('agricolae')  # for LSD.test()
library('car')  # for levenTest()
library('HH')  # for intxplot()
library('ggplot2')  # for ggplot(), etc.
library('Rmisc')  # for summarySE()
#-----------------------------------------------------------------------------#

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
my.data <- data.frame(block, clone, nitrogen, yield)
write.csv(my.data, 'rcbd_two_var.csv', quote=FALSE, row.names=FALSE)

# Load the data.
#-----------------------------------------------------------------------------#
my.data <- read.csv('rcbd_two_var.csv')
my.data$block <- as.factor(my.data$block)
#-----------------------------------------------------------------------------#

# Use a log10 transformation.
#-----------------------------------------------------------------------------#
my.data$log10.yield <- log10(my.data$yield)
#-----------------------------------------------------------------------------#

# Construct the model.
#-----------------------------------------------------------------------------#
model <- aov(log10.yield ~ block + clone + nitrogen + clone:nitrogen,
             data=my.data)
#-----------------------------------------------------------------------------#

# Create box plot of levels of treatments for each factor (in this case clone &
# nitrogen levels).
dev.new()
#-----------------------------------------------------------------------------#
par(mfrow = c(2, 1))
boxplot(log10.yield ~ clone, data = my.data,
        main = "Effect of clone on yield",
        xlab = "Clone", ylab = "log10(Yield)")
boxplot(log10.yield ~ nitrogen, data = my.data,
        main = "Effect of nitrogen on yield",
        xlab = "Nitrogen Level", ylab = "log10(Yield)")
#-----------------------------------------------------------------------------#
invisible(dev.copy(png, 'rcbd-two-var-box-plots.png'))
invisible(dev.off())

# Plot two standard fit plots: residuals vs predicted, Normal Q-Q plot of the
# residuals.
dev.new()
#------------------------------------------------------------------------------#
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))
plot(model, c(1, 2))
#------------------------------------------------------------------------------#
invisible(dev.copy(png, 'rcbd-two-var-fit-plots.png'))
invisible(dev.off())

# Make sure the residuals are normal (this can also be seen in the Q-Q plot).
cat('Shapiro-Wilk Normality Test\n')
sep(79)
#------------------------------------------------------------------------------#
shapiro.test(residuals(model))
#------------------------------------------------------------------------------#
sep(79)

# Run Levene's Test for a one-way ANOVA of each of the main factors.
# TODO : clone is significant in Levene's test so transformation is necessary.
cat("Levene's Test\n")
sep(79)
#------------------------------------------------------------------------------#
leveneTest(log10.yield ~ clone, data=my.data)
leveneTest(log10.yield ~ nitrogen, data=my.data)
#------------------------------------------------------------------------------#
sep(79)

# Perform a Tukey 1-df Test for Non-additivity
# TODO : It may be necessary to use as.factor() on the factor variables if we
# have to use lm() here, since squared.preds is continuous.
cat("Tukey 1-df Test for Non-additivity\n")
sep(79)
#------------------------------------------------------------------------------#
my.data$squared.preds <- predict(model)^2
one.df.model <- lm(log10.yield ~ clone + nitrogen + clone:nitrogen + block +
                   squared.preds, my.data)
anova(one.df.model)
#------------------------------------------------------------------------------#
sep(79)

# Print the ANOVA table of the fit. The user will have to note the significant
# factors. In this case all factors, including interaction nitrogen:clone, are
# significant.
cat('ANOVA Table\n')
sep(79)
#------------------------------------------------------------------------------#
Anova(model, type="III")
#------------------------------------------------------------------------------#
sep(79)

# Plot the mean yield with respect to each clone for each N level and vice
# versa.
dev.new()
#------------------------------------------------------------------------------#
intxplot(log10.yield ~ clone, groups = nitrogen, data = my.data, se = TRUE,
         ylim = range(my.data$log10.yield), offset.scale = 500)
#------------------------------------------------------------------------------#
invisible(dev.copy(png, 'rcbd-two-var-clone-int-plot.png'))
invisible(dev.off())
dev.new()
#------------------------------------------------------------------------------#
intxplot(log10.yield ~ nitrogen, groups = clone, data = my.data, se = TRUE,
         ylim = range(my.data$log10.yield), offset.scale = 500)
#------------------------------------------------------------------------------#
invisible(dev.copy(png, 'rcbd-two-var-nitrogen-int-plot.png'))
invisible(dev.off())

# The ANOVA table shows that each variable and the interaction are significant,
# so we then see which levels of both clone and nitrogen are significant with
# respect to each other using least significant difference.
cat('Least Significant Difference\n')
sep(79)
#------------------------------------------------------------------------------#
lsd.results <- LSD.test(model, c('clone', 'nitrogen'), console = TRUE)
#------------------------------------------------------------------------------#
sep(79)

# Create Post-hoc Bar Graph
dev.new()
#-----------------------------------------------------------------------------#
summary.stats <- summarySE(data = my.data, "log10.yield",
                           groupvars = c("clone", "nitrogen"))
summary.stats$trt <- apply(summary.stats[ , c("clone", "nitrogen")], 1, paste,
                           collapse = ":")
merged.table <- merge(summary.stats, lsd.results$groups, by = 'trt')
ggplot(merged.table, aes(x = trt, y = means, ymax = 1.5)) +
  geom_bar(stat = "identity", fill = "gray50", colour = "black", width = 0.7) +
  geom_errorbar(aes(ymax = means + se, ymin = means - se), width = 0.0,
                size = 0.5, color = "black") +
  geom_text(aes(label = M, y = means + se / 1.8, vjust = -2.5), size = 6) +
  labs(x = "Treatment", y = "log10(Yield)") +
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
invisible(dev.copy(png, 'rcbd-two-var-clone-bar-graph.png'))
invisible(dev.off())
