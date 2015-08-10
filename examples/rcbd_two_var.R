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

##Testing various transformations for improving assumption tests ==> LOG-TRANSFORM was best##
#Note, you must load data fresh each time any of the 3 transformations are run
#(1) Create a sqrt-transformed variable
my.data[,4]<-sqrt(my.data[,4])

#(2) Create a log-transformed variable
my.data[,4]<-log10(my.data[,4])

#(3)----- Finding the exponent for a power transformation ---- #
my.data$merged_treatment <- paste(clone, nitrogen, sep = "")
as.factor(my.data$merged_treatment)
str(my.data)
means <- aggregate(my.data$yield, list(my.data$merged_treatment), mean)
vars <- aggregate(my.data$yield, list(my.data$merged_treatment), var)
logmeans <- log10(means$x)
logvars <- log10(vars$x)
power.mod<-lm(logvars ~ logmeans)
summary<-summary(power.mod)
#identify the slope
summary$coefficients[2,1]
#calculate the appropriate power of the transformation, where Power = 1 – (slope/2)
power <- 1-(summary$coefficients[2,1])/2
power
#Create power-tranformed variable
my.data$yield<-(my.data$yield)^(power)

# Construct the model.
#-----------------------------------------------------------------------------#
model <- aov(yield ~ block + clone + nitrogen + clone:nitrogen, data=my.data)
#-----------------------------------------------------------------------------#

# Create Box Plot of levels of treatments for each factor (in this case clone &
# nitrogen levels).
dev.new()
#-----------------------------------------------------------------------------#
par(mfrow = c(2, 1))
boxplot(yield ~ clone, data = my.data,
        main = "Effect of clone on yield",
        xlab = "Clone", ylab = "Yield (?)")
boxplot(yield ~ nitrogen, data = my.data,
        main = "Effect of nitrogen on yield",
        xlab = "Nitrogen Level", ylab = "Yield (?)")
#-----------------------------------------------------------------------------#

# Plot two standard fit plots: residuals vs predicted, Normal Q-Q plot of the
# residuals.
dev.new()
#------------------------------------------------------------------------------#
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))
plot(model, c(1, 2))
#------------------------------------------------------------------------------#

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
leveneTest(yield ~ clone, data=my.data)
leveneTest(yield ~ nitrogen, data=my.data)
#------------------------------------------------------------------------------#
sep(79)

# Generate predicted values for Tukey 1-df Test
# Perform a Tukey 1-df Test for Non-additivity
cat("Tukey 1-df Test for Non-additivity\n")
sep(79)
#------------------------------------------------------------------------------#
my.data$sq_preds <- predict(model)^2
one.df.model <- lm(yield ~ clone + nitrogen + clone:nitrogen + block + sq_preds, my.data)
anova(one.df.model)
#------------------------------------------------------------------------------#
sep(79)

# Print the ANOVA table of the fit. The user will have to note the significant
# factors. In this case all factors, including interaction nitrogen:clone, are
# significant.
cat('ANOVA Table\n')
sep(79)
#------------------------------------------------------------------------------#
anova(model)
#------------------------------------------------------------------------------#
sep(79)

# Show the confidence intervals.
cat('Confidence Intervals\n')
sep(79)
#------------------------------------------------------------------------------#
confint(model)
#------------------------------------------------------------------------------#
sep(79)

# Plot the mean yield with respect to each clone for each N level and vice
# versa.
# TODO : This call to par() is not making the intxplots subplots for some
# reason.
dev.new()
#------------------------------------------------------------------------------#
par(mfrow = c(2, 1))
intxplot(yield ~ clone, groups = nitrogen, data=my.data, se=TRUE,
         ylim=range(my.data$yield), offset.scale=500)
intxplot(yield ~ nitrogen, groups = clone, data=my.data, se=TRUE,
         ylim=range(my.data$yield), offset.scale=500)
#------------------------------------------------------------------------------#

# The ANOVA table shows that each variable and the interaction are significant,
# so we then see which levels of both clone and nitrogen are significant with
# respect to each other using least significant difference.
cat('Least Significant Difference\n')
sep(79)
#------------------------------------------------------------------------------#
LSD.test(model, c('clone', 'nitrogen'), console=TRUE)
#------------------------------------------------------------------------------#
sep(79)
