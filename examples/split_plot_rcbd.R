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

##Testing various transformations for improving assumption tests##
#(1) Create a sqrt-transformed variable
my.data[,4]<-sqrt(my.data[,4])

#(2) Create a log-transformed variable
my.data[,4]<-log10(my.data[,4])

#(3)----- Finding the exponent for a power transformation ---- #
my.data$merged_treatment <- paste(my.data$SeedLotA, my.data$TrtmtB, sep = "-")
as.factor(my.data$merged_treatment)
str(my.data)
means <- aggregate(my.data$Yield, list(my.data$merged_treatment), mean)
vars <- aggregate(my.data$Yield, list(my.data$merged_treatment), var)
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
my.data$Yield<-(my.data$Yield)^(power)

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
#Save error and df terms for main-plot error and sub-plot error (i.e. SeedLotA and TrtmtB, respectively) 
 #to be used later for LSD. Note that '[4]' in the following command represents the 
 #4th position of the Block:SeedLotA term in model.tmp. That order would need to be fixed for the following to always work:
mp_error <- summary(model.tmp)[[1]][["Mean Sq"]][4]
mp_df <- summary(model.tmp)[[1]][["Df"]][4]
sp_error <- summary(model.tmp)[[1]][["Mean Sq"]][6]
sp_df <- summary(model.tmp)[[1]][["Df"]][6]

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

# Add Levene's Test.
leveneTest(Yield ~ SeedLotA, data=my.data)
leveneTest(Yield ~ TrtmtB, data=my.data)

#1-df Tukey:
my.data$sq_preds <- predict(model.tmp)^2
one.df.model <- lm(Yield ~ SeedLotA + Block + SeedLotA:Block + TrtmtB + SeedLotA:TrtmtB + sq_preds, my.data)
anova(one.df.model)

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
#ANOVA table shows significant interaction ==> must do LSD for simple effects (all 12 combinations of SeedLotA and TrtmtB)
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
#plot interaction plots
library(HH)
intxplot(Yield ~ SeedLotA, groups = TrtmtB, data=my.data, se=TRUE, ylim=range(my.data$Yield),
  offset.scale=500)
intxplot(Yield ~ TrtmtB, groups = SeedLotA, data=my.data, se=TRUE, ylim=range(my.data$Yield),
  offset.scale=500)
# TODO : Do we simply check for the SeedLotA in this example? Can we use the
# LSD.test() on the model without the error term for the correct results?
cat('Least Significant Difference\n')
sep(50)
#-----------------------------------------------------------------------------#
#LSD.test(model.tmp, 'SeedLotA', alpha=alpha, console=TRUE) #This happens to give the same mean separations as line169, 
              #but uses wrong LSD value, and it gives wrong conf intervals..
####Means comparisons####
#(1)If the interaction between main plot * subplot is NOT significant, do both:
  #(a)Comparisons among main plot levels
   MP_comparison<-LSD.test(my.data$Yield, my.data$SeedLotA, DFerror = mp_df,
                        MSerror = mp_error)
   MP_comparison
  #(b)Comparisons among subplot levels
    #To compare subplots, it is not necessary to specify subplot error because 
     #it is the residual error (the default MSE for all F tests):
    SP_comparison<-LSD.test(my.data$Yield, my.data$TrtmtB, DFerror = sp_df,
                            MSerror = sp_error)
    SP_comparison    
#-----------------------------------------------------------------------------#
sep(50)
