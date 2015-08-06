# This script runs a single variate ANOVA based on one dependent continuous
# variable and one independent factor. It is based on the example given on page
# 27 of the agricolae tutorial [1]. Everything displayed to the terminal and the
# two plots will be displayed in the app. To run the script with full output:
#
# source('crd_one_var.R', print.eval = TRUE)
#
# [1] https://cran.r-project.org/web/packages/agricolae/vignettes/tutorial.pdf
#
# NOTE : All lines shown to the user in the app are surrounded by this
# separator: #-----------------------------#

#-----------------------------------------------------------------------------#
library('agricolae')  # for sweetpotato, LSD.test()
library('car')  # for leveneTest()
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

# Box plot of the effect on virus on yield.
dev.new()
#-----------------------------------------------------------------------------#
boxplot(yield ~ virus, data = my.data, main = "Effect of virus on yield",
        xlab = "virus", ylab = "yield (?)")
#-----------------------------------------------------------------------------#

# Plot two standard fit plots: residuals vs predicted, Normal Q-Q plot of the
# residuals.
dev.new()
#-----------------------------------------------------------------------------#
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))  # plots as subplots of single graph
plot(model, which = c(1, 2))
#-----------------------------------------------------------------------------#

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

#------------------------------------------------------------------------------#
sep(79)

# Print the ANOVA table of the fit. The user should will have to note the
# significant factors. In this case the single virus factor is significant.
cat('ANOVA Table\n')
sep(79)
#-----------------------------------------------------------------------------#
anova(model)
#-----------------------------------------------------------------------------#
sep(79)

# Show the confidence intervals for the intercept and each virus level.
cat('Confidence Intervals\n')
sep(79)
#-----------------------------------------------------------------------------#
confint(model)
#-----------------------------------------------------------------------------#
sep(79)

# The ANOVA table shows that the virus factor is significant, so we then see
# which levels are significant with respect to each other using least
# significant difference.
cat('Least Significant Difference\n')
sep(79)
#-----------------------------------------------------------------------------#
LSD.test(model, "virus", console=TRUE)
#-----------------------------------------------------------------------------#
sep(79)

##Create Post-hoc Bar Graph with LSD letter labels##
#Save lsd summary
lsd <- LSD.test(model, "virus", group = TRUE)
#Save each component in temporary df to use in creating bar graph
lsd.letters <- as.character(lsd$groups[,3])
lsd.trt.means <- (lsd$groups[,2])
lsd.trt.names <- (lsd$groups[,1])
table1 <- data.frame(lsd.trt.names,lsd.trt.means,lsd.letters)
#Calc standard errors for error bars in graph, and save in temporary df 
Data2 <- data.frame(summarySE(data=my.data, 
                  "yield", 
                  groupvars="virus", 
                  conf.interval = 0.95))
#merge into one df so we have LSD letters matching with means and standard errors
merged_table <- merge(Data2,table1,by.x='virus',by.y='lsd.trt.names')

#create bar graph
library(ggplot2) 
ggplot(merged_table, 
       aes(x = virus, y = lsd.trt.means, 
           ymax=50, ymin=0.0))  + #note scale depends on dataset
  geom_bar(stat="identity", fill="gray50",
           colour = "black", width = 0.7)  +
  geom_errorbar(aes(ymax=lsd.trt.means+se, ymin=lsd.trt.means-se), 
                width=0.0, size=0.5, color="black")  +
 geom_text(aes(label=lsd.letters,
               y = lsd.trt.means + se/1.8, vjust=-2.5)) +
  labs(x = "Virus",   #should be customizable
       y = "Yield")  +  #should be customizable
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5),
        panel.border = element_rect(colour="black")
  )
