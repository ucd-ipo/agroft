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

# Show the confidence intervals for the intercept and each virus level.
cat('Confidence Intervals\n')
sep(79)
#-----------------------------------------------------------------------------#
confint(model)
#-----------------------------------------------------------------------------#
sep(79)

# Plot two standard fit plots: residuals vs predicted and Normal Q-Q plot of the
# residuals.
dev.new()
#-----------------------------------------------------------------------------#
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))  # plots as subplots of single graph
plot(model, which = c(1, 2))
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
wheat_1df.mod<-lm(Yield ~ Treatment + Block + sq_preds, my.data)
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

##Create Post-hoc Bar Graph with LSD letter labels##
#Save lsd summary
lsd <- LSD.test(model, "Treatment", group = TRUE)
#Save each component in temporary df to use in creating bar graph
lsd.letters <- as.character(lsd$groups[,3])
lsd.trt.means <- (lsd$groups[,2])
lsd.trt.names <- (lsd$groups[,1])
table1 <- data.frame(lsd.trt.names,lsd.trt.means,lsd.letters)
#Calc standard errors for error bars in graph, and save in temporary df 
Data2 <- data.frame(summarySE(data=my.data, 
                              "Yield", 
                              groupvars="Treatment", 
                              conf.interval = 0.95))
#merge into one df so we have LSD letters matching with means and standard errors
merged_table <- merge(Data2,table1,by.x='Treatment',by.y='lsd.trt.names')

#create bar graph
library(ggplot2) 
ggplot(merged_table, 
       aes(x = Treatment, y = lsd.trt.means, 
           ymax=1.25*max(lsd.trt.means), ymin=0.0))  + #note scale depends on dataset
  geom_bar(stat="identity", fill="gray50",
           colour = "black", width = 0.7)  +
  geom_errorbar(aes(ymax=lsd.trt.means+se, ymin=lsd.trt.means-se), 
                width=0.0, size=0.5, color="black")  +
  geom_text(aes(label=lsd.letters,
                y = lsd.trt.means + se/2, vjust=-1.5), size=6) +
  labs(x = "Variety",   #should be customizable
       y = "Yield")  +  #should be customizable
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(2), 
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5),
        panel.border = element_rect(colour="black"),
        text = element_text(size=20)          
  )
