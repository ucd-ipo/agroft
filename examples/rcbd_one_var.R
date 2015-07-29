wheat.data <- read.csv("Wheat_yield_data.csv", header = TRUE, stringsAsFactors = FALSE)

library(car)  # for leveneTest()
library(agricolae)  # for LSD.test()

#Inform R that Block and Treatment are factors
wheat.data$Block<-as.factor(wheat.data$Block)
wheat.data$Treatment<-as.factor(wheat.data$Treatment)

##The ANOVA##
wheat.mod<-lm(Yield ~ Treatment + Block, wheat.data)
anova(wheat.mod)

##TESTING ASSUMPTIONS##
#Generate residual and predicted values
wheat.data$resids <- residuals(wheat.mod)
wheat.data$preds <- predict(wheat.mod)
wheat.data$sq_preds <- wheat.data$preds^2

#Look at a plot of residual vs. predicted values
plot(resids ~ preds, data = wheat.data,
     xlab = "Predicted Values", ylab = "Residuals")
#Create Box Plot of treatments
boxplot(Yield ~ Treatment, data = wheat.data,
        main = "Effect of wheat variety on yield",
        xlab = "Wheat cultivar", ylab = "Yield (ton/ha)")
#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(wheat.data$resids)
qqnorm(resid(wheat.mod))
plot(density(resid(wheat.mod)))

#Perform Levene's Test for homogenity of variances among treatments
leveneTest(Yield ~ Treatment, data = wheat.data)

#Perform a Tukey 1-df Test for Non-additivity
wheat_1df.mod<-lm(Yield ~ Treatment + Block + sq_preds, wheat.data)
anova(wheat_1df.mod)

##POST-HOC##
#Perform LSD mean separation of treatments
LSD <- LSD.test(wheat.mod, "Treatment")
