# This examples uses data [1] from a split plot design with a field variable
# added in for a random effect. There are factors for oat variety and nitrogen
# levels. It is based on [2].
#
# To run the example:
# source('split_plot_mixed_effects.R', print.eval = TRUE)
#
# [1] http://finzi.psych.upenn.edu/library/nlme/html/Oats.html
# [2] https://www.stat.wisc.edu/courses/st850-lindstro/handouts/splitplotlme.pdf

library(nlme)  # for Oats

data(Oats)
my.data <- data.frame(Oats)
names(my.data) <- tolower(names(my.data))
# Add in a location variable.
my.data$field <- as.numeric(as.factor(paste(my.data$block, my.data$variety)))

# The field and block are random effects?
model <- lme(fixed = yield ~ variety + nitro + variety:nitro,
             random = ~ 1|block/field, data=my.data)

anova(model)

summary(model)

# No idea what to do after this...
