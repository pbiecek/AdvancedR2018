# install.packages("mplot")

library(mplot)
library(MASS)
library(ggplot2)

############################################################################################################
# Example - diabetes
############################################################################################################

head(diabetes)

# interactive version 
# http://garthtarr.com/apps/mplot

# Linear Regression
lm.d <- lm(y ~ ., data = diabetes)

# 200 bootstrap sample
vis.d <- vis(lm.d, B = 200, seed = 1)
af.d <- af(lm.d, B = 200, n.c = 100, c.max = 100, seed = 1)

# Variable Inclusion Plot
plot(vis.d, interactive = FALSE, which = "vip")

# Comment: As the penalty value increases, and a more parsimonious model is sought, the hdl variable is
# selected more frequently while at the same time other variables with similar information are
# dropped. Such paths occur when a group of variables contains similar information to another
# variable.
# The bmi and ltg paths are horizontal with a bootstrap probability of 1 for all penalty values indicating that they are
# very important variables, as are map and sex.
# The path for the age variable lies below the path for the redundant
# variable, indicating that it does not provide any useful information.


# Bootstrap plot
plot(vis.d, interactive = FALSE, which = "boot", max.circle = 10,
     highlight = "hdl") + scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12))
vis.d
# for example: In models of size four (including theintercept), the model with bmi, ltg and map 
# was selected in 70% of bootstrap resamples.


# Adaptive fence plot
plot(af.d, interactive = FALSE, best.only = TRUE,
     legend.position = "right")
plot(af.d, interactive = FALSE, best.only = FALSE,
     legend.position = "right")

# interactive version 
mplot(lm.d, vis.d, af.d)



