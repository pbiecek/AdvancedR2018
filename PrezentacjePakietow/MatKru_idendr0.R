
set.seed(123)
data <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv')

data <- data[sample(nrow(data)),][1:100,-1]
rownames(data) <- NULL

# table(data[,'Region'])
# 1 - Lizbona
# 2 - Porto
# 3 - Inne

hc <- hclust(dist(data[,-1]))

# install.packages('idendr0')
library(idendr0)

# idendro(hc) # podstawowe wywołanie
# idendro(hc,data) # dodatkowo heatmapa


options(device = 'X11') #jesli linux
# options(device = 'windows') #jesli windows
plot(data$Milk,data$Grocery, pch = 19)
colorizeCallback <- function(clr) {
  clusterColors <- c('black', 'red', 'green', 'blue', 'yellow', 'magenta',
                     'cyan', 'darkred', 'darkgreen', 'purple', 'darkcyan')
  plot(data$Milk,data$Grocery,
       col = clusterColors[clr + 1], pch = 19)
}
idendro(hc, data, colorizeCallback = colorizeCallback)



# wymaga niestety dodatkowych bibliotek, samo install.packages() nie zadziała
# install.packages('rggobi')
# library(rggobi)
# idendro(hc, data, ggobi = TRUE, ggobiGlyphType = 4, ggobiGlyphSize = 3)

