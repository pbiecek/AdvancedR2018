### Instalacja ###
install.packages("vdmR")
library("vdmR")

tempdir() # ścieżka do plików tymczasowych

### Przykład ###
data("vsfuk2012", package = "vdmR")
head(vsfuk2012[,1:5])

vscat(MortalityRate, FertilityRate, vsfuk2012, "scat01", "vsfuk2012")
vhist(MarriageRate, vsfuk2012, "hist01", "vsfuk2012")

vlaunch(vsfuk2012, "main", "vsfuk2012")

### Jedno okno ###
vlaunch(vsfuk2012, "main", "vsfuk2012", iframe = TRUE)

### Pobieranie zaznaczonych danych z okna ### Wcześniej Ctrl+C w głównym oknie html
vsfuk2012.sub <- read.table("clipboard", header = TRUE)


#########################################################################
vscat(MortalityRate, FertilityRate, vsfuk2012, "scat01", "vsfuk2012",
      color = Type, size = pop_male)
vlaunch(vsfuk2012, "main", "vsfuk2012")


vhist(MarriageRate, vsfuk2012, "hist01", "vsfuk2012",
      fill = I("green"), color = I("black"))
vlaunch(vsfuk2012, "main", "vsfuk2012")


vpcp(vsfuk2012, 4:17, "pcp1", "vsfuk2012", alphaLines = 0.1,
     scale = "std", missing = "mean")
vlaunch(vsfuk2012, "main", "vsfuk2012")

 
# install.packages("maptools")
# library("maptools")
# shp.path <- file.path(system.file(package = "vdmR"),
#                       "etc/shapes/fukuoka2012.shp")
# vsfuk2012.spdf <- readShapeSpatial(shp.path, IDvar = "CityCode")
# head(vsfuk2012.spdf@data)
# 
# frcol <- ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red",
#                               midpoint = median(vsfuk2012$FertilityRate))
# vcmap(shp.path, vsfuk2012, "CityCode", "CityCode", "map1", "vsfuk2012",
#       fill = FertilityRate, ggscale = frcol)
# vlaunch(vsfuk2012, "main", "vsfuk2012")
