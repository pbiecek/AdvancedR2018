# polimorficzność

# generic vs message passing

library(breakDown)
head(HR_data,2)

plot(HR_data$average_montly_hours, HR_data$satisfaction_level)
plot(HR_data$salary)

model <- lm(satisfaction_level ~ average_montly_hours, data=HR_data)
plot(model)

stats:::plot.lm(model)


methods(generic.function = "plot")
methods(class = "data.frame")
methods(class = "lm")

plot

print



# klasy S3

uczen <- list(imie = "Artur", wiek = 9)
class(uczen) <- "osobaS3"

uczen <- structure(list(imie = "Artur", wiek = 9), 
                   class = "osobaS3")

print.osobaS3 <- function(x) {
  cat(x$imie, " to osobnik w wieku ", x$wiek, "\n\n")
}

uczen

print(uczen)

print.osobaS3(uczen)

maluj <- function (x, ...)
  UseMethod("maluj")

maluj.default <- function(x, ...)
  print(x)

maluj.osobsS3 <- function(x, ...)
  print(x)

maluj(uczen)

# more classes
class(uczen) <- c("osobaS3", "dzieckoS3")


# klasy S4

setClass("osobaS4", 
         representation(imie="character", wiek="numeric"))

print.osobaS3 <- function(x) {
  cat(x$imie, " to osobnik w wieku ", x$wiek, "\n\n")
}


setMethod("print", "osobaS4",
          function(x) {
            cat(x@imie, " to osobnik w wieku ", x@wiek, "\n\n")
          })

o2 <- new("osobaS4")
o2 <- new("osobaS4", imie="Artur", wiek=9)

print(o2)


setClass("osoba", representation(imie="character",
                                 wiek="numeric"))
## [1] "osoba"

(obiektOsoba <- new("osoba", imie="Artur", wiek=9))
## An object of class “”osoba
## Slot "imie":
## [1] "Przemek"
## Slot "wiek":
## [1] 27

obiektOsoba@imie <- "Artur"
obiektOsoba@imie <- 13
## Error in checkSlotAssignment(object, name, value) :
## assignment of an object of class "numeric" is not valid

setMethod("[", "osoba",
          function(.Object, imie = character("Nieznane"), wiek = numeric(0)) {
            if (nargs() > 1) {
              if(wiek > 120 || wiek < 0)
                stop("Niedopuszczalna wartosc wieku")
              .Object@imie <- imie
              .Object@wiek <- wiek
            }
            .Object
          })

obiektOsoba <- new("osoba", imie="Artur", wiek=2700)
## Error in .local(.Object, ...) : Niedopuszczalna śćwarto wieku
##

setClassUnion("liczbaNapis", c("numeric", "character"))

setClass("osoba2", representation(imie="character", wiek="liczbaNapis"))
obiektOsoba2 <- new("osoba2", imie="Artur", wiek="dorosly")

setClass("dwieOsoby", representation(pierwsza="osoba", druga="osoba"))



