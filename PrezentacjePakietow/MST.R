# instalacja i ładowanie pakietu:
install.packages("MST")
library(MST)

# wczytujemy dane znajdujące się w pakiecie, które posłużą do ćwiczeń
data("Teeth", package = "MST")

head(Teeth)
# zbiór zawiera 51 zmiennych, informację o rodzaju zęba, id pacjenta, numer zęba
# wskaźnik zdarzenia oraz czas obserwacji
# analizę przeprowadzimy dla zębów trzonowych
molar <- subset(Teeth, molar == 1)

# na tak dużym zbiorze obliczenia trwają długo, w związku z tym okroimy zbiór danych
molar <- molar[molar$id %in% 1:300,]

nPatients <- length(unique(molar$id))

# ponieważ mamy dość duży zbiór danych, przy tworzeniu drzewa będziemy używać
# selection.method = "test.sample"
# zate tworzymy zbiór treningowy i testowy, służący do oceny dopasowania każdego z 
# drzew
set.seed(482046)
id.train <- sample(x = unique(molar$id), size = floor(nPatients * 2/3),
                      replace = FALSE)
molar_training <- molar[molar$id %in% id.train, ]
molar_test <- molar[!(molar$id %in% id.train), ]

# tworzymy formułę modelu
form <- as.formula(paste0("Surv(time, event) ~ ",
                           paste0("x", 1:51, collapse = " + "), " | id"))

# dopasowujemy drzewo przeżycia do danych
fit_molar <- MST(form, data = molar_training, test = molar_test,
                     method = "marginal", selection.method = "test.sample",
                     distinct = FALSE, delta = 0.05, nCutPoints = 10, minevents = 5,
                     minbucket = 20)

# wyświetla się wykres dopasowania drzewa w zależności od jego rozmiaru
# funkcję kryterialna Ga chcemy maksymalizować
# 2, 3, 4, ln(n) oznacza jak duża kara za rozmiar drzewa jest uwzględniona w funkcji

# dopasowane drzewo początkowe
fit_molar$tree0
plot(fit_molar$tree0)

# informacje o dopasowaniu drzew przycinanych
fit_molar$pruning.info

# informacja o rozmiarze najlepiej dopasowanych drzew z uwzglednieniem różnych kar
fit_molar$best.tree.size

# informacja o najlepiej dopasowanych drzewach z uwzglednieniem różnych kar
fit_molar$best.tree.structure


# wykresy najlepszych drzew dla różnych kar
plot(getTree(fit_molar, "0"))   #czyli drzewo początkowe
plot(getTree(fit_molar, "2"))
plot(getTree(fit_molar, "3"))
plot(getTree(fit_molar, "4"))
plot(getTree(fit_molar, "log_n"))   #sam korzeń

# wybieramy drzewo
tree_final <- getTree(fit_molar, "2")

# przykład dopasowania modelu przed predykcją

molar$term_nodes <- as.factor(predict(tree_final, newdata = molar, type = 'node'))
new_model <- coxph(Surv(time, event) ~ term_nodes + cluster(id), data = molar)

