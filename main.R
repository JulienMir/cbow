source("create_data.R")
source("sgd.R")

library(text2vec)

# Importation des données
corpus <- import_data("text9")

# Pour les tests
N <- 1e4

# Largeur de la fenêtre contexte
l <- 3
dat <- create_data(corpus[1:N], l)

# dimension des représentations
p <- 3
# nombre d'itérations de la SGD
it <- 2

U <- sgd_CBOW(dat$D, dat$vocab, p, it)
