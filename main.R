source("create_data.R")
source("sgd.R")

# Largeur de la fenêtre contexte
l <- 3
dat <- create_data("text9", l)

# dimension des représentations
p <- 10
# nombre d'itérations de la SGD
it <- 5

U <- sgd_CBOW(dat$D, dat$vocab, p, it)
