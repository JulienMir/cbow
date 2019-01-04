setwd("D:/Repo_git/cbow")

library(Rcpp)
library(tm)
library(text2vec)
library(tictoc)

######## PRÉPARATION DU CORPUS ########
source("create_data.R")

# Importation des données
corpus <- import_data("text8")

# Suppression des stopwords
stopwords <- stopwords(kind = "en")
print(length(corpus))
corpus <- corpus[!(corpus %in% stopwords)]
print(length(corpus))

# Suppression des mots trop rares
wordcount <- table(corpus)
min <- 50
rare <- names(wordcount[wordcount < min])
corpus <- corpus[!(corpus %in% rare)]
print(length(corpus))

# Dictionnaire
dict <- unique(corpus)
print(length(dict))

# Ecriture du dictionnaire dans un .txt (pour le sauvegarder)
fileConn <- file("dict8.txt")
writeLines(dict, fileConn)
close(fileConn)

# Ecriture du corpus dans un .txt (pour le sauvegarder)
fileConn <- file("corpus8.txt")
writeLines(corpus, fileConn)
close(fileConn)

# Lecture du corpus et du dictionnaire
dict <- readLines("dict8.txt")
corpus <- readLines("corpus8.txt")

# Transformation du corpus en index
index <- which_rcpp(corpus, dict)
write.csv2(index, "corpus8_indexe.csv", row.names = F)

######## CONSTRUCTION DU JEU DE DONNÉE D ########

# Importation du corpus indexé
corpus <- read.csv2("corpus8_indexe.csv", header = T)[,1]

# Rayon de la fenêtre contexte
l <- 5

# Rajout de mots vides au début et à la fin
corpus <- c(rep(0, l),
            corpus,
            rep(0, l))

# Initialisation de D
D <- matrix(NA, nrow = length(corpus)-2*l, ncol = 1+2*l)
colnames(D) <- c("target", paste0("context_", 1:(2*l)))

# Construction de D
for(w in 1:(length(corpus)-2*l)){
  D[w,] <- c(corpus[w+l], corpus[w+l-(1:l)], corpus[w+l+(1:l)])
}

# Sauvegarde de D
write.table(D, file = "D8.txt")

######## CBOW ########
source("sgd.R")

# Lecture de D
D <- as.matrix(read.table("D8.txt", header = TRUE, row.names = 1))

# importation du dictionnaire
vocab <- readLines("dict8.txt")

# CBOW
tic()
U <- sgd_CBOW(D[1:1e4,], vocab, p = 25, n_iter = 1, eta = 0.025)
toc()

306.5*nrow(D)/1e4/3600
