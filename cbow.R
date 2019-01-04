setwd("D:/Repo_git/cbow")

######## PRÉPARATION DU CORPUS ########
library(Rcpp)
library(tm)
library(text2vec)

source("create_data.R")

# Importation des données
corpus <- import_data("text9")

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
fileConn <- file("dict.txt")
writeLines(dict, fileConn)
close(fileConn)

# Ecriture du corpus dans un .txt (pour le sauvegarder)
fileConn <- file("corpus.txt")
writeLines(corpus, fileConn)
close(fileConn)

# Lecture du corpus et du dictionnaire
dict <- readLines("dict.txt")
corpus <- readLines("corpus.txt")

# Taille du corpus
N <- length(corpus)

# Transformation du corpus en index : en 4 fois pour éviter les problèmes de mémoire
index <- which_rcpp(corpus[1:2e7], dict)
write.csv2(index, "corpus_indexe1.csv", row.names = F)
rm(index)


index <- which_rcpp(corpus[(2e7+1):4e7], dict)
write.csv2(index, "corpus_indexe2.csv", row.names = F)
rm(index)


index <- which_rcpp(corpus[(4e7+1):6e7], dict)
write.csv2(index, "corpus_indexe3.csv", row.names = F)
rm(index)


index <- which_rcpp(corpus[(6e7+1):N], dict)
write.csv2(index, "corpus_indexe4.csv", row.names = F)
rm(index)

######## CONSTRUCTION DU JEU DE DONNÉE D ########

# Importation du corpus indexé
corpus <- read.csv2("corpus_indexe1.csv", header = T)[,1]
corpus <- c(corpus, read.csv2("corpus_indexe2.csv", header = T)[,1])
corpus <- c(corpus, read.csv2("corpus_indexe3.csv", header = T)[,1])
corpus <- c(corpus, read.csv2("corpus_indexe4.csv", header = T)[,1])

corpus <- read.csv2("corpus_indexe4.csv", header = T)[,1]

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
write.table(D, file = "D4.txt")

######## CBOW ########
source("sgd.R")

# Lecture de D
D <- as.matrix(read.table("D1.txt", header = TRUE, row.names = 1))
# D <- as.matrix(read.table("D2.txt", header = TRUE, row.names = 1))
# D <- as.matrix(read.table("D3.txt", header = TRUE, row.names = 1))
# D <- as.matrix(read.table("D4.txt", header = TRUE, row.names = 1))

# importation du dictionnaire
vocab <- readLines("dict.txt")


# CBOW
tic()
U <- sgd_CBOW(D[1:20,], vocab, p = 50, n_iter = 5, eta = 0.025)
e20 <- toc()