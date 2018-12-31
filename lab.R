setwd("D:/Repo_git/cbow")
source("create_data.R")
source("sgd.R")

library(Rcpp)
library(parallel)
library(text2vec)
library(tm)
library(tictoc)

# Importation des données
corpus <- import_data("text9")

# Suppression des stopwords
stopwords <- stopwords(kind = "en")
print(length(corpus))
corpus <- corpus[!(corpus %in% stopwords)]
print(length(corpus))

# Suppression des mots trop rares
wordcount <- table(corpus)
min <- 5
rare <- names(wordcount[wordcount < min])
corpus <- corpus[!(corpus %in% rare)]

# Dictionnaire
dict <- unique(corpus)
print(length(dict))

# Ecriture du corpus et du dictionnaire
fileConn <- file("dict.txt")
writeLines(dict, fileConn)
close(fileConn)
fileConn <- file("corpus_indexe.txt")
writeLines(corpus, fileConn)
close(fileConn)

# Lecture du corpus et du dictionnaire
tic("lecture")
dict <- readLines("dict.txt")
corpus <- readLines("corpus.txt")
toc()
N <- length(corpus)

# Transformation du corpus en index
tic("transformation en index")
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
duree <- toc()


# Relecture
# a <- read.csv2("corpus_indexe4.csv", header = T)[,1]
