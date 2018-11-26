raw <- " Remember, remember!
    The fifth of November,
The Gunpowder treason and plot;
I know of no reason
Why the Gunpowder treason
Should ever be forgot!
Guy Fawkes and his companions
Did the scheme contrive,
To blow the King and Parliament
All up alive.
Threescore barrels, laid below,
To prove old England's overthrow.
But, by God's providence, him they catch,
With a dark lantern, lighting a match!
A stick and a stake
For King James's sake!
    If you won't give me one,
I'll take two,
    The better for me,
    And the worse for you.
    A rope, a rope, to hang the Pope,
    A penn'orth of cheese to choke him,
A pint of beer to wash it down,
And a jolly good fire to burn him.
Holloa, boys! holloa, boys! make the bells ring!
Holloa, boys! holloa boys! God save the King!
Hip, hip, hooor-r-r-ray!"

corpus <- tm::MC_tokenizer(raw)

dict <- unique(corpus)

# Largeur de la fenêtre contexte
l <- 3

# Rajout de mot vide au début et à la fin
corpus <- c(rep("<>NULL<>", l),
            corpus,
            rep("<>NULL<>", l))

D <- matrix(NA, nrow = length(corpus)-2*l, ncol = 1+2*l)
colnames(D) <- c("target", paste0("context_", 1:(2*l)))

# Construction du jeu de données D
for(w in 1:(length(corpus)-2*l)){
  D[w,] <- c(corpus[w+l], corpus[w+l-(1:l)], corpus[w+l+(1:l)])
}

# Transformation des mots en indices
for(ligne in 1:nrow(D)){
  for(col in 1:ncol(D)){
    D[ligne, col] <- which(D[ligne, col] == dict)
  }
}
D <- apply(D, 2, as.numeric)
