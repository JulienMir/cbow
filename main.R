source("create_data.R")
source("sgd.R")


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

# Largeur de la fenêtre contexte
l <- 3
dat <- create_data(raw, l)

# dimension des représentations
p <- 10
# nombre d'itérations de la SGD
it <- 5

U <- sgd_CBOW(dat$D, dat$vocab, p, it)

vec50 <- read.csv("fr_50k.vec", quote = "", header = FALSE, sep = " ", encoding = "UTF-8")
words <- vec50[,1]
vectors <- as.matrix(vec50[,2:301])
