bayer <- searchTwitter("bayer", n=1000, lang="en", resultType = "recent")
pfizer <- searchTwitter("pfizer", n=1000, lang="en", resultType = "recent")
Roche <- searchTwitter("roche", n=1000, lang="en", resultType = "recent")
ova <- searchTwitter("novartis", n=1000, lang="en", resultType = "recent")

bayer_txt = sapply(bayer, function(x) x$getText())
nova_txt = sapply(Nova, function(x) x$getText())
roche_txt = sapply(Roche, function(x) x$getText())
pfizer_txt = sapply(pfizer, function(x) x$getText())

source("score.R")
pos <- readLines("positive.txt")
neg <- readLines("negative.txt")

nd = c(length(bayer_txt), length(pfizer_txt), length(roche_txt),
       +        length(nova_txt))

company = c(bayer_txt, pfizer_txt, roche_txt, nova_txt) 
sc <- scores(company, pos, neg, .progress = "text")
sc$company <- factor(rep(c("Bayer","Pfizer", "Roche", "Novartis"), nd))

sc$verypos <- as.numeric(sc$score >= 2)
sc$veryneg <- as.numeric(sc$score <= -2)
numpos <- sum(sc$verypos)
numneg <- sum(sc$veryneg)
global_score = round( 100 * numpos / (numpos + numneg) )

library(lattice)
histogram(data=sc, ~score|company, main="Sentiment Analysis of 4
+ Companies", col=c("red", "grey"),xlab="", sub="Sentiment Score")

boxplot(score~company, data=sc )


