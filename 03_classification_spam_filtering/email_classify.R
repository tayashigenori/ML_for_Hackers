library(tm)
library(ggplot2)

get.msg <- function(path) {
    con <- file(path, open="rt", encoding="latin1")
    text <- readLines(con)
    # The message always begins after the first full line break
    msg <- text[seq(which(text=="")[1]+1, length(text), 1)]
    close(con)
    return(paste(msg, collapse="\n"))
}
get.tdm <- function(doc.vec) {
    doc.corpus <- Corpus(VectorSource(doc.vec))
    control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, minDocFreq=2)
    doc.dtm <- TermDocumentMatrix(doc.corpus, control)
    return(doc.dtm)
}

spam.path <- "data/spam/"
spam2.path <- "data/spam_2/"
easyham.path <- "data/easy_ham/"
easyham2.path <- "data/easy_ham_2/"
hardham.path <- "data/hard_ham/"
hardham2.path <- "data/hard_ham_2/"

spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs,
                   function(p) get.msg(file.path(spam.path, p)))

spam.tdm <- get.tdm(all.spam)

spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts),
    as.numeric(spam.counts)), stringAsFactors=FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

spam.occurrence <- sapply(1:nrow(spam.matrix),
    function(i) {length(which(spam.matrix[i,] > 0)) / ncol(spam.matrix)})
spam.density <- spam.df$frequency / sum(spam.df$frequency)

spam.df <- transform(spam.df, density=spam.density,
    occurence=spam.occurrence)

head(spam.df[with(spam.df, order(-occurence)),])

