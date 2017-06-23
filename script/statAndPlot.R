#
#
#
library(tm)
library(qdap)
library(wordcloud)
source("preProcessData.R")

getBagOfWord <- function() {
    data <- getPreprocessedData()
    corpus <- Corpus(VectorSource(data$Body))
    # corpus <- llply(corpus, bag_o_words)
    # get frequency of word in docs
    bow <- llply(corpus, word_list)
    # data <- cbind(data, as.list(bow))
    return (data)
}

simularity <- function(data, dtm) {
    cos.sim <- function(ix) {
        A = dtm[ix[1],]
        B = dtm[ix[2],]
        return ( sum(A*B) / sqrt(sum(A^2) * sum(B^2)) )
    }
    n <- nrow(dtm)
    cmb <- expand.grid(i=1:n, j=1:n)
    C <- matrix(apply(cmb, 1, cos.sim), n, n)
    image(C)
}

countAndDraw <- function() {
    data <- getBagOfWord()
    corpus <- VCorpus(VectorSource(data$Body))
    # tdm <- TermDocumentMatrix(corpus)
    dtm <- DocumentTermMatrix(corpus)
    word_freq <- colSums(as.matrix(dtm))
    simularity(data, dtm)

    # find words which occur over 100 times
    words_over_100 <- findFreqTerms(dtm, 100)

    # find 100 most words
    ord <- order(word_freq, decreasing=TRUE)
    words_head_100 <- word_freq[ord[1:100]]

    # draw word cloud of first 100 words
    wordcloud(names(words_head_100), words_head_100, colors=brewer.pal(6, 'Dark2'))
}