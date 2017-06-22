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
    llply(corpus, bag_o_words)
    data <- cbind(data, as.list(corpus))
    return (data)
}

countAndDraw <- function() {
    data <- getBagOfWord()
    corpus <- VCorpus(VectorSource(data$Body))
    # tdm <- TermDocumentMatrix(corpus)
    dtm <- DocumentTermMatrix(corpus)
    word_freq <- colSums(as.matrix(dtm))

    # find words which occur over 100 times
    words_over_100 <- findFreqTerms(dtm, 100)

    # find 100 most words
    ord <- order(word_freq, decreasing=TRUE)
    words_head_100 <- word_freq[ord[1:100]]

    # draw word cloud of first 100 words
    wordcloud(names(words_head_100), words_head_100, colors=brewer.pal(6, 'Dark2'))
}