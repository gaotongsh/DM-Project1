###########################################
# DM Proj1                                #
# 1.1 1.2 Preprocess Data into Data.frame #
###########################################
library(XML)
library(plyr)
library(tm)

# 1.1 Construct data.frame
getDataFrame <- function() {
    curpath <- getwd()
    docpath <- "../nyt_corpus/samples_500"
    setwd(docpath)
    files <- list.files()
    Data <- ldply(files, parse_xml)
    setwd(curpath)
    return(Data)
}

parse_xml <- function(FileName) {
    doc1 <- xmlParse(FileName)

    # (1) Find Full_text
    textnodes <- getNodeSet(doc1,"//block[@class='full_text']/p")
    # Excluding the Leading paragraph
    if (!is.na(xmlValue(textnodes[[1]])) & substr(xmlValue(textnodes[[1]]), 1, 4) == "LEAD") {
        textnodes <- textnodes[-1]
    }
    text <- paste(llply(textnodes, xmlValue), collapse="")

    # (2) Find Date
    yearnode <- getNodeSet(doc1,"//meta[@name='publication_year']")
    year <- xmlGetAttr(yearnode[[1]], name="content")
    monthnode <- getNodeSet(doc1,"//meta[@name='publication_month']")
    month <- xmlGetAttr(monthnode[[1]], name="content")
    daynode <- getNodeSet(doc1,"//meta[@name='publication_day_of_month']")
    day <- xmlGetAttr(daynode[[1]], name="content")
    date <- as.Date(paste(year, month, day, sep="/"))

    # (3) Find Class
    classnodes <- getNodeSet(doc1,"//classifier[@type='taxonomic_classifier']")
    classes <- llply(classnodes, xmlValue)
    # Get top feature
    classes <- unlist(classes)[c(grep("Top/Features/", classes),
                                 grep("Top/News/", classes))]
    classes <- llply(classes, strsplit, split="/")
    # Choose the third element and unique it
    classes <- list(unique(unlist(llply(classes, function(l) l[[1]][3]))))
    if(is.null(classes[[1]])) { classes <- NA }

    doc <- data.frame(Filename=FileName, Body=text, Date=date)
    doc[["Classifier"]] <- classes
    doc[["Body"]] <- llply(doc[["Body"]], as.character)
    return(doc)
}

# 1.2 Preprocess with tm
getPreprocessedData <- function(data) {
    # To corpus
    corpus <- Corpus(VectorSource(data$Body))
    # Six Preprocess Items
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, stemDocument)
    data$Body <- as.list(corpus)
    return(data)
}
