require(tm)
require(wordcloud)
require(Rstem)

files <- file.path("./temp", system("ls ./temp | grep pdf", intern=TRUE))

read.group <- function(files){
  # set path to pdftotxt application and convert pdf to text
  exe <- "/usr/bin/pdf2txt"
  for(i in files) {
  system(paste("", exe, "", i, ">", sub(".pdf", ".txt", i), sep = " "), wait = F)
  # get txt-file name and open it  
  filetxt <- c(sub(".pdf", ".txt", files))
  }
  return(filetxt)
}

filetxt <- read.group(files)

txt <- lapply(filetxt, readLines) # don't mind warning..

txt <- tolower(txt)
txt <- removeWords(txt, c("\\f", stopwords()))

corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, removePunctuation)
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

# Stem words
d$stem <- wordStem(row.names(d), language = "english")

# and put words to column, otherwise they would be lost when aggregating
d$word <- row.names(d)

# remove web address (very long string):
d <- d[nchar(row.names(d)) < 20, ]

# aggregate freqeuncy by word stem and
# keep first words..
agg_freq <- aggregate(freq ~ stem, data = d, sum)
agg_word <- aggregate(word ~ stem, dasta = d, function(x) x[1])

d <- cbind(freq = agg_freq[, 2], agg_word)

# sort by frequency
d <- d[order(d$freq, decreasing = T), ]

# print wordcloud:
wordcloud(d$word, d$freq)

# remove files
# file.remove(dir(tempdir(), full.name=T)) # remove files