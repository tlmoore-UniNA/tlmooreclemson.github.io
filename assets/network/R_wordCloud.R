# Libraries ----
require(tm)
require(SnowballC)
require(wordcloud)
require(RColorBrewer)
require(pdftools)
library(stringr)
MINDEDpalette <- c('#004392', '#92D9DE', '#3AAFE7', '#55555B', '#1A7BEB', '#0677B1', '#7D2053')


# Pull from PDFs ----
#setwd("~/Dropbox/ProfPort/Proposals/2020/failed_2020_ERC_StartingGrant/2020_appPacket/R_figures/rawText/")
setwd("~/Dropbox/ProfPort/Publications/")
files = list.files(pattern = "pdf$")
txt <- lapply(files, pdf_text)
txt.Corpus <- Corpus(URISource(files),
                     readerControl = list(reader = readPDF))
setwd("~/Dropbox/ProfPort/Personal_Webpage/")

# Pull from the pdf2txt python tool
#txt <- readLines("merged.csv")
#txt <- data.frame(txt)
#txt.Corpus <- Corpus(VectorSource(txt$txt))

# Clean the Corpus -----
txt.Corpus <- tm_map(txt.Corpus, content_transformer(tolower))
txt.Corpus <- tm_map(txt.Corpus, PlainTextDocument)
txt.Clean <- tm_map(txt.Corpus, removeNumbers)
txt.Clean <- tm_map(txt.Clean, removeWords, stopwords("english"))
txt.Clean <- tm_map(txt.Clean,removePunctuation)
txt.Clean <- tm_map(txt.Clean,stripWhitespace)
txt.Clean <- tm_map(txt.Clean,stemDocument)
txt.Clean <- tm_map(txt.Clean, removeWords, c("can", "use", "studi",
                                              "show", "figur", "chem",
                                              "also"))
remove(txt, txt.Corpus, files)

# Wordcloud construction
dtm <- TermDocumentMatrix(txt.Clean)
matrix <- as.matrix(dtm)
sorted <- sort(rowSums(matrix), decreasing = TRUE)
data <- data.frame(word = names(sorted), freq=sorted)
remove(matrix, sorted, txt.Clean, dtm)
data$word <- str_replace(data$word, "cultur", "culture")
data$word <- str_replace(data$word, "particl", "particle")
data$word <- str_replace(data$word, "surfac", "surface")
data$word <- str_replace(data$word, "imag", "image")
data$word <- str_replace(data$word, "releas", "release")
data$word <- str_replace(data$word, "deliveri", "delivery")
data$word <- str_replace(data$word, "measur", "measure")
data$word <- str_replace(data$word, "uptak", "uptake")
data$word <- str_replace(data$word, "tissu", "tissue")
data$word <- str_replace(data$word, "increas", "increase")
data$word <- str_replace(data$word, "materi", "material")
data$word <- str_replace(data$word, "compar", "compare")
data$word <- str_replace(data$word, "polym", "polymer")
data$word <- str_replace(data$word, "howev", "however")
data$word <- str_replace(data$word, "properti", "properties")
data$word <- str_replace(data$word, "membran", "membrane")
data$word <- str_replace(data$word, "sampl", "sample")
data$word <- str_replace(data$word, "vitro", "in vitro")
data$word <- str_replace(data$word, "vivo", "in vivo")
data$word <- str_replace(data$word, "stabil", "stability")
data$word <- str_replace(data$word, "aunp", "AuNP")
data$word <- str_replace(data$word, "dls", "DLS")
data$word <- str_replace(data$word, "cnt", "CNT")
data$word <- str_replace(data$word, "observ", "observe")
data$word <- str_replace(data$word, "viabil", "viability")
data$word <- str_replace(data$word, "concentr", "concentration")
data$word <- str_replace(data$word, "nanotechnol", "nanotechnology")
data$word <- str_replace(data$word, "hydroxyapatit", "hydroxyapatite")
data$word <- str_replace(data$word, "hydrodynam", "hydrodynamic")
data$word <- str_replace(data$word, "pga", "PGA")
data$word <- str_replace(data$word, '“diameter”', "diameter")
data$word <- str_replace(data$word, "chemic", "chemical")
data$word <- str_replace(data$word, "synthesi", "synthesis")
data$word <- str_replace(data$word, "determin", "determine")
data$word <- str_replace(data$word, "fluoresc", "fluorescent")
data$word <- str_replace(data$word, "adsorpt", "adsorption")
data$word <- str_replace(data$word, "adsorpt", "adsorption")
data$word <- str_replace(data$word, "techniqu", "technique")
data$word <- str_replace(data$word, "temperatur", "temperature")
data$word <- str_replace(data$word, "structur", "structure")
data$word <- str_replace(data$word, "peg", "PEG")
data$word <- str_replace(data$word, "xray", "X-ray")
data$word <- str_replace(data$word, "charg", "charge")
data$word <- str_replace(data$word, 'oxid', 'oxide')
data$word <- str_replace(data$word, 'activ', 'active')
data <- data[!grepl("liu", data$word),]
data <- data[!grepl("addit", data$word),]
data <- data[!grepl("nps", data$word),]
data <- data[!grepl("however", data$word),]
data <- data[!grepl("chemistri", data$word),]
data <- data[!grepl("adv", data$word),]
data <- data[!grepl("wang", data$word),]
data <- data[!grepl("polymerer", data$word),]
data <- data[!grepl("kim", data$word),]
data <- data[!grepl("nha", data$word),]
data <- data[!grepl("fig", data$word),]
data <- data[!grepl("chen", data$word),]
data <- data[!grepl("chang", data$word),]
data <- data[!grepl("zhang", data$word),]
data <- data[!grepl("wileyvch", data$word),]
data <- data[!grepl("weinheim", data$word),]
data <- data[!grepl("kgaa", data$word),]
data <- data[!grepl("med", data$word),]
data <- data[!grepl("verlag", data$word),]
rownames(data) <- seq(length=nrow(data))


# Plot -----
set.seed(1776)
pdf('wordCloud.pdf')
wordcloud(words = data$word, freq = data$freq,
          min.freq=35, max.words=120, random.order=FALSE, rot.per=.15,
          colors = MINDEDpalette)
dev.off()
system("pdfcrop --margins '5 5 5 5' wordCloud.pdf wordCloud.pdf")


# To crop the word cloud in a Unix system
# > Open a new terminal
# > Go to the correct directory:
# >> $ cd OneDrive/Proposals/2020_ERC_StartingGrant/2020_appPacket/R_figures
# > Crop the figure using a bash command:
# >> $ pdfcrop wordCloud.pdf wordCloud_cropped.pdf

# To conver the pdf to a png in a Unix system
# > While still in the correct directory, use pdftoppm
# >> $ pdftoppm wordCloud_cropped.pdf wordCloud_cropped -png