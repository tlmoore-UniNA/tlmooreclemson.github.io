library(shiny)

# Load citation data -----
dat_scopus_url <- "https://raw.githubusercontent.com/tlmooreclemson/tlmooreclemson.github.io/main/pages/pub_shiny/scopus_citationOverview.csv"
scopus <- read.csv(dat_scopus_url, head=TRUE, skip=4)


# Organize data frame
names(scopus)[1:7] <- scopus[2, 1:7] # re-name columns based on second row
names(scopus)[8:length(scopus)] <- scopus[1,8:length(scopus)] # re-name columns based on the "years" (1st) row
scopus <- scopus[3:nrow(scopus),]

library(dplyr) # Load the dplyr package
scopus <- scopus %>% select(-contains(c("<", ">", "total")))

# Create df with citations by year
library(reshape) # Load the reshape package
df <- melt(scopus)
names(df)[names(df) == "variable"] <- "citation_year"
names(df)[names(df) == "value"] <- "no_citations"

# Summary of total citations by year
df_cite = df %>% # Summarize by year
  group_by(citation_year) %>%
  summarise(
    citations = sum(no_citations)
  )

df_cite <- df_cite %>%
  mutate(cumulative_cites = cumsum(citations))

df_cite$citation_year = as.numeric(as.character(df_cite$citation_year))

# Data frame of number of publications per year
df_pubs <- data.frame("PublicationYear" = scopus[, 1],
                      "type" = scopus[,8])
df_pubs = df_pubs %>%
  group_by(PublicationYear, type) %>%
  count(PublicationYear)
df_pubs$PublicationYear = as.numeric(df_pubs$PublicationYear)


# Load Scopus bibliometric data via web scraping
library(rvest)

# Navigate to Scopus page
scopus_url <- 'https://www.scopus.com/authid/detail.uri?authorId=36542652400'
scopus_web <- read_html(scopus_url)

# Get h-index
h_index <- scopus_web %>% html_node(".row3 .valueColumn span")%>% html_text()

# Shiny app ==========================================================
ui <- fluidPage(
	## App title -----
	titlePanel("Publications"),


