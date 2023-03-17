# Set working directory
setwd("~/Dropbox/ProfPort/Personal_Webpage/")

# Libraries ###########
library(reshape)
#library(stringr)
#library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(ggsci)


# Import Data #############
scopus <- read.csv("scopus_citationOverview.csv", header = TRUE, skip = 4)

names(scopus)[1:7] <- scopus[2, 1:7] # re-name columns based on second row
names(scopus)[8:length(scopus)] <- scopus[1,8:length(scopus)] # re-name columns based on the "years" (1st) row 
scopus <- scopus[3:nrow(scopus),]
scopus <- scopus %>% select(-contains(c("<", ">", "total")))

# Create df with citations by year
df <- melt(scopus)
names(df)[names(df) == "variable"] <- "citation_year"
names(df)[names(df) == "value"] <- "no_citations"

df_cite = df %>% # Summarize by year
  group_by(citation_year) %>%
  summarise(
    citations = sum(no_citations)
  )

df_cite <- df_cite %>%
  mutate(cumulative_cites = cumsum(citations))

df_cite$citation_year = as.numeric(as.character(df_cite$citation_year))


### Plots #########
p_citeYear <- ggplot(df_cite, aes(citation_year, cumulative_cites))+
  geom_col(colour="black")+
  scale_x_continuous(name = "Year", breaks = pretty_breaks())+
  scale_y_continuous(name = "Cumulative no. Citations")+
  scale_fill_lancet()+
  theme(aspect.ratio = 1, panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill="grey97"),
        legend.position = "bottom")
p_citeYear# + labs(fill = "Article\nType")
ggsave("citations.pdf", device = "pdf",
       dpi = 300, width = 11, height = 12, units = c("cm"),
       plot = last_plot())


### Number of Publications ##########################
df_pubs <- data.frame("PublicationYear" = scopus[, 1],
                      "type" = scopus[,8])
df_pubs = df_pubs %>%
  group_by(PublicationYear, type) %>%
  count(PublicationYear)
df_pubs$PublicationYear = as.numeric(df_pubs$PublicationYear)


p_citePubs <- ggplot(df_pubs, aes(PublicationYear, n))+
  geom_col(colour="black")+
  scale_x_continuous(name = "Year", breaks = pretty_breaks())+
  scale_y_continuous(name = "No. Publications")+
  scale_fill_lancet()+
  theme(aspect.ratio = 1, panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill="grey97"),
        legend.position = "bottom")
p_citePubs# + labs(fill = "Article\nType")
ggsave("publications.pdf", device = "pdf",
       dpi = 300, width = 11, height = 12, units = c("cm"),
       plot = last_plot())
