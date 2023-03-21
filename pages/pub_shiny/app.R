# Load packages -----
library(shiny)
library(scholar)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(viridis)
library(geomnet)

# For development
# options(browser='brave')

# Pull and parse data from Google Scholar -----
scholar_id <- 'l5fRQdoAAAAJ'
auth <- get_profile(scholar_id)
h_index <- auth$h_index

pubs <- get_publications(id=scholar_id)

# Drop known 'non-publication publications'
pubs <- pubs[which(!pubs$pubid == 'qUcmZB5y_30C'),]
pubs <- pubs[which(!pubs$pubid == 'IjCSPb-OGe4C'),]
pubs <- pubs[which(!pubs$pubid == 'Se3iqnhoufwC'),]
pubs <- pubs[which(!pubs$pubid == 'roLk4NBRz8UC'),]

# Drop book chapters etc.
pubs <- pubs[which(!pubs$pubid == 'QIV2ME_5wuYC'),]
pubs <- pubs[which(!pubs$pubid == '3fE2CSJIrl8C'),]

# Get number of publications and total citations
num_pubs <- nrow(pubs)
tot_cites <- sum(pubs$cites)

# Get number of publications per year
pubYears <- data.frame("year" = pubs$year)
pubYears <- pubYears %>% group_by(year) %>%
		count(year)
pubYears$year = as.numeric(pubYears$year)

# Get citations per year
citesYear <- get_citation_history(scholar_id)
citesYear$cumulativeCites <- cumsum(citesYear$cites) # cumulative citations

# Data frame for publication summary
df <- data.frame("num_pubs" = num_pubs,
				 "h_index" = as.numeric(h_index),
				 "tot_cites" = as.numeric(tot_cites))
df$h_index <- round(df$h_index, digits = 0)
df$tot_cites <- round(df$tot_cites, digits=0)
# Rename columns
names(df) = c("Total no. publications", "h-index", "Total no. citations")


# Generate a blank data frame to get complete list of authors
auth_net = data.frame("Authors" = NA)

# Loop through publication list and get extended data
for (i in pubs$pubid){ # Append the complete list of authors for each publication
  auth_net[nrow(auth_net)+1,] = get_publication_data_extended(id=scholar_id, pub_id = i)$Authors
}
rm(i) # Remove the i variable created
auth_net <- na.omit(auth_net) # Remove the initial blank row
auth_net <- cbind(auth_net, pubs[,c("title", "journal", "pubid")]) # Flesh out the extended authors data frame

# Split authors separated by commas into individual rows
auth_net <- auth_net %>%
  mutate(Authors = strsplit(as.character(Authors), ",")) %>%
  unnest(Authors)

auth_net$Authors <- trimws(auth_net$Authors, which=c("both")) # Remove leading/trailing spaces
auth_net$Authors <-  gsub('[[:punct:] ]+',' ',auth_net$Authors) # Remove punctuation

# Substitute aliases for name
auth_net$Authors[auth_net$Authors == "Thomas L Moore"] <- "Thomas Moore" 
auth_net$Authors[auth_net$Authors == "Thomas Lee Moore"] <- "Thomas Moore"
auth_net$Authors[auth_net$Authors == "Ana M Milosevic"] <- "Ana Milosevic"
auth_net$Authors[auth_net$Authors == "Apparao M Rao"] <- "Apparao Rao"
auth_net$Authors[auth_net$Authors == "Daniel C Colvin"] <- "Daniel C. Colvin"
auth_net$Authors[auth_net$Authors == "Erika K Jelen"] <- "Erika K. Jelen"
auth_net$Authors[auth_net$Authors == "Purnima Manghnani"] <- "Purnima N Manghnani"
auth_net$Authors[auth_net$Authors == "Alke Petri Fink"] <- "Alke Petri-Fink"
auth_net$Authors[auth_net$Authors == "Barbara Rothen Rutishauser"] <- "Barbara Rothen-Rutishauser"
auth_net$Authors[auth_net$Authors == "Dominic A Urban"] <- "Dominic Urban"
auth_net$Authors[auth_net$Authors == "S C J Loo"] <- "Joachim Loo"
auth_net$Authors[auth_net$Authors == "Rachel A Morrison"] <- "Rachel Morrison"
auth_net$Authors[auth_net$Authors == "Rama Podilakrishna"] <- "Ramakrishna Podila"

# Import the affilication list
affils <- read.csv("affiliationList.csv")
auth_net <- merge(auth_net, affils, by=c("Authors"), keep.all=TRUE) # merge with the affiliations

# Create a data frame for plotting the network
auth_net <- auth_net %>%
				left_join(auth_net, by = "title") %>%
				filter(!Authors.x == Authors.y) %>%
				select(primary_author = Authors.x,
							 connection = Authors.y, 
							 primary_affiliation = affiliation.x,
							 title)
vertices <- auth_net[!duplicated(auth_net[c('primary_author', 'primary_affiliation')]),]
vertices <- vertices %>% select('primary_author', 'primary_affiliation')
edges <- auth_net %>% select('primary_author', 'connection')
MMnet <- fortify(as.edgedf(edges), vertices)


# Begin shiny app -----
ui <- fluidPage(
	## Publication summary -----
	titlePanel("Article Publication Summary"),
	mainPanel(
		# Output table as first result
		tableOutput('table'),
		fluidRow(splitLayout(vcellWidths = c("33.3%", "33.3%", "33.4%"), 
												 plotlyOutput(outputId = 'plotYears'),
												 plotlyOutput(outputId = 'citeYears'),
												 plotOutput(outputId = 'network')
												)
						)
					)
)
# Define server logic
server <- function(input, output){
  output$table <- renderTable({
    df
    })
  output$plotYears <- renderPlotly({
    p_years <- ggplot(pubYears, aes(year, n))+
      geom_col(colour="#595959",fill="#3399CC")+
      scale_x_continuous(name = "Year")+
      scale_y_continuous(name = "No. publications")+
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            panel.grid = element_blank(),
            panel.background = element_rect(colour="#595959", fill = NA),
            aspect.ratio=1)
		p_years
	})
	output$citeYears <- renderPlotly({
	  p_cites <- ggplot(citesYear, aes(year, cumulativeCites))+
	    geom_line(colour = "#3399CC")+
	    geom_point(size=4, shape = 21,
	               fill = "#3399CC", colour = "#595959")+
	    scale_x_continuous(name = "Year")+
	    scale_y_continuous(name = "Total no. citations")+
	    theme(axis.title = element_text(size = 14),
	          axis.text = element_text(size = 12),
	          panel.grid = element_blank(),
	          panel.background = element_rect(colour="#595959", fill = NA),
	          aspect.ratio=1)
	  p_cites
	})
	output$network <- renderPlot({
	  p_network <- ggplot(data = MMnet, aes(from_id = from_id, to_id = to_id))+
	    geom_net(aes(colour = primary_affiliation), layout.alg="kamadakawai",
	             size=4, labelon=FALSE, vjust=-0.6, ecolour="black",
	             directed=FALSE, fontsize=3, ealpha=0.05)+
	    scale_colour_viridis_d(option="turbo")+
	    xlim(c(-0.05, 1.05))+
	    theme_net()+
	    theme(legend.position = "none")
	  p_network
	})
}

# Run the app -----
shinyApp(ui = ui, server = server)
# App located at: 
#
# To update, run:
# library(rsconnect)
# rsconnect::deployApp("<path/to/app/directory>", appName="publication_summary")
