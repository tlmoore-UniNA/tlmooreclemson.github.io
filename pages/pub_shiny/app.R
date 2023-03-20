library(shiny)
library(scholar)
library(dplyr)
library(ggplot2)

# For development
# options(browser='brave')

scholar_id <- 'l5fRQdoAAAAJ'
auth <- get_profile(scholar_id)
h_index <- auth$h_index

pubs <- get_publications(id='l5fRQdoAAAAJ')
# Drop known 'non-publication publications'
pubs <- pubs[which(!pubs$pubid == 'qUcmZB5y_30C'),]
pubs <- pubs[which(!pubs$pubid == 'IjCSPb-OGe4C'),]
pubs <- pubs[which(!pubs$pubid == 'Se3iqnhoufwC'),]
pubs <- pubs[which(!pubs$pubid == 'roLk4NBRz8UC'),]
# Drop book chapters etc.
pubs <- pubs[which(!pubs$pubid == 'QIV2ME_5wuYC'),]
pubs <- pubs[which(!pubs$pubid == '3fE2CSJIrl8C'),]

num_pubs <- nrow(pubs)
tot_cites <- sum(pubs$cites)

# Get number of publications per year
pubYears <- data.frame("year" = pubs$year)
pubYears <- pubYears %>% group_by(year) %>%
		count(year)
pubYears$year = as.numeric(pubYears$year)

# Get citations per year
citesYear <- get_citation_history(scholar_id)
citesYear$cumCites <- cumsum(citesYear$cites) # cumulative citations

# Data frame for publication summary
df <- data.frame("num_pubs" = num_pubs,
				 "h_index" = as.numeric(h_index),
				 "tot_cites" = as.numeric(tot_cites))
df$h_index <- round(df$h_index, digits = 0)
df$tot_cites <- round(df$tot_cites, digits=0)

names(df) = c("Total no. publications", "h-index", "Total no. citations")

ui <- fluidPage(
	## Publication summary -----
	titlePanel("Article Publication Summary"),
	mainPanel(
		# Output table as first result
		tableOutput('table'),
		fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
					plotOutput(outputId = 'plotYears'),
					plotOutput(outputId = 'citeYears')	
					)
                )
	)
)
# Define server logic
server <- function(input, output){
	output$table <- renderTable({
			df
	})
	output$plotYears <- renderPlot({
		p_years <- ggplot(pubYears, aes(year, n))+
				geom_col(colour="black",fill="#3399CC")+
				scale_x_continuous(name = "Year", 
								   limits = c(2010, max(pubYears$year)))+
				scale_y_continuous(name = "No. publications")+
				theme(panel.grid = element_blank(),
					  panel.background = element_rect(colour="#595959", fill = NA),
					  aspect.ratio=1)
		p_years
	})
	output$citeYears <- renderPlot({
		p_cites <- ggplot(citesYear, aes(year, cumCites))+
				geom_line(colour = "#3399CC")+
				geom_point(shape = 21, fill = "#3399CC", colour = "#595959")+
				scale_x_continuous(name = "Year",
								   limits = c(2010, max(citesYear$year)))+
				scale_y_continuous(name = "Total no. citations")+
				theme(panel.grid = element_blank(),
                      panel.background = element_rect(colour="#595959", fill = NA),
                      aspect.ratio=1)
		p_cites
	})
}

# Run the app -----
shinyApp(ui = ui, server = server)
# App located at: 
#
# To update, run:
# library(rsconnect)
# rsconnect::deployApp("<path/to/app/directory>", appName="publication_summary")
