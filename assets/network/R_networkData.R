# Set working directory -----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  # Possible solution to data parsing problem:
  # https://stackoverflow.com/questions/24905042/how-to-make-a-network-of-user-in-a-dataframe
  # UPDATE data from: https://www.scopus.com/cto2/main.uri?origin=AuthorProfile&stateKey=CTOF_1316187325&groupedAuthor=false

# Libraries needed -----
library(dplyr)
library(GGally)
library(geomnet)
library(ggnetwork) # may need to install gcc-fortran on linux dist.
#library(igraph)

# Thomas -----
## Loading data & shaping data -----
df <- read.csv("scopusNetwork.csv", header = TRUE)
df2 <- df %>%
  left_join(df, by="manuscript") %>%
  filter(!authors.x == authors.y) %>%
  select(primary_author = authors.x,
         connection = authors.y,
         primary_affiliation = affiliation.x,
         manuscript)

vertices <- df2[!duplicated(df2[c('primary_author', 
                                  'primary_affiliation')]),]
vertices <- vertices %>%
  select('primary_author', 'primary_affiliation')
edges <- df2 %>%
  select('primary_author', 'connection')

MMnet <- fortify(as.edgedf(edges), vertices)

## Plot network -----
set.seed(1776)
p <- ggplot(data = MMnet, aes(from_id = from_id, to_id = to_id)) +
  geom_net(aes(colour = primary_affiliation), layout.alg = "kamadakawai", # fruchtermanreingold, 
           # 
           size = 4, labelon = FALSE, vjust = -0.6, ecolour = "black",
           directed =FALSE, fontsize = 3, ealpha = 0.05) +
  scale_colour_manual(values = c("#F9B413", # AMI
                                 "#F66733", # Clemson
                                 "#42155E", # East Carolina University
                                 "#80BD26", # ETH
                                 "#D23264", # Helmholtz Dresden-Rossendrof
                                 "#0077B3", # IIT
                                 "#FFFFFF", # Me
                                 "#03447B", # MUSC
                                 "#E01A31", # Nanyang
                                 "#0F2C53", # Milano
                                 "#F25757", # Tel-Aviv
                                 "#0B7C2A", # TKI
                                 "#A40047", # Barcelona
                                 "#000000", # UniFr
                                 "#094183", # UniMelbourne
                                 "#73000A", # Ugly as fuck USC 'garnet'
                                 "#987E3E", # Vanderbilt
                                 "#0564A5"  # ZHAW
                                 )) +
  xlim(c(-0.05, 1.05)) +
  theme_net() +
  theme(legend.position = "none", 
        #legend.position = "bottom",
        legend.title = element_blank()
        )
p# + guides(colour = guide_legend(ncol=2, byrow = TRUE))
ggsave("plot_collabNetwork_noLegend.pdf",
       #"plot_collabNetwork_Legend.pdf",
       device = "pdf", 
       plot = last_plot(), 
       units = c("cm"), 
       dpi = 600, 
       width = 16,
       height = 16)




# Gabriella -----
rm(list = ls())
## Load Gabri data -----
df <- read.csv("gabri_network.csv", header = TRUE)
df2 <- df %>%
  left_join(df, by="manuscript") %>%
  filter(!authors.x == authors.y) %>%
  select(primary_author = authors.x,
         connection = authors.y,
         primary_affiliation = affiliation.x,
         manuscript)

vertices <- df2[!duplicated(df2[c('primary_author', 
                                  'primary_affiliation')]),]
vertices <- vertices %>%
  select('primary_author', 'primary_affiliation')
edges <- df2 %>%
  select('primary_author', 'connection')

MMnet <- fortify(as.edgedf(edges), vertices)

# GabiBabi Network Plot
p <- ggplot(data = MMnet, aes(from_id = from_id, to_id = to_id)) +
  geom_net(aes(colour = primary_affiliation), layout.alg = "kamadakawai",
           size = 2, labelon = FALSE, vjust = -0.6, ecolour = "black",
           directed =FALSE, fontsize = 3, ealpha = 0.05) +
  scale_colour_manual(values = c("#F9B413", # AMI
                                 "#75B9F3", # Children's Hospital Bambino Gesù
                                 "#85CA26", # DuPont Health & Nutrition
                                 "#132577", # ESRF-The European Synchrotron
                                 "#80BD26", # ETH Zürich
                                 "#FFFFFF", # GabiBabi
                                 "#8EB73C", # Gattefossé SAS
                                 "#1E64CB", # Ghent University
                                 "#0075A9", # Institut Laue-Langevin
                                 "#0077B3", # IIT
                                 "#009341", # LMU
                                 "#006DAE", # Monash University
                                 "#102951", # NRC Italy
                                 "#00A5CB", # PharmaSolv Consulting
                                 "#FF0000", # China Research center
                                 "#032D93", # Russian Academy of Sciences
                                 "#812432", # Sapienza University Roma
                                 "#FED106", # SD State U
                                 "#000000", # Tel Aviv University
                                 "#009ED0", # University Bordeaux
                                 "#B70718", # University Cagliari
                                 "#809CBC", # Uni Campania Luigi V.
                                 "#F29100", # U Innsbruck
                                 "#1F8389", # U Lille
                                 "#023367", # Uni Milano
                                 "#2A68AF", # U Nairobi
                                 "#0E7492", # Uni Napoli Federico II
                                 "#2A140F", # Nottingham
                                 "#B2274b", # Pavia
                                 "#002AC5", # Toronto
                                 "#00508C", # Urbino
                                 "#094DA6", # Uni Roma Tre
                                 "#FDBD10", # VCU
                                 "#0C5248" # Wayne State University
  ))+
  xlim(c(-0.05, 1.05)) +
  theme_net() +
  theme(legend.position = "none",
        #legend.position = "bottom",
        legend.title = element_blank())
p# + guides(colour = guide_legend(nrow = 5, byrow = TRUE))


## Save file ----- 
ggsave("plot_gabiNetwork.png",
       device = "png", 
       plot = last_plot(), 
       units = c("cm"), 
       dpi = 600, 
       width = 14,
       height = 14)
