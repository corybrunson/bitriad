# Create graph objects for affiliation network data sets

# Setup
library(devtools)  # for install_github()
library(igraph)    # for functions
library(latentnet) # for Davis data
#install_github("corybrunson/scottish.capital")
library(scottish.capital)
# http://bioconductor.wustl.edu/data/experiment/html/SNAData.html
#source("http://bioconductor.org/biocLite.R")
#biocLite("SNAData", ask = FALSE)
library(SNAData)

# Davis Southern Women
data(davis)
women_group <- graph_from_incidence_matrix(davis)
V(women_group)$type <- !(substr(V(women_group)$name, 2, 2) %in% LETTERS)
# Fix the names
V(women_group)$name[which(V(women_group)$name == "MYRNA")] <- "MYRA"
V(women_group)$name[!V(women_group)$type] <- paste(
  substr(V(women_group)$name[V(women_group)$type == 0], 1, 1),
  tolower(substr(V(women_group)$name[V(women_group)$type == 0], 2,
                 nchar(V(women_group)$name[V(women_group)$type == 0]))),
  sep = "")
V(women_group)$name[V(women_group)$type] <- c(
  "6/27", "3/2", "4/12", "9/26", "2/25", "5/19", "3/15",
  "9/16", "4/8", "6/10", "2/23", "4/7", "11/21", "8/3"
)
# Assign times to be days of the single calendar year
V(women_group)$time[V(women_group)$type] <- as.numeric(
  as.Date(paste0("1933/", V(women_group)$name[V(women_group)$type])) -
    as.Date("1932/12/31")
)
# Put the events in chronological order
women_group <- permute(women_group, c(
  1:actor_count(women_group),
  actor_count(women_group) +
    order(order(V(women_group)[V(women_group)$type == TRUE]$time)))
)

# "Deep South" > "Social Cliques in Colored Society" > Fig. 11: Clique A
# Read data directly into bipartite incidence matrix
women_clique <- graph_from_incidence_matrix(as.matrix(read.csv(
  "DGG_Clique_A.csv",
  row.names = 1)))

# Scott and Hughes, *The Anatomy of Scottish Capital*
# https://github.com/corybrunson/scottish.capital
data(scottish.capital)
scotland1920s <- permute(scottish.capital[[2]],
                         order(order(V(scottish.capital[[2]])$type)))

# Galaskiewicz, "Social organization of an urban grants economy"
library(SNAData)
data(CEOclubsAM)
minneapolis1970s <- graph_from_incidence_matrix(CEOclubsAM)

# Barnes-Burkett corporate board affiliations
# Read data directly into bipartite incidence matrix
chicago1960s <- graph_from_incidence_matrix(as.matrix(read.csv(
  "Barnes-Burkett-Table1.csv", row.names = 1)))

# Noordin Top Terrorist Network Data
# http://www.thearda.com/Archive/Files/Descriptions/TERRNET.asp
# SPSS data were (a) copied into a csv and (b) their names put into a string
# Read data into a data frame
data <- read.csv("Noordin-network.csv", header = FALSE)
rownames(data) <- data[, 1]
data <- data[, 2:dim(data)[2]]
colnames(data) <- c(
  paste("organ", 1:32, sep = ""),
  paste("school", 1:25, sep = ""),
  paste("class", 1:79, sep = ""),
  paste("commun", 1:79, sep = ""),
  paste("kin", 1:79, sep = ""),
  paste("train", 1:16, sep = ""),
  paste("employ", 1:10, sep = ""),
  paste("operat", 1:14, sep = ""),
  paste("friend", 1:79, sep = ""),
  paste("relig", 1:8, sep = ""),
  paste("soul", 1:79, sep = ""),
  paste("place", 1:35, sep = ""),
  paste("provide", 1:4, sep = ""),
  paste("meet", 1:20, sep = ""),
  "educ", "contact", "military", "nation", "status", "role", "group",
  "noordin")
# Restrict to the 32 organizations
data1 <- data[, grep("meet", colnames(data))]
data1 <- data1[rowSums(data1) > 0, ]
nmt_meetings <- graph_from_incidence_matrix(as.matrix(data1))
data2 <- data[, grep("organ", colnames(data))]
data2 <- data2[rowSums(data2) > 0, ]
nmt_organizations <- graph_from_incidence_matrix(as.matrix(data2))

# Fischer-Han Whig groups
# http://www.sscnet.ucla.edu/polisci/faculty/chwe/ps269/han.pdf
# https://github.com/kjhealy/revere
# Read data into a data frame
data <- read.csv("PaulRevereAppD.csv", row.names = 1)
# Remove non-group affiliations (Tea Party and London Enemies)
# and any participants who don't feature elsewhere
data <- data[!(names(data) %in% c("TeaParty", "LondonEnemies"))]
data <- data[rowSums(data) > 0, ]
whigs <- graph_from_incidence_matrix(as.matrix(data))
V(whigs)$name[V(whigs)$type == 0] <- sapply(
  V(whigs)$name[V(whigs)$type == 0], function(name) {
    paste(rev(strsplit(name, split = ".", fixed = TRUE)[[1]]),
          collapse = " ")
  })

# Save graphs to package data folder (with comments as to why)
if (file.exists("../data")) {
  for(name in c(
    # widely used; dynamic; triad closure example
    "women_group",
    # triad census example
    "women_clique",
    # multiple components; interlocking directorates
    "scotland1920s",
    # widely used, triad closure example; interlocking directorates
    "minneapolis1970s",
    # actor and event nodes example; interlocking directorates
    "chicago1960s",
    # partial; subversive groups
    "nmt_meetings",
    # partial; subversive groups
    "nmt_organizations",
    # high actor-event ratio; subversive groups
    "whigs"
  )) {
    save(list = name, file = paste0("../data/", name, ".rda"))
  }
} else warning("Directory 'data' is not where it should be.")
