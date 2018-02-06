##Trying SOM in R with Kohonen Package and data from NBA Players 

#Installing and loading packages 
packageurl <- "https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.19.tar.gz"
install.packages(packageurl, repos= NULL, type= "source")
require(kohonen) #Link to the package: https://cran.r-project.org/web/packages/kohonen/kohonen.pdf
require(RColorBrewer)

#Getting the data
library(RCurl)
NBA <- read.csv(text = getURL("https://raw.githubusercontent.com/clarkdatalabs/soms/master/NBA_2016_player_stats_cleaned.csv"), 
                sep = ",", header = T, check.names = FALSE)

#To check the intire data glossary, go here: https://www.basketball-reference.com/about/glossary.html

View(NBA)
summary(NBA)
## NBA.measures1: 
# FTA: Free Throw Attempts; 2PA: 2-Point Field Goal Attempts; 3-Point Field Goal Attempts   
NBA.measures1 <- c("FTA", "2PA", "3PA")
summary(NBA.measures1)
scale(NBA[NBA.measures1])

NBA_col <- NBA[c(-1, -2, -3, -5)]
NBA_matrix<- as.matrix(NBA_col)
View(NBA_matrix)

NBA.SOM1 <- som(scale(NBA[NBA.measures1]), grid = somgrid(3, 2, "rectangular"))
plot(NBA.SOM1)

#Training the model

som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")


som_model <- som(NBA_matrix, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 n.hood="square" )
#Viz

plot(NBA.SOM1, type="changes", main = "SOM neighbour distances")
plot(NBA.SOM1, type="codes", main = "SOM neighbour distances")

plot(NBA_matrix, type="dist.neighbours",main = "mapping plot")

