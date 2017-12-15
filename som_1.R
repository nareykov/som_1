#https://clarkdatalabs.github.io/soms/SOM_NBA
packageurl <- "https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.19.tar.gz"
install.packages(packageurl, repos = NULL, type = "source")

require(kohonen)
require(RColorBrewer)

library(RCurl)
NBA <- read.csv(text = getURL("https://raw.githubusercontent.com/clarkdatalabs/soms/master/NBA_2016_player_stats_cleaned.csv"), 
                sep = ",", header = T, check.names = FALSE)

colnames(NBA)

NBA.measures1 <- c("FTA", "2PA", "3PA")
NBA.SOM1 <- som(scale(NBA[NBA.measures1]), grid = somgrid(6, 4, "rectangular"))
plot(NBA.SOM1)

# reverse color ramp
colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}

plot(NBA.SOM1, type = "counts", palette.name = colors, heatkey = TRUE)

par(mfrow = c(1, 2))
plot(NBA.SOM1, type = "mapping", pchs = 20, main = "Mapping Type SOM")
plot(NBA.SOM1, main = "Default SOM Plot")

NBA.SOM2 <- som(scale(NBA[NBA.measures1]), 
                grid = somgrid(6, 6, "hexagonal", 
                               toroidal = TRUE))

par(mfrow = c(1, 2))
plot(NBA.SOM2, type = "mapping", pchs = 20, main = "Mapping Type SOM")
plot(NBA.SOM2, main = "Default SOM Plot")

plot(NBA.SOM2, type = "dist.neighbours", palette.name = terrain.colors)

NBA.measures2 <- c("FTA", "FT", "2PA", "2P", "3PA", "3P", "AST", "ORB", "DRB", 
                   "TRB", "STL", "BLK", "TOV")

training_indices <- sample(nrow(NBA), 200)
NBA.training <- scale(NBA[training_indices, NBA.measures2])
NBA.testing <- scale(NBA[-training_indices, NBA.measures2], center = attr(NBA.training, 
                                                                          "scaled:center"), scale = attr(NBA.training, "scaled:scale"))

NBA.SOM3 <- xyf(NBA.training, classvec2classmat(NBA$Pos[training_indices]), 
                grid = somgrid(13, 13, "hexagonal", toroidal = TRUE), rlen = 100, xweight = 0.5)

pos.prediction <- predict(NBA.SOM3, newdata = NBA.testing)
table(NBA[-training_indices, "Pos"], pos.prediction$prediction)

NBA.SOM4 <- xyf(scale(NBA[, NBA.measures2]), classvec2classmat(NBA[, "Pos"]), 
                grid = somgrid(13, 13, "hexagonal", toroidal = TRUE), rlen = 300, xweight = 0.7)

par(mfrow = c(1, 2))
plot(NBA.SOM4, type = "codes", main = c("Codes X", "Codes Y"))
NBA.SOM4.hc <- cutree(hclust(dist(NBA.SOM4$codes$Y)), 5)
add.cluster.boundaries(NBA.SOM4, NBA.SOM4.hc)

bg.pallet <- c("red", "blue", "yellow", "purple", "green")

# make a vector of just the background colors for all map cells
position.predictions <- classmat2classvec(predict(NBA.SOM4)$unit.predictions)
base.color.vector <- bg.pallet[match(position.predictions, levels(NBA$Pos))]

# set alpha to scale with maximum confidence of prediction
bgcols <- c()
max.conf <- apply(NBA.SOM4$codes$Y, 1, max)
for (i in 1:length(base.color.vector)) {
  bgcols[i] <- adjustcolor(base.color.vector[i], max.conf[i])
}

par(mar = c(0, 0, 0, 4), xpd = TRUE)
plot(NBA.SOM4, type = "mapping", pchs = 21, col = "black", bg = bg.pallet[match(NBA$Pos, 
                                                                                levels(NBA$Pos))], bgcol = bgcols)

legend("topright", legend = levels(NBA$Pos), text.col = bg.pallet, bty = "n", 
       inset = c(-0.03, 0))

