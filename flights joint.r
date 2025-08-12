library(tidyverse)
library(geosphere)

#
### GLOBALE WERTE
# Farben (Im Hauptplot Ränder, Punkte, Linie, Akzent, Score, Umkreis)
colo <- c("grey67", "dodgerblue4", "dodgerblue3", "powderblue", "firebrick", "white")
# Eintellungen Umkreis
spot <- "VIE"
radius <- 2500

#
### PLOT
# Plot 1
# Weltkarte
p1 <- ggplot() +
  geom_polygon(data = map_data("world"), mapping = aes(x = long, y = lat, group = group), fill = "grey75", color = colo[1]) +
  scale_x_continuous(name = "Longitude [°E]", breaks = seq(from = -180, to = 180, by = 30), expand = c(0, 0)) +
  scale_y_continuous(name = "Latitude [°N]", breaks = seq(from = -90, to = 90, by = 30), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-180, 190), ylim = c(-85, 90))

# Umkreis um 'Spot'
# circ <- data.frame(destPoint(airports[which(airports$IATA == spot), c("Long", "Lat")], seq(0, 359, by = 5), radius * 1e3), fill = "a")
circ <- data.frame(destPoint(filter(myflights, Dep == spot) |> pluck("DepDet", 1) |> select(Long, Lat) |> unlist(), seq(0, 359, by = 5), radius * 1e3), fill = "a")
p1 <- p1 +
  geom_polygon(data = circ, aes(x = lon, y = lat, fill = fill), alpha = 1/2) +
  scale_fill_manual(name = "Radius", values = colo[6], label = paste(radius, "km @ VIE"))

# Aufräumen
rm(circ)

# Zeichnen der Großkreissegmente
paths <- myroutes %>% select(Route, GCPath) %>% unnest(cols = GCPath)
p1 <- p1 + geom_path(data = paths, mapping = aes(x = Long, y = Lat, group = Route), color = colo[3])

# Aufräumen
rm(paths)

# Punkte auf Flughäfen (zuerst 'Score')
p1 <- p1 +
  geom_point(data = myairports, mapping = aes(x = Long, y = Lat, size = Score), color = colo[5], alpha = 1/3) +
  geom_point(data = myairports, mapping = aes(Long, Lat), color = colo[2], size = 1.25) +
  theme(legend.position = "top", legend.box = "horizontal")

# PLOT 2
p2 <- ggplot(data = myflights, mapping = aes(x = GCDist, fill = Relation)) +
  geom_histogram(binwidth = 625, center = 312.5, color = colo[1]) +
  labs(x = "Distance [km]", y = "Count") +
  scale_x_continuous(breaks = seq(from = 0, to = 20000, by = 5000)) +
  scale_fill_manual(values = c(colo[4], colo[2])) +
  theme(legend.position = "top")

#PLOT 3
p3 <- myflights %>%
  transmute(Distance = GCDist,
            InitHead = cut((Bearing + 22.5 ) %% 360, (0:8) * 45, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))) %>%
  group_by(InitHead) %>%
  summarise(Count = n(), AvgDist = mean(Distance)) %>%
  ggplot(mapping = aes(x = InitHead, y = Count)) +
  geom_bar(aes(fill = AvgDist), width = 1, color = colo[1], stat = "identity") +
  labs(x = "Initial Heading", y = "Count") +
  coord_polar(theta = "x", start = -22.5 / 180 * pi) +
  scale_fill_gradient(name = "Avg Distance", low = colo[2], high = colo[4], breaks = c(0, 1250, 2500, 3750, 5000)) +
  theme(legend.position = "top")

#windows(16, 8)
pdf(paste0("myflights_", comment(myflights), ".pdf"), width = 16, height = 8)
gridExtra::grid.arrange(p1, p2, p3 ,layout_matrix = matrix(c(1, 1, 1, 1, 2, 1, 1, 1, 1, 3), nrow=2, byrow=T),
                        top = grid::textGrob(paste("My Flights", comment(myflights), sep = " - "), gp = grid::gpar(fontsize = 17)))
dev.off()
rm(p1, p2, p3, colo, radius, spot)
