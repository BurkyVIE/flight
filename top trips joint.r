library(tidyverse)
library(ggrepel)

#
### GLOBALE WERTE
# Anzahl TOP
topn <- 5
#Farben
# color <- rainbow(topn, v = abs(seq(from = -2, to = 2, length = topn) / 10) + .8, start = 0.125, end = 0.875)
color <- viridis::plasma(topn, begin = 0.125, end = 0.875, direction = -1)

# Finde TOP 'topn' (längste) Trips
toptrips <- mytrips %>%
  head(topn)

# Benenne Farben nach den TOP-Trips
names(color) <- toptrips %>% pull(Trip)

# Extrahiere entsprechende Flughäfen
topapts <- toptrips %>%
  pull(Airports) %>%
  unlist() %>%
  unique() %>%
  enframe(name = NULL) %>%
  rename(IATA = value) %>%
  left_join(myairports %>% select(IATA,
                                Long,
                                Lat),
            by = c("IATA" = "IATA"))

# Extrahiere Flugpfade
paths <- toptrips %>%
  select(Trip, GCPath) %>% unnest(cols = GCPath)

#
### PLOT
# Plot 1
p1 <- ggplot(data = toptrips) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, topn)) +
  geom_text(mapping = aes(x = 0, y = (topn:1) - 0.5, label = paste0("Trip ",
                                                                    Trip,
                                                                    " (",
                                                                    format(GCDist %>% round(0), big.mark = ","),
                                                                    " km)\n",
                                                                    Route),
                          color = as.character(Trip)), hjust = 0, vjust = 0.5, size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = color) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# PLOT 2
p2 <- ggplot(data = toptrips, mapping = aes(x = reorder(as.character(Trip), GCDist, function(x) -x), y = GCDist, fill = as.character(Trip))) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  scale_y_continuous(labels = function(x) format(x, big.mark =",")) +
  scale_fill_manual(values = color) +
  labs(x = "Trip", y = "Distance [km]")

# PLOT 3
# Weltkarte
p3 <- ggplot() +
  geom_polygon(data = map_data("world"), mapping = aes(x = long, y = lat, group = group), fill = "grey75", color = "grey67") +
  scale_x_continuous(name = "Longitude [°E]", breaks = seq(from = -180, to = 180, by = 30), expand = c(0, 0)) +
  scale_y_continuous(name = "Latitude [°N]", breaks = seq(from = -90, to = 90, by = 30), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-180, 190), ylim = c(-85, 90))

# Flugpfade
p3 <- p3 +
  geom_path(data = paths, mapping = aes(x = Long, y = Lat, group = Trip, color = as.character(Trip)), linewidth = 1.5, show.legend = FALSE) +
  scale_color_manual(values = color)

# Flughäfen - Punkte
p3 <- p3 +
  # geom_label_repel(data = topapts, aes(x = Long, y = Lat, label = IATA), size = 2.5, fill = "black", colour = "white",
  #            label.r = 0.25, label.padding = 0.15, label.size = NA, fontface = "bold",
  #            point.padding = 0)
  geom_label(data = topapts, aes(x = Long, y = Lat, label = IATA), size = 2.5, fill = "black", colour = "white",
                   label.r = unit(0.25, "lines"), label.size = NA, fontface = "bold")

#windows(18, 8)
pdf(paste0("top trips_", comment(mytrips), ".pdf"), width = 18, height = 8)
gridExtra::grid.arrange(p1,p2,p3,layout_matrix=matrix(c(1,3,3,3,3,2,3,3,3,3),nrow=2,byrow=T),
                        top=grid::textGrob(paste0("Top ", topn, " Trips - ", comment(mytrips)),gp=grid::gpar(fontsize=17)))
dev.off()

#Aufräumen
rm(topn, color, toptrips, topapts, paths, p1, p2, p3)
