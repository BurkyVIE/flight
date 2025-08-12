library(tidyverse)

trip <- mytrips %>% filter(Trip == 74)

path <- trip %>% select(Path) %>% unnest(col = Path)
apts <- trip %>% select(Airports) %>% unnest(col = Airports) %>% left_join(myairports, by = c("Airports" = "IATA"))

library(rnaturalearth)
box <- path %>% summarise_all(list(min, max), na.rm = TRUE) %>% as.numeric() + c(-1, -1, 1, 1) * 5
box[1] <- max(box[1], -180)
box[2] <- max(box[2], -90)
box[3] <- min(box[3], 180)
box[4] <- min(box[4], 90)
mapdata <- ne_countries(scale = 10, returnclass = "sf")

library(ggrepel)
p <- ggplot() +
  geom_sf(data = mapdata, fill = "antiquewhite") +
  geom_path(data = path, mapping = aes(x = lon, y = lat), linewidth = 2, color = "red3", alpha = 1/4) +
  geom_point(data = apts, mapping = aes(x = Long, y = Lat), size = 4, color = "red3") +
  geom_label_repel(data = apts, mapping = aes(x = Long, y = Lat, label = Airports),
                   size = 3, fontface = "bold", point.padding = 3/4, color = "red3") +
  scale_color_manual(values = c("loc" = "red4", "apt" = "red3")) +
  coord_sf(xlim = box[c(1, 3)], ylim = box[c(2, 4)], expand = FALSE) +
  labs(x = "Longitude", y = "Latitude",
       title = paste0("Trip #", trip$Trip, ": ", trip$Distance %>% round(0) %>% format(big.mark = ","), "km"),
       subtitle = trip$Route) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "lightblue"))

windows(9, 12)
plot(p)

rm(trip, path, apts, box, mapdata, p)

#---

ggplot() +
  geom_sf(data = ne_countries(scale = 50, returnclass = "sf"), fill = "antiquewhite") +
  lapply(myroutes$Path, geom_path, mapping = aes(x = lon, y = lat), color = "red3", alpha = 2/5) +
  geom_point(data = myairports, mapping = aes(x = Long, y = Lat, size = Score), color = "red3", alpha = 1/4, show.legend = FALSE) +
#  coord_sf(expand = FALSE) +
  coord_sf(xlim = c(-12, 54), ylim = c(29, 71), expand = FALSE) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "lightblue"))