library(tidyverse)
library(lubridate)

# DEFINIERE Trip der erstellt werden soll
trip <- 79

# Errechen Pfad und frage Filenamen ab 
path <- sprintf("../Fluege Datendetails/trip%03d", trip)
files <- path %>% dir() %>% paste0(path, "/", .) %>% as.list()

# Einlesen der Rohdaten
rawdata <- purrr::map_dfr(files, read_csv)

# Aufräumen
rm(path, files)

# In alte Files UTC wiederherstellen
if(trip <= 58) rawdata <- rawdata %>% mutate(UTC = Timestamp %>% as_datetime()) # Thomas <= 58; Henning <= 2

# Bereinigung Datensatz
tripdata <- rawdata %>%
  arrange(UTC) %>% 
  separate(Position, into = c("Lat", "Long"), sep = ",") %>%
  mutate_at(.vars = c("Lat", "Long"), as.numeric)

# Entferen Artefakte
tripdata <- tripdata %>%
  filter(Altitude > 0 | Speed > 100)

# Reise über die Datumsgrenze
if((tripdata$Long %>% diff() %>% abs() %>% max()) > 300)
  tripdata <- add_row(tripdata, .after = which((tripdata$Long %>% diff() %>% abs()) > 300), Lat = NA, Long = NA)

# Hole Beschreibung der Reise aus 'myflights'
tripdesc <- myflights %>%
  filter(Trip == trip) %>%
  group_by(Leg) %>%
  transmute(res = paste0(c(Dep %>% head(1), Arr), collapse = "-")) %>%
  unique() %>%
  pull(res) %>%
  paste0(collapse = ", ")

# Ergänze Datum und Flugdauer bei tripdesc
tripdesc <- paste0(tripdata$UTC[1] %>% month() %>% sprintf("%02d", .), "/", tripdata$UTC %>% year(), ": ",
                   tripdesc, "; Airtime = ",
                   tripdata %>% group_by(Callsign) %>% summarize(takeoff = min(UTC), land = max(UTC)) %>%
                     mutate(Duration = difftime(land, takeoff, units = "hours")) %>% pull(Duration) %>%
                     sum(na.rm = TRUE) %>% sprintf("%.1f", .),
                   " hours")[1]

# Hole beteiligte Flughäfen aus airports
stops <- myflights %>%
  filter(Trip == trip)
stops <- c(stops$Dep, stops$Arr) %>%
  unique() %>%
  enframe(name = "IATA") %>%
  select(IATA = value) %>%
  left_join(airports)

# Hintergrund (im ersten Schritt werden die Grenzen um 1° erweitert)
library(ggmap)
bgmapbbox <- c(range(tripdata$Long, na.rm = TRUE), range(tripdata$Lat, na.rm = TRUE))[c(1,3,2,4)]
bgmapbbox <- bgmapbbox + 1 * c(-1, -1, 1, 1)
# https://client.stadiamaps.com
# ...Stadia...
register_stadiamaps("523d7972-fb94-4a02-a351-5ab2443b2f92")
bgmap <- get_map(bgmapbbox, source = "stadia", maptype = "stamen_toner")
rm(bgmapbbox)

# Plot
library(ggrepel)
# p <- ggmap(bgmap, base_layer = ggplot(data = tripdata, mapping = aes(x = Long, y = Lat)), darken = c(1 / 2, "linen")) +
p <- ggmap(bgmap, darken = c(1 / 2, "linen")) +
  geom_path(data = tripdata, mapping = aes(x = Long, y = Lat, color = Altitude), lineend = "round", linewidth = 3) +
  geom_path(data = tripdata, mapping = aes(x = Long, y = Lat), lineend = "round", color = "linen") +
  scale_color_gradient(name = "Altitude [1,000 ft]", labels = function(x)x/1e3, low = "green3", high = "dodgerblue", limits = c(0, 42e3)) +
  geom_point(data = stops, mapping = aes(x = Long, y = Lat), size = 2, color = "red3") +
  geom_label_repel(data = stops, mapping = aes(x = Long, y = Lat, label = IATA), point.padding = 2.5, size = 3, color = "red3", fontface = "bold", alpha = 3/4) +
  labs(x = "Longitude [°E]", y = "Latitude [°N]",
       title = paste0("Flightpath of Trip ",trip),
       subtitle = tripdesc) +
  coord_map() +
  theme_bw() +
  theme(plot.title = element_text(size=14, face="bold"), legend.position = "top")

# DIN A4 in mm: 210 x 297; in inch: 8.2 x 11.6
pdfsize <- rev(dim(bgmap))
pdfsize <- round(pdfsize / min(pdfsize) * 8.2, 1)
# Aufschlag für Überschriftenbereich
pdfsize[2] <- pdfsize[2] + 1.5
if(min(pdfsize) > 8.2) pdfsize <- round(pdfsize / min(pdfsize) * 8.2, 1)
if(max(pdfsize) > 11.6) pdfsize <- round(pdfsize / max(pdfsize) * 11.6, 1)

# Ausgabe in pdf-File
pdf(paste0("../Fluege Datendetails/trip",trip,".pdf"), pdfsize[1], pdfsize[2] + 1 / 2)
plot(p)
dev.off()

#Aufräumen
rm(trip, tripdesc, stops, rawdata, tripdata, bgmap, p, pdfsize)