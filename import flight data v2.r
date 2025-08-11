# LIBRARIES ----
library(tidyverse)
library(lubridate)
library(geosphere)

# GLOBAL ----
# Eintellungen Zuhause (Zentrum) ----
spot <- "VIE"

# function makepath ----
makepath <- function(lona, lata, lonb, latb, dist){
  ret <- gcIntermediate(c(lona, lata), c(lonb, latb), n = round(10 * log(dist, 100)), addStartEnd = TRUE, breakAtDateLine = TRUE)
  # Geht die Linie über Datumsgrenze, dann erzeugt 'breakAtDateLine' eine Liste mit zwei Matrizen
  if(class(ret)[1] == "list") ret <- rbind(ret[[1]], NA, ret[[2]])
  return(ret |> as_tibble())
}

# DATA ----
## myflights ----
# Einlesen
myflights_raw <- read_delim("myflights_thomas_2.dat", delim = " ", show_col_types = FALSE, lazy = FALSE)

# Korrigieren der Flugnummern [optional]
myflights_raw <- myflights_raw |> 
  mutate(Flight = case_when(
    !is.na(Flight) & nchar(Flight) == 4 ~ paste0(substr(Flight, 1, 2), " 0", substr(Flight, 3, 4)),
    !is.na(Flight) & nchar(Flight) == 5 ~ paste0(substr(Flight, 1, 2), " ", substr(Flight, 3, 5)),
    !is.na(Flight) & nchar(Flight) == 6 ~ Flight))

## airports ----
# # Prüfe ob neuer Download nötig; Neuerlicher Download auch, wenn File älter als 90 Tage (comment)
# getit <- FALSE
# if(!exists("airports")) getit <- TRUE else
#   if(difftime(Sys.time(), comment(airports), units = "days") > 90) getit <- TRUE
# 
# # Neu herunterladen
# if(getit) {
#   download.file("http://ourairports.com/data/airports.csv", destfile = "airports.csv")
#   apts <- read_csv("airports.csv", na = "", show_col_types = FALSE, lazy = FALSE)
#   file.remove("airports.csv")
#   
#   # AIRPORTS - Ausschnitt aus 'apts'
#   # Nur durchführen wenn es Inhalte in der heruntergeladenen Datei gibt
#   if(dim(apts)[1] > 0) {
#     
#     # Kürze 'Size' und 'Name' und rechne 'Elev' von ft in m um
#     airports <- apts |> 
#       select(IATA = iata_code,
#              ICAO = ident,
#              Size = type,
#              Name = name,
#              Continent = continent,
#              Country = iso_country,
#              Long = longitude_deg,
#              Lat = latitude_deg,
#              Elevation = elevation_ft) |> 
#       mutate(Size = gsub("(\\w+)_airport", "\\1", Size),
#              Elevation = round(Elevation * .3048, 0))
#     
#     # Füge Erstelldatum als 'comment' hinzu
#     comment(airports) <- as.character(Sys.time())
#     
#     # Füge IATA für besuchte geschlossene Flughäfen wieder hinzu (ie TXL)
#     # airports[airports$ICAO == "EDDT", "IATA"] <- "TXL"
#     airports <- add_row(airports, data.frame(IATA = "TXL", ICAO = "EDDT", Size = "closed", Name = "Berlin-Tegel Otto Lilienthal Airport", Continent = "EU", Country = "DE", Lat = 52.5597, Long = 13.2877, Elevation = round(122 * .3048, 0)))
#     
#   }
#   
#   #Aufräumen
#   rm(apts)
# }
# 
# # Aufräumen
# rm(getit)

# TRANSFORM ----
## ergänze myflights ----
myflights <- myflights_raw |> 
  left_join(airports |> 
              select(IATA, Continent, Country, Long, Lat),
            by = c("Dep" = "IATA")) |>
  left_join(airports |> 
              select(IATA, Continent, Country, Long, Lat),
            by = c("Arr" = "IATA"), suffix = c(".Dep", ".Arr"))

# Errechnete Informationen
myflights <- myflights |> 
  mutate(Distance = distGeo(myflights |> select(Long.Dep, Lat.Dep),
                            myflights |> select(Long.Arr, Lat.Arr)) / 1e3,
         Bearing = bearing(myflights |> select(Long.Dep, Lat.Dep),
                           myflights |> select(Long.Arr, Lat.Arr)),
         Relation = case_when(Country.Dep == Country.Arr ~ "Domestic",
                              TRUE ~ "Int'l")) |> 
  mutate(Path = pmap(list(lona = Long.Dep, lata = Lat.Dep, lonb = Long.Arr, latb = Lat.Arr, dist = Distance),
                                          makepath))
# Aufräumen
rm(myflights_raw)

# myairports ---
myairports <- myflights |>
  select(Dep, Arr) |>
  gather(value = "IATA") |>
  select(-key) |>
  unique() |>
  arrange(IATA) |>
  left_join(airports, by = "IATA")

# Abflüge und Ankünfte national und international, sowie Connections
myairports <- myairports |>
  left_join(myflights |>
              group_by(Dep) |>
              summarise(Int.Dep = sum(Relation == "Int'l"),
                        Dom.Dep = sum(Relation == "Domestic")),
            by = c("IATA" = "Dep")) |>
  left_join(myflights |>
              group_by(Arr) |>
              summarise(Int.Arr = sum(Relation == "Int'l"),
                        Dom.Arr = sum(Relation == "Domestic")),
            by = c("IATA" = "Arr")) |>
  left_join(myflights |>
              transmute(Dep = Dep, Con = paste0(Trip, Leg)) |>
              mutate(Con = Con == lag(Con, default = "00")) |>
              group_by(Dep) |>
              summarise(Connect = sum(Con)),
            by = c("IATA" = "Dep")) |>
  replace_na(list(Int.Dep = 0L,
                  Dom.Dep = 0L,
                  Int.Arr = 0L,
                  Dom.Arr = 0L,
                  Connect = 0L))

# Berechne 'Score' und Entfernung von Spot
myairports <- myairports |>
  mutate(Score = log(0.7 * Int.Dep +
                       0.6 * Dom.Dep +
                       1 * Int.Arr +
                       0.85 * Dom.Arr +
                        -0.25 * Connect +
                       1,
                     base = 5) |> round(3),
         Dist_Spot = distGeo(myairports |> select(Long, Lat),
                             myairports |> filter(IATA == spot) |> select(Long, Lat)) / 1e3)

# Sortieren von 'myairports' nach 'Dist_Spot' und 'Score' und hinzufÃ¼gen von 'Rank'
myairports <- myairports |>
  arrange(-Score, -Dist_Spot) |>
  add_column(Rank = seq(myairports |> count() |> pull()))

# myroutes ----
myroutes <- myflights |>
  transmute(A = case_when(Dep < Arr ~ Dep,
                          TRUE ~ Arr),
            Z = case_when(Dep > Arr ~ Dep,
                          TRUE ~ Arr),
            Route = paste(A, Z, sep ="-"),
            A2Z = A == Dep,
            Distance) |>
  group_by(A, Z) |>
  summarise(Route = first(Route),
            Distance = first(Distance),
            Freq = n(),
            A2Z = sum(A2Z),
            Z2A = Freq - A2Z,
            .groups = "drop") |>
  select(-Freq) |>
  left_join(airports |>
              select(IATA,
                     Long,
                     Lat),
            by = c("A" = "IATA")) |>
  left_join(airports |>
              select(IATA,
                     Long,
                     Lat),
            by = c("Z" = "IATA"), suffix = c(".A", ".Z")) |>
  mutate(Path = purrr::pmap(list(lona = Long.A, lata = Lat.A, lonb = Long.Z, latb = Lat.Z, dist = Distance),
                            makepath)) |>
  arrange(-A2Z - Z2A, -Distance)

# mytrips ----
# Beim Zusammenlegen der Pfade werden NAs eingefügt um Verbindungen dort zu vermeiden wo keine sind
mytrips <- myflights |>
  select(Trip, Leg, Dep, Arr, Distance, Path) |>
  group_by(TL = paste0(Trip |> (\(x) sprintf("%03d", x))(), Leg)) |>
  summarise(Trip = first(Trip),
            Route = paste0(c(Dep |> head(1), Arr), collapse = "-"),
            Distance = sum(Distance),
            Path = reduce(Path, bind_rows) |> add_row(lon = NA, lat = NA) |> list()) |>
  group_by(Trip) |>
  summarise(Route = paste0(Route, collapse = ", "),
            Distance = sum(Distance),
            Path = reduce(Path, bind_rows) |> add_row(lon = NA, lat = NA) |> list()) |>
  mutate(Airports = strsplit(Route, "[^A-Z]{1,2}") |> lapply(unique)) |>
  arrange(-Distance)

# myconnections ----
myconnections <- full_join(
  myflights |> select(IATA = Arr, from = Dep) |> unique() |> arrange(from) |> chop(from),
  myflights |> select(IATA = Dep, to = Arr) |> unique() |> arrange(to) |> chop(to),
  by = "IATA") |>
  arrange(IATA) |>
  mutate_at(vars(-IATA), ~ purrr::map(., unlist)) |>
  mutate(IATA,
         inbound = purrr::map2(from, to, setdiff),
         roundtrip = purrr::map2(to, from, intersect),
         outbound = purrr::map2(to, from, setdiff),
         no_i = purrr::map_int(inbound, length),
         no_r = purrr::map_int(roundtrip, length),
         no_o = purrr::map_int(outbound, length)
  ) |>
  mutate(from = purrr::map(from, ~paste0(., collapse = "/")),
         to = purrr::map(to, ~paste0(., collapse = "/")),
         inbound = purrr::map(inbound, ~paste0(., collapse = "/")),
         roundtrip = purrr::map(roundtrip, ~paste0(., collapse = "/")),
         outbound = purrr::map(outbound, ~paste0(., collapse = "/"))) |>
  unnest(cols = c(from, to, inbound, roundtrip, outbound, no_i, no_r, no_o)) |>
  mutate(no = no_i + no_r + no_o,
         desc = paste0(IATA, " (", no, "):   ", no_i, " to ", IATA, " to ", no_r, " to ", IATA, " to ", no_o),
         desc_long = case_when(no_i == 0 & no_r == 0 ~ paste0(IATA, ": ", IATA, " to ", outbound),
                               no_i == 0 & no_o == 0 ~ paste0(IATA, ": ", IATA, " to ", roundtrip, " to ", IATA),
                               no_r == 0 & no_o == 0 ~ paste0(IATA, ": ", inbound, " to ", IATA),
                               no_i == 0 ~ paste0(IATA, ": ", IATA, " to ", roundtrip, " to ", IATA, " to ", outbound),
                               no_r == 0 ~ paste0(IATA, ": ", inbound, " to ", IATA, " to ", outbound),
                               no_o == 0 ~ paste0(IATA, ": ", inbound, " to ", IATA, " to ", roundtrip, " to ", IATA),
                               TRUE ~ paste0(IATA, ": ", inbound, " to ", IATA, " to ", roundtrip, " to ", IATA, " to ", outbound)
         )
  ) |>
  arrange(-no)

# Aufräumen
rm(spot, makepath)

