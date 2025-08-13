# LIBRARIES ----
library(tidyverse)
library(geosphere)

# GLOBAL ----

spot <- "VIE"
person <- "Thomas"

# AIRPORTS DATA ----
## import data ----
airportdata <- read_delim("airports.dat", delim = " ", quote = '"', lazy = FALSE,
                          col_types = "cccccddi") |> 
  mutate(Elevation_m = Elevation_ft * .3048, .keep = "unused")

# TRUE PATH DATA ----
## find and import data ----
truepathdata <- dir("Detaildaten/", recursive = TRUE, pattern = "\\.csv", full.names = TRUE) |>
  enframe(value = "File", name = NULL) |>
  mutate(Extr = str_replace(File, "Detaildaten\\/trip_((?:[HT0-9]{4})+)\\/.*?([0-9A-Z]{3,6}).*\\.csv", "\\1 \\2")) |>
  separate(Extr, into = c("Trip", "Flight")) |> 
  mutate(TruePath = map(File, ~tibble(read_csv(., show_col_types = FALSE, lazy = FALSE)) |>
                          separate(Position, into = c("Lat", "Long"), sep = ",") |>
                          filter(Speed >= 30)))

# FLIGHTS DATA ----
## function to clean flightnumbers
clean_flight <- function(txt)
  if(is.na(txt)) return(NA) else
    str_match(txt, "(..)(.*)") |> (\(x)paste0(x[,2], str_pad(x[,3], 4)))()

## function to read and modify files ----
read_myflights <- function(file){
  pers <- str_split_1(file, "_")[2] |> str_to_title()
  read_delim(file, delim = " ", lazy = FALSE,
             col_types = "icDcccciicc") |> 
    add_column(Pers = pers, .before = 1) |>
    mutate(.keep = "unused",
           # Flight = map_chr(Flight, ~clean_flight(.)),
           SDur = hours(Dur.h) + minutes(Dur.min))
}

## select relevant files ----
files <- dir() |> 
  str_subset("myflights_.*_2\\.dat")

## import data ----
flightdata <- map_df(files, ~read_myflights(.))
### clean up ----
rm(clean_flight, read_myflights, files)

## summarise for joint flights ----
flightdata <- nest(flightdata, Ident = Pers:Trip) |>
  mutate(Trip = map_chr(Ident, ~paste(paste0(rev(str_sub(.$Pers, 1, 1)), rev(str_pad(.$Trip, 3, pad = "0"))), collapse = "")), .before = 1) |>
  relocate(Ident, .after = Leg) |> 
  arrange(Date)

## add DEP details ----
flightdata <- left_join(flightdata, airportdata, by = c("Dep" = "IATA")) |> 
  nest(DepDet = ICAO:Elevation_m) |> 
  relocate(DepDet, .after = Dep)

## add ARR details ----
flightdata <- left_join(flightdata, airportdata, by = c("Arr" = "IATA")) |> 
  nest(ArrDet = ICAO:Elevation_m) |> 
  relocate(ArrDet, .after = Arr)

### clean up ----
rm(airportdata)

## function to calculate gc matters ----
gc_calc <- function(p1, p2) {
  GCDist <- distGeo(p1, p2) / 1e3
  Bearing <- bearing(p1, p2)
  GCPath <- gcIntermediate(p1, p2, n = round(25 * log(GCDist, 10)), addStartEnd = TRUE, breakAtDateLine = TRUE) # Geht die Linie über Datumsgrenze, dann erzeugt 'breakAtDateLine' eine Liste mit zwei Matrizen
  if(class(GCPath)[1] == "list") GCPath <- rbind(GCPath[[1]], NA, GCPath[[2]])
  return(tibble(GCDist, Bearing, GCPath = list(tibble(Long = GCPath[,1], Lat = GCPath[,2]))))
}

## add relation and great circle calculations ----
flightdata <- mutate(Relation = case_when(map2_lgl(DepDet, ArrDet, ~.x$Country == .y$Country) ~ "Domestic",
                                          TRUE ~ "International"),
                     flightdata, Res = map2(DepDet, ArrDet, ~gc_calc(p1 = c(.x$Long, .x$Lat), p2 = c(.y$Long, .y$Lat)))) |>
  unnest(Res)
### clean up ----
rm(gc_calc)

## add true path data
flightdata <- left_join(flightdata, select(truepathdata, Trip, Flight, TruePath), by = c("Trip", "Flight"))
### clean up ----
rm(truepathdata)

# find 'myflights' with 'truePath' data
# filter(myflights, !(myflights |> pull(TruePath) |> lapply(is.null) |> unlist()))

# MYFLIGHTS ----
myflights <- flightdata |> 
  unnest(cols = Ident, names_sep = ".") |>
  filter(Ident.Pers == person) |>
  select(-c(Trip, Ident.Pers)) |>
  relocate(Trip = Ident.Trip, .before = 1)
comment(myflights) <- person

# MYAIRPORTS ----
myairports <- select(myflights, IATA = Dep, DepDet) |> unnest(cols = DepDet) |>
  bind_rows(select(myflights, IATA = Arr, ArrDet) |> unnest(cols = ArrDet)) |>
  unique()

## Departures and Arrivals Domestic and International, sowie Connections
myairports <- myairports |>
  left_join(myflights |>
              group_by(Dep) |>
              summarise(Int.Dep = sum(Relation == "International"),
                        Dom.Dep = sum(Relation == "Domestic")),
            by = c("IATA" = "Dep")) |>
  left_join(myflights |>
              group_by(Arr) |>
              summarise(Int.Arr = sum(Relation == "International"),
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

## Calculate 'Score' and Dist to 'Spot' ----
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

## sort 'myairports' by 'Dist_Spot' and 'Score' add 'Rank'
myairports <- myairports |>
  arrange(-Score, -Dist_Spot) |>
  rowid_to_column(var = "Rank")
comment(myairports) <- person

# MYROUTES ----
myroutes <- myflights |>
  transmute(A = case_when(Dep < Arr ~ Dep, TRUE ~ Arr),
            ADet = case_when(Dep < Arr ~ DepDet, TRUE ~ ArrDet),
            Z = case_when(Dep > Arr ~ Dep, TRUE ~ Arr),
            ZDet = case_when(Dep > Arr ~ ArrDet, TRUE ~ DepDet),
            GCPath = case_when(Dep < Arr ~ GCPath, TRUE ~ map(GCPath, ~tibble(arrange(., desc(row_number()))))),
            Route = paste(A, Z, sep ="-"),
            A2Z = A == Dep,
            GCDist) |>
  group_by(A, ADet, Z, ZDet) |>
  reframe(Route = first(Route),
            GCDist = first(GCDist),
            A2Z = sum(A2Z),
            Z2A = n() - A2Z,
            GCPath = list(first(GCPath)))
comment(myroutes) <- person

# MYTRIPS ----
mytrips <- myflights |> 
  group_by(TL = paste0(Trip, Leg)) |> 
  summarise(Trip = first(Trip),
            Route = paste0(c(Dep |> head(1), Arr), collapse = "-"),
            GCDist = sum(GCDist),
            GCPath = reduce(GCPath, bind_rows) |> add_row(Long = NA, Lat = NA) |> list(), # Beim Zusammenlegen der Pfade werden NAs eingefügt um Verbindungen dort zu vermeiden wo keine sind
            TruePath = reduce(TruePath, bind_rows, .init = tibble(Timestamp = NA, UTC = NA, Callsign = NA, Lat = NA, Long = NA, Altitude = NA, Speed = NA, Direction = NA)) |> add_row(Long = NA, Lat = NA) |> list()
            ) |> 
  group_by(Trip) |>
  summarise(Route = paste0(Route, collapse = ", "),
            GCDist = sum(GCDist),
            GCPath = reduce(GCPath, bind_rows) |> add_row(Long = NA, Lat = NA) |> list(), # Beim Zusammenlegen der Pfade werden NAs eingefügt um Verbindungen dort zu vermeiden wo keine sind
            TruePath = reduce(TruePath, bind_rows, .init = tibble(Timestamp = NA, UTC = NA, Callsign = NA, Lat = NA, Long = NA, Altitude = NA, Speed = NA, Direction = NA)) |> add_row(Long = NA, Lat = NA) |> list()
            ) |>
  mutate(Airports = strsplit(Route, "[^A-Z]{1,2}") |> lapply(unique)) |>
  arrange(desc(GCDist))
comment(mytrips) <- person

# CLEAN-UP ----
rm(person, spot)
