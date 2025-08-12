# LIBRARIES ----
library(tidyverse)
library(geosphere)

# GLOBAL ----
# AIRPORTS DATA ----
## import data ----
airportdata <- read_delim("airports.dat", delim = " ", quote = '"', lazy = FALSE,
                          col_types = "cccccddi") |> 
  mutate(Elevation_m = Elevation_ft * .3048, .keep = "unused")

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
           Flight = map_chr(Flight, ~clean_flight(.)),
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
  arrange(Date)

## add DEP details ----
flightdata <- left_join(flightdata, airportdata, by = c("Dep" = "IATA")) |> 
  nest(DepDet = ICAO:Elevation_m)

## add ARR details ----
flightdata <- left_join(flightdata, airportdata, by = c("Arr" = "IATA")) |> 
  nest(ArrDet = ICAO:Elevation_m)

### clean up ----
rm(airportdata)

## function to calculate gc matters ----
gc_calc <- function(p1, p2) {
  Bearing <- bearing(p1, p2)
  GCDist <- distGeo(p1, p2) / 1e3
  GCPath <- gcIntermediate(p1, p2, n = round(25 * log(GCDist, 10)), addStartEnd = TRUE, breakAtDateLine = TRUE) # Geht die Linie über Datumsgrenze, dann erzeugt 'breakAtDateLine' eine Liste mit zwei Matrizen
  if(class(GCPath)[1] == "list") GCPath <- rbind(GCPath[[1]], NA, GCPath[[2]])
  colnames(GCPath) <- c("Long", "Lat")
  return(tibble(Bearing, GCDist, GCPath = list(GCPath)))
}

## add  great circle calculations
flightdata <- mutate(flightdata, Res = map2(DepDet, ArrDet, ~gc_calc(p1 = c(.x$Long, .x$Lat), p2 = c(.y$Long, .y$Lat))),
                     Relation = case_when(map2_lgl(DepDet, ArrDet, ~.x$Country == .y$Country) ~ "Domestic",
                                          TRUE ~ "International")) |>
  unnest(Res)
### clean up
rm(gc_calc)







# RESTE ----

flightdata |> filter(str_detect(Trip, "T")) |> pluck("GCPath") |> (\(x)do.call(rbind, lapply(x, as.data.frame)))() |> (\(x)plot(x$Long, x$Lat, type = "l"))()


files <- dir(recursive = TRUE) |> 
  str_subset(".*\\.csv")
flightdata <- mutate(flightdata, FlightPath = )







