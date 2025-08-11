# LIBRARIES ----
library(tidyverse)

# GLOBAL ----

# READ DATA ----
## Thomas ----
data_t <- read_delim("myflights_thomas_2.dat", delim = " ", lazy = FALSE,
                     col_types = "icDcccciicc") |> 
  add_column(.before = "Trip", Person = "Thomas") |> 
  select(-starts_with("Dur"))
## Henning ----
data_h <- read_delim("myflights_henning_2.dat", delim = " ", lazy = FALSE,
                     col_types = "icDcccciicc") |> 
  add_column(.before = "Trip", Person = "Henning") |> 
  select(-starts_with("Dur"))

# COMBINE DATA ----
flightdata <- left_join(data_t, data_h, by = c())
