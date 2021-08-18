
# Setup -------------------------------------------------------------------

library(tidyverse) #< Load tidyverse to get the pipe.
library(readr)


# Change this to reflect the location of YOUR file.
data.file <- "data/Bordeaux.csv"


# Read Data ---------------------------------------------------------------

bordeaux.raw <- read_csv(data.file)

glimpse(bordeaux.raw)

count(bordeaux.raw, P95andAbove)
count(bordeaux.raw, FirstGrowth )
count(bordeaux.raw, CultWine )
count(bordeaux.raw, Pomerol )
count(bordeaux.raw, VintageSuperstar )


# Clean-up ----------------------------------------------------------------

bordeaux <- bordeaux.raw %>%
    mutate_at(vars(P95andAbove:VintageSuperstar), as.logical)

# Save --------------------------------------------------------------------

saveRDS(bordeaux, "bordeaux.rds")
