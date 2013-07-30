# Second argument to apply. Seriously, R, magic numbers?!
ROWS <- 1
COLS <- 2
BOTH <- c(ROWS, COLS)

# Analogous to vec[1]
last <- function (vec) tail(vec, n = 1)

# FIXME Replace by module definitions.
source('functional.R')
source('strings.R')
source('graphics.R')
source('io.R')
source('system.R')
source('seq.R')
