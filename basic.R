# Second argument to apply. Seriously, R, magic numbers?!
ROWS <- 1
COLS <- 2
BOTH <- c(ROWS, COLS)

# Analogous to vec[1]
last <- function (vec) tail(vec, n = 1)

# Adjust tick marks -- not necessary with OS X PDF driver, it seems.
#plot <- function (x, y, ...) {
#    result <- graphics::plot(x, y, xaxt = 'n', yaxt = 'n', ...)
#    xTicksAt <- axTicks(1, usr = par('usr')[1 : 2])
#    yTicksAt <- axTicks(2, usr = par('usr')[3 : 4])
#    xLabs <- gsub('-', '\U2212', print.default(xTicksAt))
#    yLabs <- gsub('-', '\U2212', print.default(yTicksAt))
#    axis(1, at = xTicksAt, labels = xLabs)
#    axis(2, at = yTicksAt, labels = yLabs)
#    result
#}

# FIXME Replace by module definitions.
source('functional.R')
source('strings.R')
source('graphics.R')
source('io.R')
source('system.R')
