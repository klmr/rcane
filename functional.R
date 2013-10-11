# Functional tools

# Basic helpers {{{

id <- function (x) x

# This uses R's peculiarities in argument matching explained here:
# <http://stat.ethz.ch/R-manual/R-devel/doc/manual/R-lang.html#Argument-matching>
# `.expr` starts with a dot to allow `expr` being used in the actual
# expression.
let <- function (.expr, ...)
    eval(substitute(.expr), list2env(list(...), parent = parent.frame()))

# }}}

# Tools for function composition and chaining {{{

#' Partial function application from right to left.
#' NB: this is the opposite from the (wrongly-named) roxygen::Curry:
#'
#'   minus <- function (x, y) x - y
#'   partial(minus, 5)(1) == -4
#'
#' But:
#'
#'   partial(minus, x = 5)(1) == 4
#'
partial <- function (f, ...)
    let(capture = list(...),
        function (...) do.call(f, c(list(...), capture)))

lpartial <- function(f, ...)
    let(capture = list(...),
        function (...) do.call(f, c(capture, list(...))))

# Define shortcuts because these functions are so commonly used and constitute
# syntactic noise.
# Not something I would normally do but there's precedence in R; consider `c`.

p <- partial
lp <- lpartial

#' Compose functions `g` and `f`. `compose(g, f)(...) = g(f(...))`.
#' NB: Functions are applied in the inverse order of `roxygen::Compose`.
#' <http://tolstoy.newcastle.edu.au/R/e9/help/10/02/4529.html>
compose <- function (g, f)
    function (...) g(f(...))

# Dot operator (as in Haskell)
`%.%` <- compose

# Function chaining operator (as in F#)
`%|>%` <- function (g, f) compose(f, g)

# Pipe operator modified after idea from Robert Sugar, e.g. at
# <http://markmail.org/thread/uygwsdulfvxlydlh>
`%|%` <- function (x, y)
    let(thecall = match.call(),
        if (is.name(thecall$y) || is.function(thecall$y))
            y(x)
        else
            eval(thecall$y,
                 list(value = eval(thecall$x, env = parent.frame()))))

# }}}

# Higher-order list functions {{{

# Applies a list of functions to the same argument.
fapply <- function (x, ...)
    lapply(list(...), function (f) f(x))

# What is up with the naming of these (standard R) functions?

map <- base::Map

reduce <- base::Reduce

# Hides `stats::filter` but I don't care.
filter <- base::Filter

groupby <- function (data, cond, FUN = sum) {
    if (! is.list(cond))
        cond <- list(cond)
    result <- aggregate(data, by = cond, FUN)
    rownames(result) <- result[[1]]
    result[[1]] <- NULL
    result
}

# }}}

# Helpers for working with ranges {{{

# TODO Handle negative indices?
boolmask <- function (indices, length)
    is.element(1 : length, indices)

# Again, where does this naming come from?

indices <- seq_along

# Conditionally count elements.
count <- length %.% which

# Wrapper around `order` that returns the ordered data rather than the index
# permutation. Like `sort`, but allows specifying multiple sort keys.
sorted <- function (data, ..., decreasing = FALSE)
    let(key = if (length(list(...)) == 0) colnames(data) else list(...),
        data[do.call(order, c(lapply(key, lp(`[[`, data)), decreasing = decreasing)), ])

# }}}

# Creates an item selector function for a given item
item <- lp(p, `[[`)

items <- lp(p, `[`)

# Negates a function. Similar to `base::Negate`.
neg <- function (f) `!` %.% f

# Creates a lazy value retrieval function. `.(x)() == x`.
# The retrieval function swallows all its arguments.
. <- function (x) function (...) x
