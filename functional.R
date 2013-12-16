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

ppartial <- function (f, arg, ...)
    let(capture = list(...), arg = as.character(substitute(arg)),
        function (x) do.call(f, c(setNames(x, arg), capture)))


# Define shortcuts because these functions are so commonly used and constitute
# syntactic noise.
# Not something I would normally do but there's precedence in R; consider `c`.

p <- partial
lp <- lpartial
pp <- ppartial

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
`%|%` <- function (x, y) y(x)

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

#' Like \code{c}, for dictionaries (\code{list}s with names).
#'
#' @examples
#' cdict(list(a=1, b=NULL), list(a=NULL, b=2), list(c=3)) # list(a=1, b=2, c=3)
cdict <- function (...) {
    lists <- list(...)
    names <- reduce(union, map(names, lists))

    nonnull <- function (n, a, b) if (is.null(a[[n]])) b[[n]] else a[[n]]
    reduce(function (a, b) map(function (n) nonnull(n, a, b), names), lists)
}

# }}}

# Creates an item selector function for a given item
item <- lp(p, `[[`)

items <- lp(p, `[`)

# Negates a function. Similar to `base::Negate`.
neg <- function (f) `!` %.% f

#' @TODO Add %or% and %and% analogously

#' Corresponds to the null-coalesce operator \code{??} in C#
`%else%` <- function (a, b)
    if(is.null(a) || is.na(a) || is.nan(a) || length(a) == 0) b else a

# Creates a lazy value retrieval function. `.(x)() == x`.
# The retrieval function swallows all its arguments.
. <- function (x) function (...) x

#' @TODO Implement argument name matching
#' @TODO Implement `...`
#' @TODO Rename to `.` (will change `.`’s semantics!)
fun <- function (...) {
    args <- match.call(expand.dots = FALSE)$...
    last <- length(args)
    params <- c(args[-last], names(args)[[last]])
    if (length(args) > 1 && length(params) != length(args))
        stop('Must be of the form `fun(a, b = expr)`')
    for (arg in args[-last])
        if (! is.name(arg))
            stop('Invalid argument specifier: ', arg)

    enclos <- parent.frame()

    function (...) {
        dots <- list(...)
        if (length(dots) < length(params))
            stop('Argument(s) missing')
        else if (length(dots) > length(params))
            stop('Unused argument(s)')
        # Match the order of arguments if the caller specified arg names, to
        # allow the following code:
        #   fun(x, y = x - y)(y = 1, x = 2) # => 1
        dots <- if (is.null(names(dots))) dots else {
            matched <- dots[match(params, names(dots))]
            matched <- matched[! is.na(names(matched))]
            unmatched <- dots[names(dots) == '']
            c(matched, unmatched)
        }
        eval(args[[length(args)]], setNames(dots, params), enclos)
    }
}
