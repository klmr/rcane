# Functional tools

id <- function (x) x

# This uses R's peculiarities in argument matching explained here:
# <http://stat.ethz.ch/R-manual/R-devel/doc/manual/R-lang.html#Argument-matching>
# `.expr` starts with a dot to allow `expr` being used in the actual
# expression.
let <- function (.expr, ...)
    eval(substitute(.expr), list2env(list(...), parent = parent.frame()))

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

`%.%` <- compose

# Pipe operator modified after idea from Robert Sugar, e.g. at
# <http://markmail.org/thread/uygwsdulfvxlydlh>
`%|%` <- function (x, y)
    let(thecall = match.call(),
        if (is.name(thecall$y) || is.function(thecall$y))
            y(x)
        else
            eval(thecall$y,
                 list(value = eval(thecall$x, env = parent.frame()))))

slice <- function (x, ...)
    x[...]

# TODO Handle negative indices?
boolMask <- function (indices, length)
    is.element(1 : length, indices)
