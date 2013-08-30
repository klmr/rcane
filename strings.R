# String helper functions

# Simple naming schema: all functions working on strings
# start with `str`. All functions working with a regular
# expression start with `rx` instead.

strlstrip <- lp(sub, '^ +', '')

strrstrip <- lp(sub, ' +$', '')

strstrip <- strlstrip %.% strrstrip

# FIXME Vectorize
strrev <- p(paste, collapse = '') %.% rev %.% item(1) %.% p(strsplit, '')

# FIXME This can be implemented in terms of `regmatches` somehow.
#' Return the first match of a regular expression search inside a string.
#'
#' The returned match is the matching substring. The matching is performed
#' as if done with \code{\link{regexpr}}.
rxmatch <- function (pattern, text) {
    positions <- regexpr(pattern, text)
    mapply(function (p, l, t) if (p == -1) NULL else substr(t, p, p + l - 1),
           positions,
           attr(positions, 'match.length'),
           text)
}

rxmatches <- function (pattern, text) {
    result <-
    mapply(function (positions, t)
               mapply(function (p, l)
                          if (p == -1) NULL else substr(t, p, p + l - 1),
                      positions,
                      attr(positions, 'match.length')),
           gregexpr(pattern, text),
           text)
    # Sanitise to get rid of redundant singleton lists of NULL
    lapply(result,
           function (x)
               if (is.null(x[[1]])) NULL else x)
}

capitalize <-
    p(fapply, toupper %.% p(substring, 1, 1), p(substring, 2)) %|>%
    lp(do.call, paste0)

readable <- capitalize %.% lp(gsub, '_|-', ' ')
