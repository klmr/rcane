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

#' @TODO Make vectorised
readable <- capitalize %.% lp(gsub, '_|-', ' ')

#' Like \code{switch}, with regular expressions.
#'
#' Replacement arguments can be either character strings which are used as the
#' \code{replacement} argument to \code{sub}', or they can be functions taking
#' a single character string and returning one.
regswitch <- function (str, ..., FULL.MATCH = TRUE) {
    alternatives <- list(...)
    if (FULL.MATCH)
        names(alternatives) <- sprintf('(?:^%s)$', names(alternatives))

    tryPatterns <- function (str) {
        for (p in names(alternatives)) {
            m <- regexpr(p, str)
            if (m[1] != -1) {
                replacement <- alternatives[[p]]
                if(is.character(replacement))
                    return(sub(p, replacement, str))
                else
                    return(replacement(str))
            }
        }
        str
    }
    sapply(str, tryPatterns)
}
