syscmd <- function (cmd, ...)
    system(paste(cmd, ...))

readcmd <- function (cmd, ...) {
    on.exit(close(con))
    con <- pipe(paste(cmd, ...))
    readLines(con)
}

# Making this work would be awesome but it requires dynamically generating
# functions. Well, actually it would suffice to return a `function (...)` but I
# hesitate to do this because it makes using the function harder (no `args` ...)
#mkdir <- syscmd('mkdir', flags = '-p', filename)
mkdir <- function (what, flags = '-p')
    syscmd('mkdir', flags, what)

#' Silences all output from an expression by redirecting the sink
silent <- function (.expr) {
    on.exit(sink())
    sink(file = (if (Sys.info()['sysname'] == 'Windows') 'NUL' else '/dev/null'))
    eval(.expr, envir = parent.frame())
}

progress <- function (x, max = 100) {
    percent <- x / max * 100
    cat(sprintf('\r[%-50s] %d%%',
                paste(rep('=', percent / 2), collapse = ''),
                floor(percent)))
    if (x == max)
        cat('\n')
}

#' @TODO Integrate with functional … re-think organisation!
map_with_progress <- function (f, ...) {
    max_progress <- max(mapply(length, list(...)))
    progress(0, max_progress)
    invisible(Map(function (i, ...) {
        on.exit(progress(i, max_progress))
        f(...)
    }, 1 : max_progress, ...))
}
