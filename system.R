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
