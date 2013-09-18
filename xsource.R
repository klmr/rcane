xsource <- function (module) {
    module <- as.character(substitute(module))
    # Prepend '' to ensure that at least one path component exists, otherwise
    # `file.path` will subsequently return an empty vector instead of ''.
    parts <- c('', unlist(strsplit(module, '\\.')))

    # Use all-but-last parts to construct module source path, last part to
    # determine name of source file.
    modulePath <- do.call(file.path, as.list(parts[-length(parts)]))
    filePattern <- sprintf('%s\\.[rR]', parts[length(parts)])

    # The search paths are ordered from highest to lowest priority.
    searchPath <- c('.', getOption('xsourcePath'))
    candidatePaths <- file.path(searchPath, modulePath)

    # `list.files` accepts multiple input paths, but it sorts the returned files
    # alphabetically, and we thus lose information about the priority.
    hits <- unlist(sapply(candidatePaths, function (p)
                          list.files(p, filePattern, full.names = TRUE)))

    if (length(hits) == 0)
        stop('Unable to load module ', module, '; not found in ',
             paste(Map(function (p) sprintf('"%s"', p), searchPath), collapse = ', '))

    source(file.path(basepath, file), chdir = TRUE)
}
