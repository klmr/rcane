# I/O helper functions

# FIXME Remove after refactoring is complete
path <- file.path

#' Augment \code{\link{utils::read.table}} by a mechanism to guess the file format.
#' For the moment, only separators are handled based on the file extension.
#' This might change in the future to be more powerful, think Python’s
#' \code{csv.Sniffer} class.
read.table <- function (file, ..., text) {
    args <- list(...)
    if (missing(file))
        return(do.call(utils::read.table, c(args, text = text)))

    if (! ('sep' %in% names(args))) {
        separators <- list('.csv' = ',',
                           '.tsv' = '\t')
        extension <- rxmatch('\\.(\\w+)$', file)
        args$sep <- separators[[extension]]
    }

    args$file <- file
    do.call(utils::read.table, args)
}
