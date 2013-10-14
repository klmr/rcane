#' @TODO Only import files once; check whether module already present in search path
#' @TODO Add a way of unloading/reloading a module
#' @TODO Add explicit relative imports (e.g. \code{xsource(.basics)})

#' Load an R code module
#'
#' A code module is an R source file ending in \code{.R} or \code{.r}.
#' Unlike for \code{source}, the user doesn't specify a character vector
#' containing a file path or connection but an identifier naming the module.
#' Modules may be nested by separating them by \code{.}.
#'
#' @note If a nested module name is given, then that nested name must correspond
#' to a relative path in the file system, rooted at some point in the module
#' search path. \code{xsource(foo.bar)} will search for a file called
#' \code{foo/bar.R} somewhere in the search path.
#'
#' Modules are loaded by searching for them linearly in the search path vector,
#' which may be given via \code{option('xsourcePath')}. However, the current
#' working directory is always searched first. Consequently, if there is a file
#' \code{foo.R} in the local directory, \code{xsource(foo)} will load that file
#' rather than any other found in the search path.
#'
#' The module code is loaded and executed into a new environment. If the
#' \code{attach} argument is given, the environment is subsequently attached to
#' the R object search path. It is also returned to the caller.
#'
#' The returned environment overloads the \code{::} operator, and thus acts
#' similar to a package namespace in usage.
#'
#' Internally, the module file may \code{source} other source files. If their
#' path is given as relative, it's relative to the module's own path.
#'
#' @param module an identifier specifying the module to load.
#' @param attach whether to attach the loaded module to the R objects search
#'  path.
#' @return an (invisible) copy of the environment into which the module has
#'  been loaded.
xsource <- function (module, attach = TRUE) {
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

    xenv <- new.env(parent = globalenv())
    class(xenv) <- 'xsourceenv'
    xenv$.hits <- hits
    local(source(file.path(.hits[1]), chdir = TRUE, local = TRUE),
          envir = xenv)
    if (attach)
        base::attach(xenv, name = paste('xsource', module, sep = ':'))

    invisible(xenv)
}

`::.xsourceenv` <- function (module, name)
    module[[as.character(substitute(name))]]

`::` <- function (pkg, name) {
    pkgName <- as.character(substitute(pkg))
    if (exists(pkgName) && is(pkg, 'xsourceenv'))
        UseMethod('::')
    else
        getExportedValue(pkgName, as.character(substitute(name)))
}
