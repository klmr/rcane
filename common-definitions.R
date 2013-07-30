# Common functions for the tRNA project.

library(RColorBrewer)

source('basic.R')
source('helpers.R')

conditions <- c("brain_e15.5","brain_e18.5","brain_P0.5","brain_P4","brain_P22","brain_P29",
                "liver_e15.5","liver_e18.5","liver_P0.5","liver_P4","liver_P22","liver_P29")
conditionsBrain <- grep('brain', conditions)
conditionsLiver <- grep('liver', conditions)

tissues <- c(liver = 'liver', brain = 'brain')
stages <- c('e15.5', 'e18.5', 'P0.5', 'P4', 'P22', 'P29')
methods <- c('scale', 'de', 'q')

colors <- colorRampPalette(brewer.pal(6, 'Dark2'))(10)
# !!! The order of the colours is important, must reflect `tissues`.
tissueColor <- c('liver' = colors[length(colors)], 'brain' = colors[1])

# Common functions {{{
groupConditions <- function (geneCounts, conditions, mapping) {
    pcgc <- matrix(nrow = nrow(geneCounts),
                   ncol = length(conditions))

    rownames(pcgc) <- rownames(geneCounts)
    colnames(pcgc) <- conditions

    for (cond in conditions) {
        replicates <- rownames(mapping[mapping$Condition == cond, ])
        pcgc[, cond] <- if (length(replicates) > 1)
            rowMeans(geneCounts[, replicates]) else geneCounts[, replicates]
    }
    as.data.frame(pcgc)
}

normalize <- function (rawGeneCounts, conditionMap, mapping, method) {
    normalizedGeneCounts <- method(rawGeneCounts)
    conditions <- unique(conditionMap)
    groupConditions(normalizedGeneCounts, conditions, mapping)
}

# FIXME no longer used?
# Return IDs for a given criterion; example:
#
#   map(Tissue = brain, Stage = e15.5)
map <- function(...) {
    conds <- match.call(expand.dots = FALSE)$`...`
    names(conds) <- paste('mrnaMapping', names(conds), sep='$')
    conds <- paste(names(conds), gsub('(.*)', '"\\1"', conds),
                   sep = ' == ')
    conds <- paste(conds, collapse = ' & ')
    rownames(mrnaMapping[eval(parse(text = conds)), ])
}

loadAminoAcids <- function () {
    aminoAcidPath <- '../chip/data/amino_acids.tsv'
    aminoAcids <<- read.table(aminoAcidPath,
                              col.names = c('Long', 'Short'),
                              stringsAsFactors = FALSE)
}

loadGeneticCode <- function () {
    geneticCodeFile <- '../rna/data/genetic_code.tsv'
    geneticCode <<- read.table(geneticCodeFile, row.names = 1,
                               col.names = c('', 'AA'),
                               stringsAsFactors = FALSE)
}
# }}}
