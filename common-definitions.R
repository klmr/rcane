library(RColorBrewer)

source('basic.R')
source('helpers.R')

conditions <- c("brain_e15.5","brain_e18.5","brain_P0.5","brain_P4","brain_P22","brain_P29",
                "liver_e15.5","liver_e18.5","liver_P0.5","liver_P4","liver_P22","liver_P29")
conditionsBrain <- grep('brain', conditions)
conditionsLiver <- grep('liver', conditions)

tissues <- c(liver = 'liver', brain = 'brain')
stages <- c('e15.5', 'e18.5', 'P0.5', 'P4', 'P22', 'P29')
names(stages) <- stages
methods <- c('scale', 'de', 'q')

colors <- brewer.pal(8, 'Dark2')
# !!! The order of these colours is important, must reflect `tissues`.
tissueColor <- c(liver = '#D01B24', brain = '#E6AB02')

plotFamily <- 'Helvetica'

source('plot-matrix.R')

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

loadAminoAcids <- function () {
    aminoAcidPath <- '../common/data/amino_acids.tsv'
    aminoAcids <<- read.table(aminoAcidPath,
                              col.names = c('Long', 'Short'),
                              stringsAsFactors = FALSE)
}

loadGeneticCode <- function () {
    geneticCodeFile <- '../common/data/genetic_code.tsv'
    geneticCode <<- read.table(geneticCodeFile, row.names = 1,
                               col.names = c('', 'AA'),
                               stringsAsFactors = FALSE)
}

# FIXME Remove the TeXy hack once we switch to modules.
oldReadable <- readable
readable <- function (str)
    paste(regswitch(unlist(strsplit(oldReadable(str), ' ')),
                          e15.5 = 'E15.5',
                          e18.5 = 'E18.5',
                          '[dD]o(\\d{4})' = 'DO\\1'),
          collapse = ' ')
# }}}
