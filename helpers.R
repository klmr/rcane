deseqNormalize <- function (x) {
    require(DESeq)
    conditionMap <- colnames(x)
    cds <- newCountDataSet(x, conditionMap)
    cds <- estimateSizeFactors(cds)
    counts(cds, normalized = TRUE)
}

scaleNormalize <- function (x) x / colSums(x)

quantileNormalize <- function (x) {
    ord <- apply(x, COLS, order)
    ranks <- apply(x, COLS, partial(rank, ties.method = 'min'))
    normalized <- rowMeans(sapply(1 : ncol(x), function (i) x[ord[, i], i]))
    result <- apply(ranks, 2, function (r) normalized[r])
    rownames(result) <- rownames(x)
    result
}

rpkm <- function (x) log10(x * 1000000)
lg <- function (x) log2(x)
