replicateImpCols <- function(m) {
    ## check columns with ';'
    vals <- data.frame()
    for(i in 1:nrow(m)) {
        cols <- sapply(m[i,], function(x) {
            res <- all(grepl(";", x))
        })
        if(sum(cols)>0) {
            ## replicate cols with ';'
            res <- do.call("cbind", sapply(m[i, cols], function(x) {
                strsplit(x, ";")
            }))
            colnames(res) <- colnames(m)[cols]
            ## replicate "old" data
            tmp <- as.data.frame(lapply(m[i,!cols], rep, nrow(res)))
            tmp <- cbind(res, tmp)
            tmp <- tmp[colnames(m)]
        } else {
            tmp <- m[i,]
            tmp <- tmp[colnames(m)]
        }
        ## combine 'old' with replicated data
        vals <- rbind(vals, tmp)
    }
    ## ready
    return(invisible(vals))
}
