replicateImpCols <- function(m) {
    vals <- list()
    for(i in 1:nrow(m)) {
        ## check columns with ';'
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
            ## combine 'old' with replicated data
            tmp <- cbind(res, tmp)
        } else {
            tmp <- m[i,]
        }
        tmp <- tmp[colnames(m)]
        vals[[i]] <- tmp
    }
    vals <- do.call("rbind", vals)    
    ## ready
    return(invisible(vals))
}
