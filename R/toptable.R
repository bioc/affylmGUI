topTable2 <- function (fit, coef = 1, number = 10, genelist = NULL, adjust.method = "holm", 
    sort.by = "B") 
{
    if (!missing(genelist)) 
        fit$genes <- genelist
    if ("t" %in% names(fit))
      toptable2(fit = fit[c("coefficients", "stdev.unscaled")], 
        coef = coef, number = number, genelist = fit$genes, A = fit$Amean, 
        eb = fit[c("t", "p.value", "lods")], adjust.method = adjust.method, 
        sort.by = sort.by)
    else
      toptable2(fit = fit[c("coefficients", "stdev.unscaled")], 
        coef = coef, number = number, genelist = fit$genes, A = fit$Amean, 
        adjust.method = adjust.method, 
        sort.by = sort.by)
}

toptable2 <- function (fit, coef = 1, number = 10, genelist = NULL, A = NULL, 
    eb = NULL, adjust.method = "holm", sort.by = "B", ...) 
{
    if (is.null(eb)) {
        fit$coefficients <- as.matrix(fit$coef)[, coef]
        fit$stdev.unscaled <- as.matrix(fit$stdev)[, coef]
        eb <- ebayes(fit, ...)
        coef <- 1
    }
    M <- as.matrix(fit$coef)[, coef]
    if (is.null(A)) {
        if (sort.by == "A") 
            stop("Cannot sort by A-values as these have not been given")
    }
    else {
        if (NCOL(A) > 1) 
            A <- rowMeans(A, na.rm = TRUE)
    }
    tstat <- as.matrix(eb$t)[, coef]
    P.Value <- as.matrix(eb$p)[, coef]
    B <- as.matrix(eb$lods)[, coef]
    ord <- switch(sort.by, M = order(abs(M), decreasing = TRUE), 
        A = order(A, decreasing = TRUE), P = order(P.Value, decreasing = FALSE), 
        p = order(P.Value, decreasing = FALSE), T = order(abs(tstat), 
            decreasing = TRUE), t = order(abs(tstat), decreasing = TRUE), 
        B = order(B, decreasing = TRUE), order(B, decreasing = TRUE))
    top <- ord[1:number]
    i <- is.na(P.Value)
    if (any(i)) 
        P.Value[!i] <- p.adjust(P.Value[!i], method = adjust.method)
    else P.Value <- p.adjust(P.Value, method = adjust.method)
    if (is.null(genelist)) 
        tab <- data.frame(M = M[top])
    else if (is.null(dim(genelist))) 
        tab <- data.frame(Name = I(genelist[top]), M = M[top])
    else tab <- data.frame(genelist[top, ,drop=FALSE], M = M[top])
    if (!is.null(A)) 
        tab <- data.frame(tab, A = A[top])
    tab <- data.frame(tab, t = tstat[top], P.Value = P.Value[top], 
        B = B[top])
    rownames(tab) <- as.character(1:length(M))[top]
    tab
}
