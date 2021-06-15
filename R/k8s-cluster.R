deleteK8sCluster <- function(provider, verbose = 1){
    aks <- .getAKS(provider)
    out <- capture.output(aks$delete(confirm = FALSE, wait = FALSE))
    verbosePrint(verbose>0, out)
}
