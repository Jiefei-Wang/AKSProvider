deleteAKSCluster <- function(AKS, verbose = 1){
    out <- capture.output(AKS$delete(confirm = FALSE, wait = FALSE))
    verbosePrint(verbose>0, out)
}
