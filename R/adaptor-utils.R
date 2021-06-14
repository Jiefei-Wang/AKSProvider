getContainerStatus <- function(cluster, deploymentName){
    provider <- .getCloudProvider(cluster)
    k8sCluster <- getK8sCluster(provider)
    status <-
        k8sGetJson(k8sCluster,
                   paste0("deployment/", deploymentName), options = "-o json")
    if(is.null(status)){
        list(initializing = 0L, running = 0L)
    }else{
        running <- status$status$readyReplicas
        if(is.null(running)){
            running <- 0
        }
        total <- status$status$replicas
        list(initializing = total - running, running = running)
    }
}
