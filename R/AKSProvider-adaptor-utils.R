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
        if(is.null(total)){
            total <- 0
        }
        list(initializing = total - running, running = running)
    }
}

deleteK8sCluster <- function(cluster, ask = TRUE){
    provider <- .getCloudProvider(cluster)
    aks <- .getAKS(provider)
    if(!is.null(aks)){
        if(ask){
            answer <- menu(c("Yes", "No"), title = "Do you want to delete the k8s cluster? This operation cannot be undone!")
            if(answer != 1){
                return()
            }
        }
        cluster$stopCluster()
        deleteAKSCluster(aks)
        .setAKS(provider, NULL)
        .setK8sCluster(provider, NULL)
        .setAKSName(provider, character(0))
        .setInitialized(provider, FALSE)
    }else{
        stop("The k8s cluster does not exist or the cluster has not been initialized.")
    }
}


K8sGet <- function(cluster, ...){
    provider <- .getCloudProvider(cluster)
    k8sCluster <- .getK8sCluster(provider)
    if(!is.null(k8sCluster)){
        k8sCluster$get(...)
    }else{
        stop("The k8s cluster does not exist or the cluster has not been initialized.")
    }
}


