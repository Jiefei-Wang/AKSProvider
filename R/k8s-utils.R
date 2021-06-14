k8sGetRaw <- function(k8sCluster, ...){
    output <-
        tryCatch({
            capture.output(output <- k8sCluster$get(...),file='NUL')
            output
        },
        error = function(e) e
        )

    if(!is(output, "error") && output$stdout!=""){
        output$stdout
    }else{
        NULL
    }
}
k8sGetJson <- function(k8sCluster, ...){
    raw <- k8sGetRaw(k8sCluster, ...)
    if(!is.null(raw)){
        jsonlite::fromJSON(raw)
    }else{
        NULL
    }
}

k8sGetDF <- function(k8sCluster, ...){
    raw <- k8sGetRaw(k8sCluster, ...)
    if(!is.null(raw)){
        read.table(text = raw, header = T)
    }else{
        NULL
    }
}

listDeployments <- function(k8sCluster){
    k8sGetDF(k8sCluster, "deployment")
}
deleteDeployments <- function(k8sCluster, deploymentName){
    deployments <- listDeployments(k8sCluster)
    if(deploymentName %in% deployments$NAME){
        k8sCluster$delete("deployment", deploymentName)
    }
}
listService <- function(k8sCluster){
    k8sGetDF(k8sCluster, "service")
}
listReplicaSets <- function(k8sCluster){
    k8sGetDF(k8sCluster, "rs")
}





