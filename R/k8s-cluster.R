listDeployments <- function(k8sCluster){
    capture.output(output <- k8sCluster$get("deployment"),file='NUL')
    if(output$stdout!=""){
        read.table(text=output$stdout,header = T)
    }else{
        NULL
    }
}
deleteDeployments <- function(k8sCluster, deploymentName){
    deployments <- listDeployments(k8sCluster)
    if(deploymentName %in% deployments$NAME){
        k8sCluster$delete("deployment", deploymentName)
    }
}

updateServer <- function(k8sCluster, serverName, serverContainer,hardware){
    ymlPath <- getYmlPath("cluster-server")
    yml <- read_yaml(ymlPath)
    yml$metadata$name <- serverName
    yml$spec$template$spec$containers[[1]]$env <- listToK8sEnv(serverContainer$environment)
    yml$spec$template$spec$containers[[1]]$name <- tolower(serverContainer$name)
    yml$spec$template$spec$containers[[1]]$image <- serverContainer$image
    k8sCluster$apply(saveYmlFile(yml))
}


updateWorker <- function(k8sCluster, workerName, workerContainer,hardware, workerNumber){
        ymlPath <- getYmlPath("cluster-worker")
        yml <- read_yaml(ymlPath)
        yml$metadata$name <- workerName
        yml$spec$replicas <- workerNumber
        yml$spec$template$spec$containers[[1]]$env <- listToK8sEnv(workerContainer$environment)
        yml$spec$template$spec$containers[[1]]$name <- tolower(workerContainer$name)
        yml$spec$template$spec$containers[[1]]$image <- workerContainer$image
        k8sCluster$apply(saveYmlFile(yml))
}

scaleWorker <- function(k8sCluster, workerName, workerNumber){
    deployments <- listDeployments(k8sCluster)
    if(workerName %in% deployments$NAME){
        cmd <- paste0("scale deployments/",workerName," --replicas=", workerNumber)
        k8sCluster$kubectl(cmd)
    }
}
