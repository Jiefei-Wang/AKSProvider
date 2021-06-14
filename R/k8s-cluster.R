getServerDeploymentName <- function(cluster){
    tolower(paste0(.getJobQueueName(cluster), "-server-deployment"))
}
getWorkerDeploymentName <- function(cluster){
    tolower(paste0(.getJobQueueName(cluster), "-worker-deployment"))
}

updateServer <- function(cluster, serverContainer,hardware){
    provider <- .getCloudProvider(cluster)
    k8sCluster <- getK8sCluster(provider)
    ymlPath <- getYmlPath("cluster-server")
    yml <- read_yaml(ymlPath)
    yml$metadata$name <- getServerDeploymentName(cluster)
    ## server hardware
    yml$spec$template$spec$containers[[1]]$resources$requests$cpu <-
        paste0(hardware@cpu,"m")
    yml$spec$template$spec$containers[[1]]$resources$requests$memory <-
        paste0(hardware@memory,"Mi")
    ## Server environment
    yml$spec$template$spec$containers[[1]]$env <- listToK8sEnv(serverContainer$environment)
    yml$spec$template$spec$containers[[1]]$name <- tolower(serverContainer$name)
    yml$spec$template$spec$containers[[1]]$image <- serverContainer$image
    k8sCluster$apply(saveYmlFile(yml))
}


updateWorker <- function(cluster, workerContainer,hardware, workerNumber){
    provider <- .getCloudProvider(cluster)
    k8sCluster <- getK8sCluster(provider)
    ymlPath <- getYmlPath("cluster-worker")
    yml <- read_yaml(ymlPath)
    yml$metadata$name <- getWorkerDeploymentName(cluster)
    ## Worker number
    yml$spec$replicas <- workerNumber
    ## Worker hardware
    yml$spec$template$spec$containers[[1]]$resources$requests$cpu <-
        paste0(hardware@cpu,"m")
    yml$spec$template$spec$containers[[1]]$resources$requests$memory <-
        paste0(hardware@memory,"Mi")
    yml$spec$template$spec$containers[[1]]$env <- listToK8sEnv(workerContainer$environment)
    yml$spec$template$spec$containers[[1]]$name <- tolower(workerContainer$name)
    yml$spec$template$spec$containers[[1]]$image <- workerContainer$image
    k8sCluster$apply(saveYmlFile(yml))
}

scaleWorker <- function(cluster, workerNumber){
    provider <- .getCloudProvider(cluster)
    k8sCluster <- getK8sCluster(provider)
    deployments <- listDeployments(k8sCluster)
    workerDeploymentName <- getWorkerDeploymentName(cluster)
    if(workerDeploymentName %in% deployments$NAME){
        cmd <- paste0("scale deployments/",workerDeploymentName,
                      " --replicas=", workerNumber)
        k8sCluster$kubectl(cmd)
    }
}
