#' Initialize the provider
#'
#' Initialize the provider
#'
#' @inheritParams DockerParallel::initializeCloudProvider
#'
#' @return No return value
#' @export
setMethod("initializeCloudProvider", "AKSProvider", function(provider, cluster, verbose){
    if(!.getInitialized(provider)){
        k8sCluster <- .getK8sCluster(provider)
        if(is.empty(k8sCluster)){
            initializeAzure(provider, verbose = verbose)
        }
        updateService(cluster)
        .setInitialized(provider, TRUE)
    }
    invisible(NULL)
})



#' Run the server container
#'
#' Run the server container. This will set the environment variable `AKSClusterStaticData` to
#' the server container
#'
#' @inheritParams DockerParallel::runDockerServer
#'
#' @return No return value
#' @export
setMethod("runDockerServer", "AKSProvider",
          function(provider, cluster, container, hardware, verbose = 0L){
              existsServer <- dockerClusterExists(provider = provider,
                                                  cluster = cluster,
                                                  verbose = verbose)
              if(existsServer){
                  stop("Cannot create a cluster with the same job queue name <",
                       .getJobQueueName(cluster),
                       ">")
              }

              container <- container$copy()
              container$environment["AKSClusterStaticData"] <- encodeClusterStaticData(cluster)


              verbosePrint(verbose>0, "Deploying server container")
              updateServer(cluster,
                           container,
                           hardware,
                           verbose = verbose)
          })

#' Stop the server
#'
#' Stop the server
#'
#' @inheritParams DockerParallel::stopDockerServer
#'
#' @return No return value
#' @export
setMethod("stopDockerServer", "AKSProvider",
          function(provider, cluster, verbose = 0L){
              deleteDeployments(getK8sCluster(provider),
                                getServerDeploymentName(cluster),
                                verbose = verbose)
          }
)


setMethod("getServerStatus", "AKSProvider",
          function(provider, cluster, verbose){
              status <- getContainerStatus(cluster,
                                           getServerDeploymentName(cluster))
              if(status$running == 1){
                  "running"
              }else{
                  "initializing"
              }
          })

#' Get the instance public/private IPs
#'
#' Get the instance public/private IPs
#'
#' @inheritParams DockerParallel::getDockerServerIp
#'
#' @returns
#' A list with the element name `publicIp`, `publicPort`, `privateIp` and `privatePort`
#' @export
setMethod("getDockerServerIp", "AKSProvider",
          function(provider, cluster, verbose = 0L){
              repeat{
                  ip <- getServiceIp(cluster)
                  if(ip$publicIp!="<pending>"){
                      break
                  }
                  Sys.sleep(0.5)
              }
              ip
          }
)

#' Run the worker containers
#'
#' Run the worker containers
#'
#' @inheritParams DockerParallel::setDockerWorkerNumber
#'
#' @return No return value
#' @export
setMethod("setDockerWorkerNumber", "AKSProvider",
          function(provider, cluster, container, hardware, workerNumber,
                   verbose = 0L){
              verbosePrint(verbose>0, "Adjusting worker container")
              updateWorker(cluster,
                           container,
                           hardware,
                           workerNumber,
                           verbose = verbose)
          }
)

#' Get the worker number
#'
#' Get the worker number
#'
#' @inheritParams DockerParallel::getDockerWorkerNumbers
#'
#' @return A list with running and initializing worker numbers
#' @export
setMethod("getDockerWorkerNumbers", "AKSProvider",
          function(provider, cluster, verbose = 0L){
              if(!.getInitialized(provider)){
                  return(list(initializing = 0L, running = 0L))
              }
              status <- getContainerStatus(cluster,
                                           getWorkerDeploymentName(cluster))
              status
          }
)

#' Cleanup the cluster
#'
#' Cleanup the cluster
#'
#' @inheritParams DockerParallel::cleanupDockerCluster
#' @return No return value
#' @export
setMethod("cleanupDockerCluster", "AKSProvider",
          function(provider, cluster, verbose){
              k8sCluster <- .getK8sCluster(provider)
              if(!is.null(k8sCluster)){
                  autoDelete <- .getAutoDelete(provider)
                  deleteDeployments(k8sCluster,
                                    getServerDeploymentName(cluster),
                                    verbose = verbose)
                  deleteDeployments(k8sCluster,
                                    getWorkerDeploymentName(cluster),
                                    verbose = verbose)
                  if(!is.empty(autoDelete) && autoDelete){
                      deleteAKSCluster(.getAKS(provider))
                  }
                  .setK8sCluster(provider, NULL)
                  .setAKS(provider, NULL)
              }
          })



#' Whether the cluster exists
#'
#' This function checks whether the cluster with the same job queue name
#' exists on the cloud.
#'
#' @inheritParams DockerParallel::dockerClusterExists
#' @return logical(1)
#' @export
setMethod("dockerClusterExists", "AKSProvider", function(provider, cluster, verbose){
    k8sCluster <- getK8sCluster(provider)
    deployments <- getAllDeployments(k8sCluster)
    serverDeploymentName <- getServerDeploymentName(cluster)
    serverDeploymentName %in% deployments
})

#' Reconnect to the cluster
#'
#' Reconnect to the cluster with the same job queue name.
#'
#' @inheritParams DockerParallel::dockerClusterExists
#' @return No return value
#' @export
setMethod("reconnectDockerCluster", "AKSProvider", function(provider, cluster, verbose){
    k8sCluster <- getK8sCluster(provider)

    ## Recover the cluster data from the server environment
    serverDeploymentName <- getServerDeploymentName(cluster)
    json <- k8sGetJson(k8sCluster, paste0("deployment ",serverDeploymentName),  options = "-o json")
    jsonEnv <- json$spec$template$spec$containers$env[[1]]
    idx <- which(jsonEnv$name == "AKSClusterStaticData")
    staticData <- decodeClusterStaticData(jsonEnv$value[idx])
    setDockerStaticData(cluster, staticData)
})

#' Get the extra function defined for the AKSProvider
#'
#' Get the extra function defined for the AKSProvider
#'
#' @inheritParams DockerParallel::getExportedNames
#' @return A character vector
#' @export
setMethod("getExportedNames", "AKSProvider", function(x){
    c("deleteK8sCluster", "k8sGet")
})

#' Get the extra function defined for the AKSProvider
#'
#' Get the extra function defined for the AKSProvider
#'
#' @inheritParams DockerParallel::getExportedObject
#' @return A function
#' @export
setMethod("getExportedObject", "AKSProvider", function(x, name){
    if(name == "deleteK8sCluster"){
        return(deleteK8sCluster)
    }
    if(name == "k8sGet"){
        return(K8sGet)
    }
    stop("The name <", name,"> is not defined for the AKSProvider")
})



