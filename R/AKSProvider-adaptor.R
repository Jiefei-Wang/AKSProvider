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
        initializeAzure(provider, verbose = verbose)
        updateService(cluster)
        .setInitialized(provider, TRUE)
    }
    invisible(NULL)
})



#' Run the server container
#'
#' Run the server container
#'
#' @inheritParams DockerParallel::runDockerServer
#'
#' @return No return value
#' @export
setMethod("runDockerServer", "AKSProvider",
          function(provider, cluster, container, hardware, verbose = 0L){
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


setMethod("getDockerWorkerNumbers", "AKSProvider",
          function(provider, cluster, verbose = 0L){
              status <- getContainerStatus(cluster,
                                           getWorkerDeploymentName(cluster))
              status
          }
)

#' Cleanup the cluster
#'
#' Cleanup the cluster
#'
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
                      deleteK8sCluster(provider)
                  }
              }

          })

