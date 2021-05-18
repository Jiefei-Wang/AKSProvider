#' Initialize the provider
#'
#' Initialize the provider
#'
#' @inheritParams DockerParallel::initializeProvider
#'
#' @return No return value
#' @export
setMethod("initializeProvider", "AKSProvider", function(provider, cluster, verbose){
    if(!provider$initialized){
        k8sCluster <- getCluster(provider)
        configService(provider, .getServerPort(cluster))
    }
    invisible(NULL)
})



#' Run the server container
#'
#' Run the server and return the server instance handle.
#' The function will set the environment variable `ECSFargateCloudConfigInfo` to the container
#'
#' @inheritParams DockerParallel::runDockerServer
#'
#' @return The server handle in character
#' @export
setMethod("runDockerServer", "AKSProvider",
          function(provider, cluster, container, hardware, verbose = 0L){
              verbosePrint(verbose>0, "Deploying server container")
              updateServer(provider$k8sCluster,
                           provider$serverDeploymentName,
                           container,
                           hardware)
              provider$serverDeploymentName
          })


#' Run the worker container
#'
#' Run the workers and return a list of worker instance handles.
#' The function will set the environment variable `ECSFargateCloudJobQueueName`,
#' `ECSFargateCloudServerIP` and `ECSFargateCloudWorkerNumber` to the container
#'
#' @inheritParams DockerParallel::runDockerWorkers
#'
#' @return A list of worker handle in character
#' @export
setMethod("setDockerWorkerNumber", "AKSProvider",
          function(provider, cluster, container, hardware, workerNumber,
                   verbose = 0L){
              verbosePrint(verbose>0, "Adjusting worker container")
              if(workerNumber==0){
                  deleteDeployments(provider$k8sCluster,
                                    provider$workerDeploymentName)
              }else{
                  updateWorker(provider$k8sCluster,
                               provider$workerDeploymentName,
                               container,
                               hardware,
                               workerNumber)
              }
          }
)
#'
#' Get the instance public/private IPs
#'
#' Get the instance public/private IPs
#'
#' @inheritParams DockerParallel::getDockerInstanceIps
#'
#' @returns
#' A data.frame with publicIp and privateIp columns and each row corresponds to
#' an element in instanceHandles.
#' @export
setMethod("getDockerInstanceIps", "AKSProvider",
          function(provider, instanceHandles, verbose = 0L){
              idx <- vapply(instanceHandles,
                            function(x)x == provider$serverDeploymentName,
                            logical(1))
              if(!all(idx)){
                  stop("Only the server IP is obtainable")
              }
              ip <- getServiceIp(provider)
              ip
          }
)
#'
#' #' Get the instance status
#' #'
#' #' Get the instance status
#' #'
#' #' @inheritParams DockerParallel::getDockerInstanceStatus
#' #'
#' #' @returns
#' #' A character vector with each element corresponding to an instance in instanceHandles.
#' #' Each element must be one of three possible characters "initializing", "running" or "stopped"
#' #' @export
#' setMethod("getDockerInstanceStatus", "AKSProvider",
#'           function(provider, instanceHandles, verbose = 0L){
#'               uniqueHandles <- unique(instanceHandles)
#'               taskInfo <- getTaskDetails(provider$clusterName, taskIds = uniqueHandles)
#'               instanceStatus <- rep("initializing", length(uniqueHandles))
#'               instanceStatus[taskInfo$status == "RUNNING"] <- "running"
#'               instanceStatus[taskInfo$status == "STOPPED"] <- "stopped"
#'               result <- rep("", length(instanceHandles))
#'               for(i in seq_along(uniqueHandles)){
#'                   idx <- vapply(instanceHandles, function(x) identical(x, uniqueHandles[[i]]),logical(1))
#'                   result[idx] <- instanceStatus[i]
#'               }
#'               result
#'           }
#' )
#'
#' Kill the instances
#'
#' Kill the instances
#'
#' @inheritParams DockerParallel::killDockerInstances
#'
#' @return A logical vector indicating whether the killing operation is success for each instance
#' @export
setMethod("killDockerInstances", "AKSProvider",
          function(provider, instanceHandles, verbose = 0L){
              serverName <- provider$serverDeploymentName
              if(serverName%in%instanceHandles){
                  deleteDeployments(provider$k8sCluster, serverName)
              }
              rep(TRUE, length(instanceHandles))
          }
)
#'
#'
#' #' Whether the cluster is running on the cloud?
#' #'
#' #' Whether the cluster is running on the cloud?
#' #' This function will check the instances in the ECS cluster defined by
#' #' `provider$clusterName` and find if there is any instance that has the same job queue
#' #' name as `.getJobQueueName(cluster)`
#' #'
#' #' @inheritParams DockerParallel::dockerClusterExists
#' #'
#' #' @return A logical value
#' #' @export
#' setMethod("dockerClusterExists", "AKSProvider",
#'           function(provider, cluster, verbose = 0L){
#'               ## Check if the cluster exists
#'               clusterList <- listClusters()
#'               if(!provider$clusterName%in%clusterList){
#'                   return(FALSE)
#'               }
#'
#'               ## Check if the server exists
#'               serverHandles <- listRunningServer(cluster)
#'               serverInfo <- findServerInfo(cluster, serverHandles)
#'               !is.null(serverInfo)
#'           }
#' )
#'
#'
#' #' Reconnect the cluster
#' #'
#' #' Reconnect the cluster. This function will check the instances in
#' #' the ECS cluster defined by `provider$clusterName` and find if there is any
#' #' instance that has the same job queue name as `.getJobQueueName(cluster)`. If
#' #' so, the function can reconnect the cluster.
#' #'
#' #' @inheritParams DockerParallel::reconnectDockerCluster
#' #'
#' #' @return No return value
#' #' @export
#' setMethod("reconnectDockerCluster", "AKSProvider",
#'           function(provider, cluster, verbose = 0L){
#'               cloudConfig <- .getCloudConfig(cluster)
#'               serverHandles <- listRunningServer(cluster)
#'               serverInfo <- findServerInfo(cluster, serverHandles)
#'               if(!is.null(serverInfo)){
#'                   ## Set the cloud config
#'                   serverHandle <- serverInfo$handle
#'                   cloudConfigValue <- serverInfo$cloudConfigValue
#'                   lapply(names(cloudConfigValue), function(i)
#'                       cloudConfig$field(i, cloudConfigValue[[i]]))
#'
#'                   ## Set the server runtime
#'                   serverDetails <-
#'                       getTaskDetails(provider$clusterName,taskIds = serverHandle, getIP = TRUE)
#'                   .setServerHandle(cluster, serverHandle)
#'                   .setServerPublicIp(cluster, serverDetails$publicIp)
#'                   .setServerPrivateIp(cluster, serverDetails$privateIp)
#'
#'                   ## Set the worker runtime
#'                   workerHandles <- findWorkerHandles(cluster, cloudConfigValue$jobQueueName)
#'                   .addWorkerHandles(cluster, workerHandles)
#'                   .setWorkerNumber(cluster, length(workerHandles))
#'               }
#'           }
#' )
#'
#'
#'
