getServiceName <- function(cluster){
    tolower(paste0(.getJobQueueName(cluster), "-service"))
}

updateService <- function(cluster, verbose = 1L){
    provider <- .getCloudProvider(cluster)
    k8sCluster <- getK8sCluster(provider)
    ymlPath <- getYmlPath("service")
    yml <- read_yaml(ymlPath)
    yml$metadata$name <- getServiceName(cluster)
    yml$spec$ports[[1]]$port <- .getServerPort(cluster)
    yml$spec$ports[[1]]$targetPort <- .getServerPort(cluster)
    out <- capture.output(k8sCluster$apply(saveYmlFile(yml)))
    verbosePrint(verbose > 1, out)
}


getServiceIp <- function(cluster){
    provider <- .getCloudProvider(cluster)
    k8sCluster <- getK8sCluster(provider)
    serviceName <- getServiceName(cluster)
    ## Sometimes we should be patient...
    startTime <- Sys.time()
    repeat{
        service <- k8sGetDF(k8sCluster, paste0("service/", serviceName))
        if(length(service)!=0){
            break
        }
        if(difftime(Sys.time(), startTime, units = "sec") > 10){
            stop("Cannot find the service <", serviceName,">")
        }
    }
    data.frame(publicIp = service$EXTERNAL.IP,
               publicPort = .getServerPort(cluster),
               privateIp = service$CLUSTER.IP,
               privatePort = .getServerPort(cluster))

}
