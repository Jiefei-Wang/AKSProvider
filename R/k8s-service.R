updateService <- function(k8sCluster, serviceName, port){
    ymlPath <- getYmlPath("service")
    yml <- read_yaml(ymlPath)
    yml$metadata$name <- serviceName
    yml$spec$ports[[1]]$port <- port
    yml$spec$ports[[1]]$targetPort <- port
    k8sCluster$apply(saveYmlFile(yml))
}

listService <- function(k8sCluster){
    capture.output(output <- k8sCluster$get("service"),file='NUL')
    if(output$stdout!=""){
        read.table(text=output$stdout,header = T)
    }else{
        NULL
    }
}

configService <- function(provider, port){
    k8sCluster <- provider$k8sCluster
    serviceName <- provider$serviceName
    updateService(k8sCluster, serviceName, port)
    serviceName
}

getServiceIp <- function(provider){
    k8sCluster <- provider$k8sCluster
    serviceName <- provider$serviceName
    serviceList <- listService(k8sCluster)
    idx <- which(serviceList$NAME==serviceName)
    if(length(idx)==0){
        stop("Cannot find the service <", serviceName,">")
    }
    data.frame(publicIp = serviceList$EXTERNAL.IP[idx],
               privateIp = serviceList$CLUSTER.IP[idx])

}
