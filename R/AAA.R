setClassUnion("CharOrNULL",c("NULL","character"))

#' The Azure kubenetes service provider
#'
#' The Azure kubenetes service provider
#'
.AKSProvider <- setRefClass(
    "AKSProvider",
    fields = list(
        serviceName = "character",
        serverDeploymentName = "character",
        workerDeploymentName = "character",
        azureClient = "ANY",
        subscription = "ANY",
        resourceGroup = "ANY",
        aks = "ANY",
        k8sCluster = "ANY",
        aksName = "ANY",
        resourceGroupName = "ANY",
        subscriptionName = "ANY",
        tenantID = "ANY",
        location = "ANY",
        initialized = "logical"
    ),
    contains = "CloudProvider"
)
