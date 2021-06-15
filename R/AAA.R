#' The Azure kubenetes service provider
#'
#' The Azure kubenetes service provider
#'
.AKSProvider <- setRefClass(
    "AKSProvider",
    fields = list(
        azureClient = "ANY",
        subscription = "ANY",
        resourceGroup = "ANY",
        AKS = "ANY",
        k8sCluster = "ANY",
        poolName = "character",
        VMType = "character",
        maxNodes = "integer",
        AKSName = "character",
        resourceGroupName = "character",
        subscriptionName = "character",
        tenant = "character",
        tenantSelection = "character",
        initialized = "logical",
        autoDelete = "logical"
    ),
    contains = "CloudProvider"
)
