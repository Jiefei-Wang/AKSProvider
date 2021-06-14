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
        AKSName = "character",
        resourceGroupName = "character",
        subscriptionName = "character",
        tenant = "character",
        tenantSelection = "character",
        location = "character",
        initialized = "logical"
    ),
    contains = "CloudProvider"
)
