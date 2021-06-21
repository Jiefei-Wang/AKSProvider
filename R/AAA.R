#' The Azure Kubenetes service provider
#'
#' The Azure Kubenetes service provider for the `DockerParallel` package. Please
#' call `AKSProvider()` to create the provider.
#'
#' @exportClass AKSProvider
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
