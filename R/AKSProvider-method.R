#
# az <- get_azure_login(tenant = "6a3acede-d9c8-4008-8de0-365b51ee6fd5")
# # delete_azure_login(tenant = "6a3acede-d9c8-4008-8de0-365b51ee6fd5")
# # create container registry
#
# subscription <- az$
#     get_subscription("5bc142ac-2e44-453e-b05b-3145c5451d9d")
# resgroup <- subscription$get_resource_group("test")
#
# aks <- resgroup$get_aks("test")
# cluster <- aks$get_cluster()


#' Create an AKS provider object
#'
#' Create an AKS provider object, you must have a login cache made by
#' `AzureRMR::create_azure_login` before you call this function.
#'
#' @param k8sCluster KubernetesCluster, the K8S cluster object from the package `AzureContainers`
#' @param poolName Character(1), the name of the node pool. This parameter
#' will only be used when the k8s cluster does not exist and need to be created.
#' @param AKSName Character(1), the Azure Kubernetes service name
#' @param resourceGroupName Character(1), the resource group name. If empty, the function
#' will print a menu and ask you to choose one.
#' @param subscriptionName Character(1), the subscription name. If empty, the function
#' will print a menu and ask you to choose one.
#' @param tenant Character(1), the tenant you want to use. This argument will be passed to
#' `AzureRMR::get_azure_login` to get the ARM login client.
#' @param tenantSelection Character(1), if you have multiple logins for a given tenant,
#' which one to use. This can be a number, or the input MD5 hash of the token used for the login
#' (You can find them via `AzureRMR::list_azure_logins()`). If not supplied,
#' we will print a menu and ask you to choose a login.
#' @param autoDelete Logical(1), whether the kubernetes cluster should be deleted when the
#' `DockerCluster` object has been removed from the workspace? If `NULL`, the kubernetes cluster
#' will only be removed if it is created by this function.
#'
#' @examples
#' provider <- AKSProvider(tenant = "Your tenant ID")
#' @return An `AKSProvider` reference object
#' @export
AKSProvider <- function(
    k8sCluster = NULL,
    poolName = "akspool",
    AKSName = Sys.getenv("AZ_AKS_NAME"),
    resourceGroupName = Sys.getenv("AZ_RESOURCE_GROUP_NAME"),
    subscriptionName = Sys.getenv("AZ_SUBSCRIPTION"),
    tenant = Sys.getenv("AZ_TENANT"),
    tenantSelection = character(0),
    autoDelete = NULL){

    if(AKSName == ""){
        AKSName <- character()
    }
    if(resourceGroupName == ""){
        resourceGroupName <- character()
    }
    if(subscriptionName == ""){
        subscriptionName <- character()
    }
    if(tenant == ""){
        tenant <- character()
    }
    if(is.null(autoDelete)){
        autoDelete <- logical()
    }

    provider <- .AKSProvider(
        azureClient = NULL,
        subscription = NULL,
        resourceGroup = NULL,
        AKS = NULL,
        k8sCluster = k8sCluster,
        poolName = poolName,
        AKSName = AKSName,
        resourceGroupName = resourceGroupName,
        subscriptionName = subscriptionName,
        tenant = tenant,
        tenantSelection=tenantSelection,
        initialized = FALSE,
        autoDelete = autoDelete)
    provider
}


setMethod("show", "AKSProvider", function(object){
    AKSName <- .getAKSName(object)
    resourceGroupname <- .getResourceGroupName(object)
    subscriptionName <- .getSubscriptionName(object)
    tenant <- .getTenant(object)
    initialized <- .getInitialized(object)

    cat("AKS provider reference object\n")
    cat("  AKS name           : ", AKSName, "\n")
    cat("  Resource group name: ", resourceGroupname, "\n")
    cat("  Subscription name  : ", subscriptionName, "\n")
    cat("  Tenant             : ", tenant, "\n")
    cat("  Initialized        : ", ifelse(initialized, "TRUE", "FALSE"), "\n")

    k8sCluster <- .getK8sCluster(object)
    if(!is.null(k8sCluster)){
        cat("K8s status:\n")
        out <- capture.output(k8sCluster$get("deployments"))
        cat(paste0(paste0("  ", out), collapse = "\n"))
    }
})

