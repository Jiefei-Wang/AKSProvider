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


AKSProvider <- function(
    k8sCluster = NULL,
    AKSName = Sys.getenv("AZ_AKS_NAME"),
    resourceGroupName = Sys.getenv("AZ_RESOURCE_GROUP_NAME"),
    subscriptionName = Sys.getenv("AZ_SUBSCRIPTION"),
    tenant = Sys.getenv("AZ_TENANT"),
    location = Sys.getenv("AZ_LOCATION")){

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
    if(location == ""){
        location <- character()
    }

    provider <- .AKSProvider(
        azureClient = NULL,
        subscription = NULL,
        resourceGroup = NULL,
        AKS = NULL,
        k8sCluster = k8sCluster,
        AKSName = AKSName,
        resourceGroupName = resourceGroupName,
        subscriptionName = subscriptionName,
        tenant = tenant,
        location = location,
        initialized = FALSE)
    provider
}


setMethod("show", "AKSProvider", function(object){
    AKSName <- .getAKSName(object)
    resourceGroupname <- .getResourceGroupName(object)
    subscriptionName <- .getSubscriptionName(object)
    tenant <- .getTenant(object)
    location <- .getLocation(object)
    initialized <- .getInitialized(object)

    message("AKS provider reference object")
    message("  AKS name           : ", AKSName)
    message("  Resource group name: ", resourceGroupname)
    message("  Subscription name  : ", subscriptionName)
    message("  Tenant             : ", tenant)
    message("  location           : ", location)
    message("  Initialized        : ", ifelse(initialized, "TRUE", "FALSE"))
})

