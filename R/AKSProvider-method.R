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
    azureClient = NULL,
    subscription = NULL,
    resourceGroup = NULL,
    aks = NULL,
    aksName = NULL,
    resourceGroupName = NULL,
    subscriptionName = NULL,
    tenantID = NULL,
    location = NULL,
    serviceName = "dockerparallel-redis-service",
    serverDeploymentName = "dockerparallel-server-deployment",
    workerDeploymentName = "dockerparallel-worker-deployment"){

    provider <- .AKSProvider(
        serviceName = serviceName,
        serverDeploymentName = serverDeploymentName,
        workerDeploymentName = workerDeploymentName,
        azureClient = azureClient,
        subscription = subscription,
        resourceGroup = resourceGroup,
        aks = aks,
        k8sCluster = k8sCluster,
        aksName = aksName,
        resourceGroupName = resourceGroupName,
        subscriptionName = subscriptionName,
        tenantID = tenantID,
        location = location,
        initialized = FALSE)
    provider
}
