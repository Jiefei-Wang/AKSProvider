######################
#  Character slots
######################
setAKSName <- function(x, value){
    x$AKSName <- value
}

.setResourceGroupName <- function(x, value){
    x$resourceGroupName <- value
}

.setSubscriptionName <- function(x, value){
    x$subscriptionName <- value
}

.setRenant <- function(x, value){
    x$tenant <- value
}

.setTenantSelection <- function(x, value){
    x$tenantSelection <- value
}

.setAKSName <- function(x, value){
    x$AKSName <- value
}

.setPoolName <- function(x, value){
    x$poolName <- value
}


.getAKSName <- function(x){
    x$AKSName
}

.getResourceGroupName <- function(x){
    x$resourceGroupName
}

.getSubscriptionName <- function(x){
    x$subscriptionName
}

.getTenant <- function(x){
    x$tenant
}

.getTenantSelection <- function(x){
    x$tenantSelection
}



.getPoolName <- function(x){
    x$poolName
}

######################
#  Object slots
######################

.getAzureClient <- function(x){
    x$azureClient
}

.getSubscription <- function(x){
    x$subscription
}

.getResourceGroup <- function(x){
    x$resourceGroup
}

.getAKS <- function(x){
    x$AKS
}

.getK8sCluster <- function(x){
    x$k8sCluster
}





.setAzureClient <- function(x, value){
    x$azureClient <- value
}

.setSubscription <- function(x, value){
    x$subscription <- value
}

.setResourceGroup <- function(x, value){
    x$resourceGroup <- value
}

.setAKS <- function(x, value){
    x$AKS <- value
}

.setK8sCluster <- function(x, value){
    x$k8sCluster <- value
}

######################
#  Others
######################
.getInitialized <- function(x){
    x$initialized
}

.setInitialized <- function(x, value){
    x$initialized <- value
}

.getAutoDelete <- function(x){
    x$autoDelete
}

.setAutoDelete <- function(x, value){
    x$autoDelete <- value
}
