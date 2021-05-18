getCluster <- function(provider){
    if(is.null(provider$k8sCluster)){
        aks <- getAks(provider)
        provider$k8sCluster <- aks$get_cluster()

    }
    provider$k8sCluster
}
getAks <- function(provider){
    if(is.null(provider$aks)){
        resourceGroup <- getResourceGroup(provider)
        aksList <- resourceGroup$list_aks()
        ## Check if a Azure k8s service needs to be created
        needCreate <- FALSE
        if(!is.null(provider$aksName)){
            if(!provider$aksName %in% names(aksList)){
                needCreate <- TRUE
            }else{
                aksName <- provider$aksName
            }
        }else{
            if(length(aksList)==0){
                needCreate <- TRUE
            }else{
                aksName <- names(aksList)[1]
                message("Auto find the k8s name: ", aksName)
            }
        }
        if(needCreate){
            aksName <-
                askAndCreateAks(resourceGroup = resourceGroup,
                                aksName = provider$aksName)
            aksList <- resourceGroup$list_aks()
        }

        ## find the correct Azure k8s service
        provider$aks <- aksList[[aksName]]
    }
    provider$aks
}


getResourceGroup <- function(provider){
    if(is.null(provider$resourceGroup)){
        subscription <- getSubscription(provider)
        resourceGroups <- subscription$list_resource_groups()

        ## Check if a resource group needs to be created
        needCreate <- FALSE
        if(!is.null(provider$resourceGroupName)){
            if(!provider$resourceGroupName %in% names(resourceGroups)){
                needCreate <- TRUE
            }else{
                resourceGroupName <- provider$resourceGroupName
            }
        }else{
            if(length(resourceGroups)==0){
                needCreate <- TRUE
            }else{
                resourceGroupName <- names(resourceGroups)[1]
                message("Auto find resource group: ", resourceGroupName)
            }
        }
        if(needCreate){
            resourceGroupName <-
                askAndCreateResourceGroup(subscription = subscription,
                                          resourceGroupName = provider$resourceGroupName,
                                          location = provider$location)
            resourceGroups <- subscription$list_resource_groups()
        }
        ## find the correct resource group
        provider$resourceGroup <- resourceGroups[[resourceGroupName]]
    }
    provider$resourceGroup
}


getSubscription <- function(provider){
    if(is.null(provider$subscription)){
        client <- getAzureClient(provider)
        subscriptions <- client$list_subscriptions()
        if(length(subscriptions)==0){
            stop("No subscription can be found in the tenant <",
                 client$tenant
                 ,">")
        }
        subscriptionNames <- vapply(subscriptions, function(x)x$name, character(1))

        if(!is.null(provider$subscriptionName)){
            if(!provider$subscriptionName %in% subscriptionNames){
                stop("The subscription name <",provider$subscriptionName,
                     "> is not found!")
            }
            subscriptionName <- provider$subscriptionName
            provider$subscription <- subscriptions[[which(subscriptionNames == subscriptionName)]]
        }else{
            subscriptionID <- names(subscriptions)[[1]]
            message("Auto find subscription: ", subscriptionID)
            provider$subscription <- subscriptions[[subscriptionID]]
        }
    }
    provider$subscription
}


getAzureClient <- function(provider){
    if(is.null(provider$azureClient)){
        clients <- list_azure_logins()
        if(length(clients)==0){
            stop("No azure client can be found, please login via <AzureRMR::create_azure_login>")
        }

        tenantID <- names(clients)
        if(!is.null(provider$tenantID)){
            if(!provider$tenantID %in% tenantID){
                stop("The tenant <",provider$tenantID,"> is not found!")
            }
            tenantID <- provider$tenantID
        }else{
            tenantID <- names(clients)[1]
            message("Auto find tenant: ", tenant)
        }
        provider$azureClient <- get_azure_login(tenant = tenantID)
    }
    provider$azureClient
}




