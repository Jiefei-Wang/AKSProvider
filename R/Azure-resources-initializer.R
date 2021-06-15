initializeAzure <- function(provider, verbose = 0L){
    initializeAzureClient(provider)
    initializeSubscription(provider)
    verbosePrint(verbose > 0, "Using the subscription <", .getSubscriptionName(provider), ">")
    initializeResourceGroup(provider)
    verbosePrint(verbose > 0, "Using the resource group <", .getResourceGroupName(provider), ">")
    initializeAKS(provider)
    verbosePrint(verbose > 0, "Using the AKS cluster <", .getAKSName(provider), ">")
}

getK8sCluster <- function(provider){
    k8sCluster <- .getK8sCluster(provider)
    if(is.empty(k8sCluster)){
        initializeAzure(provider, verbose = 0L)
    }
    k8sCluster
}

initializeAKS <- function(provider){
    AKS <- .getAKS(provider)
    if(is.empty(AKS)){
        resourceGroup <- .getResourceGroup(provider)
        stopifnot(!is.empty(resourceGroup))
        AKSName <- .getAKSName(provider)
        AKSList <- resourceGroup$list_aks()

        if(is.empty(AKSName)||
           !AKSName %in% names(AKSList)){
            ## Selecting the AKS
            if(length(AKSList)!=0){
                msg <- paste0("Multiple AKS clusters have been found, please select(enter 0 to exit)")
                creationOption <- "Create a new AKS cluster"
                AKSName <- .menu(c(names(AKSList), creationOption), title=msg)
                if(AKSName == creationOption){
                    AKSName <-
                        askAndCreateAKS(provider = provider, ask = FALSE)
                }
            }else{
                AKSName <-
                    askAndCreateAKS(provider = provider)
            }
        }
        AKS <- resourceGroup$get_aks(AKSName)
        .setAKSName(provider, AKSName)
        .setAKS(provider, AKS)
    }

    k8sCluster <- .getK8sCluster(provider)
    if(is.empty(k8sCluster)){
        k8sCluster <- AKS$get_cluster()
        .setK8sCluster(provider, k8sCluster)
    }

    AKS
}


initializeResourceGroup <- function(provider){
    resourceGroup <- .getResourceGroup(provider)
    if(is.empty(resourceGroup)){
        subscription <- .getSubscription(provider)
        stopifnot(!is.empty(subscription))
        resourceGroupName <- .getResourceGroupName(provider)
        resourceGroups <- subscription$list_resource_groups()

        if(is.empty(resourceGroupName)||
           !resourceGroupName %in% names(resourceGroups)){
            ## selecting the resource group
            if(length(resourceGroups)!=0){
                msg <- paste0("Multiple resource groups have been found, please select(enter 0 to exit)")
                creationOption <- "Create a new resource group"
                resourceGroupName <- .menu(c(names(resourceGroups), creationOption), title=msg)
                if(resourceGroupName == creationOption){
                    resourceGroupName <-
                        askAndCreateResourceGroup(provider = provider, ask = FALSE)
                }

            }else{
                resourceGroupName <-
                    askAndCreateResourceGroup(provider = provider)
            }
        }
        resourceGroup <- subscription$get_resource_group(resourceGroupName)
        .setResourceGroup(provider, resourceGroup)
        .setResourceGroupName(provider, resourceGroupName)
    }
    resourceGroup
}


initializeSubscription <- function(provider){
    subscription <- .getSubscription(provider)
    if(is.empty(subscription)){
        client <- .getAzureClient(provider)
        stopifnot(!is.empty(client))
        subscriptionName <- .getSubscriptionName(provider)
        if(is.empty(subscriptionName)){
            ## find the subscription from the list
            subscriptions <- client$list_subscriptions()
            if(length(subscriptions)==0){
                stop("No subscription can be found in the tenant <",
                     client$tenant
                     ,">")
            }
            subscriptionNames <- vapply(subscriptions, function(x)x$name, character(1))
            msg <- paste0("Multiple subscriptions have been found, please select(enter 0 to exit)")
            subscriptionName <- .menu(subscriptionNames, title=msg)
        }
        subscription <- client$get_subscription_by_name(subscriptionName)
        .setSubscriptionName(provider, subscriptionName)
        .setSubscription(provider, subscription)
    }
    subscription
}


initializeAzureClient <- function(provider){
    client <- .getAzureClient(provider)
    if(is.empty(client)){
        tenant <- .getTenant(provider)
        tenantSelection <- .getTenantSelection(provider)
        args <- list(tenant = tenant, selection = tenantSelection)
        args <- args[!vapply(args, is.empty, logical(1))]
        azureClient <- do.call(get_azure_login, args = args)
        .setAzureClient(provider, azureClient)
    }
    client
}


