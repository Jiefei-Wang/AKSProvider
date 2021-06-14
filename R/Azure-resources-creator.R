askAndCreateAKS <- function(resourceGroup, AKSName, location){
    msg <- paste0("No suitable Azure k8s service can be found, do you want to create one?")
    answer <- menu(c("Yes", "No"), title=msg)
    stopifnot(answer == 1)
    if(is.null(AKSName)){
        AKSName <- readline("Name of the Azure k8s service?(default: DockerParallel): ")
        if(AKSName == ""){
            AKSName <- "DockerParallel"
        }
    }
    if(is.null(location)){
        location <- readline("Location of the AKS(default: eastus)?: ")
        if(location == ""){
            location <- "eastus"
        }
    }
    resourceGroup$create_aks(name = AKSName, location = location)
    AKSName
}

askAndCreateResourceGroup <- function(subscription, resourceGroupName, location){
    msg <- paste0("No suitable resouce group can be found, do you want to create one?")
    answer <- menu(c("Yes", "No"), title=msg)
    stopifnot(answer == 1)
    if(is.null(resourceGroupName)){
        resourceGroupName <- readline("Name of the resouce group?(default: DockerParallel): ")
        if(resourceGroupName == ""){
            resourceGroupName <- "DockerParallel"
        }
    }
    if(is.null(location)){
        location <- readline("Location of the resouce group(default: eastus)?: ")
        if(location == ""){
            location <- "eastus"
        }
    }
    subscription$create_resource_group(name = name, location = location)
    resourceGroupName
}
