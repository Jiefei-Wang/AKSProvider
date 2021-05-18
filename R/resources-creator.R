askAndCreateAks <- function(resourceGroup, aksName){
    msg <- paste0("No suitable Azure k8s service can be found, do you want to create one?")
    answer <- menu(c("Yes", "No"), title=msg)
    if(answer == 1){
        if(is.null(aksName)){
            aksName <- readline("Name of the Azure k8s service?(default: DockerParallel): ")
            if(aksName == ""){
                aksName <- "DockerParallel"
            }
        }
        subscription$create_aks(name = aksName)
        aksName
    }else{
        stop("No suitable Azure k8s service can be found!")
    }
}



askAndCreateResourceGroup <- function(subscription, resourceGroupName, location){
    msg <- paste0("No suitable resouce group can be found, do you want to create one?")
    answer <- menu(c("Yes", "No"), title=msg)
    if(answer == 1){
        if(is.null(resourceGroupName)){
            resourceGroupName <- readline("Name of the resouce group?(default: DockerParallel): ")
            if(resourceGroupName == ""){
                resourceGroupName <- "DockerParallel"
            }
        }
        if(is.null(location)){
            location <- readline("Location of the resouce group?(default: eastus): ")
            if(location == ""){
                location <- "eastus"
            }
        }
        subscription$create_resource_group(name = name, location = location)
        resourceGroupName
    }else{
        stop("No suitable resouce group can be found!")
    }
}
