askForLocation <- function(provider, default = "eastus"){
    subscription <- .getSubscription(provider)
    locations <- NULL
    repeat{
        message(paste0("Location of the AKS(default: ", default,")?: "))
        message("Hint: Type <l> to view all locations")
        location <- readline()
        stopifnot(location != "0")
        if(location == "l"){
            if(is.null(locations)){
                locations <- subscription$list_locations()
                locations <- locations[stringr::str_order(locations$name),]
            }
            print(locations[,c("name", "displayName")])
        }else{
            break
        }
    }
    if(location == ""){
        location <- default
    }
    ## Do the validation if `locations` exists
    if(!is.null(locations)){
        stopifnot(location%in%locations$name)
    }
    location
}


askAndCreateAKS <- function(provider, ask = TRUE){
    resourceGroup <- .getResourceGroup(provider)
    AKSName <- .getAKSName(provider)
    subscription <- .getSubscription(provider)

    if(ask){
        msg <- paste0("No suitable Azure k8s service can be found, do you want to create one?")
        answer <- menu(c("Yes", "No"), title=msg)
        stopifnot(answer == 1)
    }
    message("We need to ask you a few questions, type <0> at any time to quit")

    ## set the aks name
    if(is.empty(AKSName)){
        message("Name of the Azure k8s service?(default: DockerParallel): ")
        AKSName <- readline()
        stopifnot(AKSName != "0")
        if(AKSName == ""){
            AKSName <- "DockerParallel"
        }
    }

    ## set the location

    location <- askForLocation(provider, default = resourceGroup$location)

    ## set the VM configuration
    VMs <- resourceGroup$list_vm_sizes()
    VMName <- names(VMs)
    VMName[VMName == "resourceDiskSizeInMB"] <- "diskSizeInMB"
    VMName[VMName == "numberOfCores"] <- "cores"
    names(VMs) <- VMName
    minCores <- 2
    minMem <- 4096
    repeat{
        message("Name of the virtual machine(default: Standard_DS2_v2)?:")
        message("Hint: ")
        message("1. You can type <cpu> or <cpu-memory> to view the available VMs, e.g. 4-4096 means to match VMs which has exact 4 cores and over 4096 MB memory.")
        message("2. Please make sure the selected VM has enough disk space for running the container.")
        VMType <- readline()
        stopifnot(VMType != "0")
        if(grepl("-", VMType, fixed = TRUE)||
           is.char.num(VMType)){
            if(is.char.num(VMType)){
                requiredCPU <- max(minCores, as.numeric(VMType))
                requiredMem <- minMem
            }else{
                config <- strsplit(VMType, "-", fixed = TRUE)[[1]]
                stopifnot(length(config) == 2)
                config <- as.integer(config)
                requiredCPU <- max(minCores, config[1])
                requiredMem <- max(minMem, config[2])
            }


            filteredVMs <- VMs[VMs$cores == requiredCPU &
                                   VMs$memoryInMB >= requiredMem,]

            filteredVMs <- filteredVMs[order(filteredVMs$cores, filteredVMs$memoryInMB, filteredVMs$diskSizeInMB),]
            print(filteredVMs[,c("name", "cores", "memoryInMB", "diskSizeInMB")])
        }else{
            if(VMType == ""){
                VMType <- "Standard_DS2_v2"
            }
            if(VMType %in% VMs$name){
                break
            }else{
                message("Invalid input")
            }

        }
    }

    cores <- VMs$cores[VMs$name == VMType]
    memory <- VMs$memoryInMB[VMs$name == VMType]
    disk <- VMs$diskSizeInMB[VMs$name == VMType]


    ## set the maximum nodes number
    message("The maximum number of the nodes in the kubernetes cluster(default: 4)?:")
    maxNodes <- readline()
    if(maxNodes != ""){
        maxNodes <- as.integer(maxNodes)
    }else{
        maxNodes <- 4L
    }
    stopifnot(maxNodes >= 1)


    message("Cluster Summary:")
    message("  VM name:   ", VMType)
    message("  Cores:     ", cores)
    message("  Memory:    ", memory)
    message("  Disk size: ", disk)
    message("  min nodes: ", 1)
    message("  max nodes: ", maxNodes)

    ## Set the agent pool
    poolName <- .getPoolName(provider)
    agent_pools <- AzureContainers::agent_pool(name = poolName,
                                               count = 1,
                                               size = VMType,
                                               use_scaleset = TRUE,
                                               autoscale_nodes = c(1, maxNodes))
    message("creating the Azure kubernetes cluster, this will take a while, please be patient.")
    AKS <- resourceGroup$create_aks(name = AKSName,
                                    location = location,
                                    agent_pools = agent_pools)
    .setAKS(provider, AKS)
    .setAKSName(provider, AKSName)
    if(is.empty(.getAutoDelete(provider))){
        .setAutoDelete(provider, TRUE)
    }
    AKSName
}

askAndCreateResourceGroup <- function(provider, ask = TRUE){
    resourceGroupName <- .getResourceGroupName(provider)
    subscription <- .getSubscription(provider)

    if(ask){
        msg <- paste0("No suitable resouce group can be found, do you want to create one?")
        answer <- menu(c("Yes", "No"), title=msg)
        stopifnot(answer == 1)
    }
    if(is.empty(resourceGroupName)){
        resourceGroupName <- readline("Name of the resouce group?(default: DockerParallel): ")
        if(resourceGroupName == ""){
            resourceGroupName <- "DockerParallel"
        }
    }
    location <- askForLocation(provider)
    resourceGroup <- subscription$create_resource_group(name = name,
                                                        location = location)
    .setResourceGroup(provider, resourceGroup)
    .setResourceGroupName(provider, resourceGroupName)
    resourceGroupName
}
