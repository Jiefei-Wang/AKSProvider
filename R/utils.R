getYmlPath <- function(name){
    ymlDir <- system.file('YAML', package = 'AKSProvider')
    if(length(ymlDir)==0){
        ymlDir <- "inst/YAML/"
    }
    paste0(ymlDir , "/", name, ".yml")
}
saveYmlFile <- function(txt){
    path <- tempfile()
    readr::write_file(yaml::as.yaml(txt), file=path)
    path
}

listToK8sEnv <- function(envList){
    k8sEnv <- list()
    for(i in seq_along(envList)){
        k8sEnv[[i]] <- list(name = names(envList)[i], value = as.character(envList[[i]]))
    }
    k8sEnv
}


verbosePrint<-function(verbose, ...){
    if(verbose)
        message(...)
}


.menu <- function(objects, title){
    choice <- menu(objects, title=title)
    stopifnot(choice != 0)
    objects[choice]
}

is.empty <- function(x){
    length(x) == 0
}
is.char.num <- function(x){
   suppressWarnings(!is.na(as.numeric(x)))
}
