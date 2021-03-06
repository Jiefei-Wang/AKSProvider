This package is an extension package for the `DockerParallel` package. In this vignette, we will  cover the information about the Azure Kubernetes service(AKS) provider. For more general information, please see the document of `DockerParallel`.

For accessing the Azure cloud, the AKS provider requires a [tenant] to log in to Azure Resource Manager, a [subscription] to determine where the bill should be sent to, a [resource group] to hold related resources for an Azure solution(in our case, the R parallel cluster) and an [Azure Kubernetes service] to run the Kubernetes cluster. 

There are three ways to create a provider depending on which arguments that are passed to the constructor `AKSProvider()`. They are

1. Specifying the `tenant`, `subscriptionName`, `resourceGroupName` and `AKSName` to `AKSProvider()`. This is recommended if you want to use R in a non-interactive environment.
2. Specifying `tenant` only let the provider ask you to select the rest components during the initialization process.
3. Specifying the argument `k8sCluster` to `AKSProvider()`.

where the argument `k8sCluster` in option 3 is a `KubernetesCluster` object from the package `AzureContainers`. If `k8sCluster` is specified, the other argument will be ignored and `k8sCluster` will be used to deploy the container.

If you choose to use `tenant` in the provider, you must first create a login cache, this will be described in the next section.


# Create a login cache
The provider uses the package `AzureRMR` to authenticate with the Azure cloud. For connecting to Azure, you must first log in to Azure Resource Manager via `AzureRMR::create_azure_login()` to create a login cache. Depending on the function argument, this might open a browser and ask for your account. If you are using a personal account, you have to pass your tenant to the function. For example

```r
AzureRMR::create_azure_login(tenant = "your tenant ID")
```
The above code will help you to log in to a specific tenant using your personal account. Once you have successfully logged in, the cache will be automatically created and you can find your login information via `AzureRMR::list_azure_logins()`


# Create a provider via tenant
The simplest provider can be made by passing your tenant ID that is used in `AzureRMR::create_azure_login()` to `AKSProvider()`. For example

```r
provider <- AKSProvider(tenant = "your tenant ID")
```
If you have multiple logins for a tenant, the provider will print a menu and ask you to choose a login. If you plan to use it in a non-interactive environment, please provides your login number or the input MD5 hash of the token to the argument `tenantSelection` in `AKSProvider` to avoid the selection. The login number or MD5 hash can be found in `AzureRMR::list_azure_logins()`.

If you do not specify the subscription, resource group and Kubernetes service name, you will be prompted to select them during the provider initialization process. You must have a valid subscription registered under the tenant you choose. If you do not have a subscription, please visit the [Azure portal] and create one. It is not required to have a resource group or a Kubernetes service before initializing the provider, you will be asked to create one if they do not exist. However, since the package is not designed to manage the Azure cloud resource, the creation function only provides the basic functionality. For having more control over the creation process, you should visit [Azure portal] and manually create the resource you want.

For avoiding the unexpected cost for the Kubernetes service, the Kubernetes service will be automatically removed when the `DockerCluster` object is removed from the R session if the Kubernetes cluster is created **during** the initialization process. There are two ways to change this default behavior, you can either call `$stopClusterOnExit = FALSE` on the `DockerCluster` object, or pass `autoDelete = FALSE` to `AKSProvider()` when creating the provider. The differences are that the former one will preserve the Kubernetes service as well as the containers running in the service when the `DockerCluster` object is removed. The latter one only keeps the service but the containers will be deleted.


# Extra functions in the provider
The provider provides two extra functions to manage the Kubernetes cluster, they are `deleteK8sCluster` and `k8sGet`. The former one can delete the Kubernetes cluster and the latter one is similar to `kubectl get` in the command line tool. Note that you cannot directly call them from the provider object and they are only available inside the `DockerCluster` object. For example

```r
library(DockerParallel)
## Create the provider and the cluster object
provider <- AKSProvider(tenant = "your tenant ID")
clusterPreset(container = "rbaseDoRedis")
cluster <- makeDockerCluster(cloudProvider = provider)

## Call `k8sGet`
cluster$cloudProvider$k8sGet("all")
```


[tenant]: https://docs.microsoft.com/en-us/azure/active-directory/develop/quickstart-create-new-tenant

[subscription]: https://docs.microsoft.com/en-us/azure/cost-management-billing/manage/create-subscription

[resource group]: https://docs.microsoft.com/en-us/azure/azure-resource-manager/management/manage-resource-groups-portal

[Azure Kubernetes service]: https://azure.microsoft.com/en-us/services/kubernetes-service/

[Azure portal]: https://portal.azure.com/


