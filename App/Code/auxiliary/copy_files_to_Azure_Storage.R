library(AzureStor)
library(tidyverse)


message("Reading data from Azure Storage...")


# Read environmental variables from local env file
readRenviron(".Renviron")

# Blob_SAS_token <- "sp=r&st=2026-02-27T17:34:20Z&se=2026-02-28T01:49:20Z&skoid=f29ce93e-b0df-4ad3-9d71-0ade7ab99f56&sktid=0347d89a-0174-4dd3-adeb-3339c89c35f5&skt=2026-02-27T17:34:20Z&ske=2026-02-28T01:49:20Z&sks=b&skv=2024-11-04&spr=https&sv=2024-11-04&sr=c&sig=magPwaT%2F5UfRnpOPPDYPiyzrbtcAe9ae0Z%2Bpk01s058%3D"
# Blob_SAS_URL <- "https://isuaabiddstg.blob.core.windows.net/iowa-data-drive?sp=r&st=2026-02-27T17:34:20Z&se=2026-02-28T01:49:20Z&skoid=f29ce93e-b0df-4ad3-9d71-0ade7ab99f56&sktid=0347d89a-0174-4dd3-adeb-3339c89c35f5&skt=2026-02-27T17:34:20Z&ske=2026-02-28T01:49:20Z&sks=b&skv=2024-11-04&spr=https&sv=2024-11-04&sr=c&sig=magPwaT%2F5UfRnpOPPDYPiyzrbtcAe9ae0Z%2Bpk01s058%3D"
# 
# endpoint <- storage_endpoint(Blob_SAS_URL, sas = Blob_SAS_token)
# my_container <- storage_container(endpoint = endpoint, name = "iowa-data-drive")
# 
# storage_read_csv(my_container, file.path("Dashboards", "Health", "HHS-region", "chl_can_fig1_2_rg.csv"), 
#                  col_types = cols(.default = "c", year = "i", index = "d", count = "d"))


# Set endpoint for azure container 
azure_container_con <- function(container_name = "iowa-data-drive"){
  # Get variables from local environment
  account_url <- Sys.getenv("AZURE_STORAGE_URL")
  access_sas <- Sys.getenv("AZURE_STORAGE_SAS") 
  # Create a storage endpoint object
  endp_sas <- storage_endpoint(account_url, sas = access_sas)
  container <- storage_container(endpoint = endp_sas, name = container_name)
  return(container)
}

# Read csv files from Azure Blob and transform
read_my_csv <- function(dir, geo, file_name) {
  storage_read_csv(azure_container_con(), file.path("Dashboards", dir, geo, data_file_suffix(geo, file_name)), 
                   col_types = cols(.default = "c", year = "i", index = "d", count = "d")) %>%
    rename_geography_columns_to_fips(geo_name = geo)
}


library(AzureRMR)
library(AzureStor)
library(tidyverse)

# --- Configuration ---
tenant_id      <- "0347d89a-0174-4dd3-adeb-3339c89c35f5"
subscription_id <- "239577bf-6eff-45cd-a9eb-c4837a6e8ce2"
resource_group <- "aab-ncus-idd-rg"
storage_acc    <- "isuaabiddstg"
container_name <- "iowa-data-drive"
blob_name      <- "Dashboards/Health/HHS-region/chl_can_fig1_2_rg.csv"

# --- 1. Authenticate to Azure with AzureRMR ---
# This will open a browser window to log in
az <- create_azure_login(tenant = tenant_id)

# --- 2. Retrieve the Storage Account Resource ---
sub <- az$get_subscription(subscription_id)
rg <- sub$get_resource_group(resource_group)
stor <- rg$get_storage(storage_acc)

# 

# --- 3. Get the Account Key ---
# AzureRMR fetches the keys securely
keys <- stor$list_keys()
storage_key <- keys$key1

# --- 4. Connect and Read Data with AzureStor ---
# Create endpoint using the key fetched via AzureRMR
endpoint <- storage_endpoint(stor$properties$primaryEndpoints$blob, key = storage_key)
container <- storage_container(endpoint, container_name)

# Read directly into a tidyverse tibble
df <- storage_read_csv(container, blob_name)

# Preview
df %>% glimpse()