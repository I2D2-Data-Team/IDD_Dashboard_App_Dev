library(tidyverse)

# function to copy files from one directory to another
copy_new_ind_data <- function(files_from, files_to, data_cross) {
  for (i in 1:nrow(data_cross)) {
    my_files_from <- files_from[str_detect(files_from, data_cross$my.file[i])]
    
    file.copy(from = my_files_from, 
              to = file.path(files_to, data_cross$my.dir[i], basename(my_files_from)), 
              overwrite = TRUE)
  }
}

# set the names of individual dashboard folders
folder_cross <- tribble(
  ~my.cybox,       ~i2d2.cybox,
  "Demographic",  "demographic indicators",
  "Economic",     "economic indicators",
  "Health",       "health and social indicators",
  "Services",     "services indicators"
)

# set folder names and corresponding data file ending patterns
data_cross <- tribble(
  ~my.dir,     ~my.file,
  'IA-county',  "_co.csv",
  'ECI-area',   "_eci.csv",
  'HHS-region',   "_rg.csv",
  'HS-grantee',   "_hs.csv",
  'MIECHV-area',   "_mv.csv"
)

# set paths to CyBox directories
my_cybox <- file.path(Sys.getenv("USERPROFILE"), "Box/My IDS/_DATA/IDD")
i2d2_cybox <- file.path(Sys.getenv("USERPROFILE"), "Box/Iowa IDS/Projects/_IA Data Drive/Indicators/I2D2 Data/ACS INDICATORS/Indicators_GioUpload")

# Create folders for each geography if does not exist
for (i in folder_cross$my.cybox) {
  for (j in data_cross$my.dir) {
    cat("Checking", j, "in", i, " >>>  ")
    dir.to.check <- file.path(my_cybox, i, j)
    if (dir.exists(dir.to.check)) {
      cat(crayon::green("folder exisits in the directory\n"))
    } else {
      dir.create(dir.to.check)
        cat(crayon::blue("created missing folder", j, "in", i, "directory\n"))
    }
  }
}

# Compute number of files copied and time
number_copied <- 0

start_time <- Sys.time()
# copy files
for (i in seq_along(folder_cross$my.cybox)) {
  files_to <- file.path(my_cybox, folder_cross$my.cybox[i])
  files_from <- list.files(file.path(i2d2_cybox, folder_cross$i2d2.cybox[i]),
                           full.names = TRUE)
  # for (j in files_from) {
  #   cat("Copying", crayon::blue(basename(j)), "to", 
  #       crayon::green(folder_cross$my.cybox[i]), "in my CyBox\n")
  # }
  cat("start copying", crayon::green(folder_cross$my.cybox[i]), "data to my CyBox\n")
  # copy_new_ind_data(files_from, files_to, data_cross)
  
  number_copied <- number_copied + length(files_from)
}
end_time <- Sys.time()

# Report number of files copied and time
total_time <- print(round(end_time - start_time, 2))
cat("Total of", number_copied, "files were copied in", total_time, attr(total_time, "units"), "\n")
