# vector of packages needed
requiredPackages = c('lubridate',
                     'data.table',
                     'roxygen2',
                     'tidyverse',
                     'DescTools')

# for each package, install and load if not present. If present, load
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  suppressWarnings(library(p,character.only = TRUE))
}

# list loaded libraries
print(.packages())