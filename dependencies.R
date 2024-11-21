packages <- c(
  
  'dplyr',
  'readxl',
  'shiny',
  'leaflet',
  'shinyjs',
  'sf',
  'reactable',
  'stringr',
  'viridis',
  'wesanderson',
  'purrr',
  'bslib',
  
  '')
options(Ncpus = -1)
for (pkg in packages) {
  if (pkg == '') {
    next
  }
  install.packages(pkg)
  if ( ! library(pkg, character.only=TRUE, logical.return=TRUE) ) {
    quit(status=1, save='no')
  }
}
