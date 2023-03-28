pkgLoad <- function( packages = "favourites" ) {
  
  if( length( packages ) == 1L && packages == "favourites" ) {
    packages <- c( "tidyverse", "here", "Hmisc", "hrbrthemes", "viridis", "reshape2", "survey", "grid", "gridExtra", "spatstat", "magrittr", "furrr", "beepr", "devtools"
    )
  }
  
  packagecheck <- match( packages, utils::installed.packages()[,1] )
  
  packagestoinstall <- packages[ is.na( packagecheck ) ]
  
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall,
                             repos = "https://cran.ma.imperial.ac.uk/"
    )
  } else {
    print( "All requested packages already installed" )
  }
  
  for( package in packages ) {
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }
  
}


pkgLoad()
devtools::install_github("INSP-RH/bw")

