# Martysbloodsugar

Have some blood sugar data, trying to learn some lessons from it.

# Folder structure

Worthwhile output is in the 'Vignettes' folder compiled to html. Its a work in progress.

Data goes in the inst/extdata folder; should be read only; for xdrip, they're .zip-ped .csv-s

R/ has the "iterations" on the code in .r scripts.

# Notes

Ran *usethis::use_mit_license("Márton Kiss")* to set up the licence.  

Ran *devtools::document()* to make the NAMESPACE document.  

Use *devtools::test()* to run the unit test(s).  

Use *devtools::build_vignettes()* to refresh the .rmd stuff in the vignettes folder.  

Website made by *pkgdown::build_site()*.

