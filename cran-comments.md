## R CMD check results

0 errors | 0 warnings | 0 notes

Latest version of rmarkdown (2.26) removed stringr -> stringi dependency, 
causing build errors for this package. This patch version explicitly includes 
stringi in Suggests.
