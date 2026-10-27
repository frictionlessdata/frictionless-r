## R CMD check results

0 errors | 0 warnings | 2 notes

> Maintainer: ‘Peter Desmet <peter.desmet@inbo.be>’
> 
> Found the following (possibly) invalid URLs:
>   URL: https://ror.org/00j54wy13
>     From: man/frictionless-package.Rd
>     Status: 429
>     Message: Too Many Requests
>   URL: https://www.go-fair.org/fair-principles/ (moved to https://www.gofair.foundation/fair-principles)
>     From: DESCRIPTION
>           man/frictionless-package.Rd
>           inst/doc/frictionless.html
>           README.md
>     Status: 200
>     Message: OK
> 
> Found the following (possibly) invalid ROR IDs:
>   ID: 00j54wy13	(from: DESCRIPTION)

These issues are not related to this package and might be temporary.
- <https://ror.org/00j54wy13> is a valid URL, ror.org is likely throttling automated requests.
- <https://www.go-fair.org/fair-principles/> is a valid URL, with an unstable status code (200 and 301). I have contacted the website maintainers.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
