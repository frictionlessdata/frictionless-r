## R CMD check results

0 errors | 0 warnings | 1 note

* New maintainer:
    Peter Desmet <peter.desmet@inbo.be>
  Old maintainer(s):
    Peter Desmet <peter.desmet.work@gmail.com>
  
  I have updated my email address to be the same for all packages on CRAN.

* CRAN ERROR for version 1.1.0 has been resolved.
  The example for `get_schema()` failed because of a changed URL.
  The example no longer requires a remote file and the function fails gracefully
  if it does.

* CRAN NOTE for the version 1.1.0 has been resolved.
  `read_resource.Rd` had a missing package anchor for `\link{}`.
  This has been fixed.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages
