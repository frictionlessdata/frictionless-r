# camtrapdp (0.5.0)

* GitHub: <https://github.com/inbo/camtrapdp>
* Email: <mailto:peter.desmet@inbo.be>
* GitHub mirror: <https://github.com/cran/camtrapdp>

Run `revdepcheck::revdep_details(, "camtrapdp")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       Expected `read_camtrapdp(file.path(temp_dir_merged, "datapackage_different_xy.json"))` not to throw any errors.
       Actually got a <purrr_error_indexed> with message:
         i In index: 1.
         Caused by error in `check_path()`:
         ! Can't find file at './deployments.csv'.
       
       [ FAIL 1 | WARN 6 | SKIP 117 | PASS 12 ]
       Deleting unused snapshots: 'merge_camtrapdp/datapackage_identical_xy.json',
       'write_camtrapdp/datapackage.json', 'write_dwc/meta.xml',
       'write_dwc/multimedia.csv', 'write_dwc/multimedia_media_based.csv',
       'write_dwc/occurrence.csv', 'write_dwc/occurrence_media_based.csv', and
       'write_eml/eml.xml'
       Error:
       ! Test failures.
       Execution halted
     ```

