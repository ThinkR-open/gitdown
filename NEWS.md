# gitdown 0.1.6

* Ask for Pandoc > 2.0 as for new {rmarkdown}
* Use 'main' branch as default for the repository example
* Fix unit test for new 'rmarkdown' version
* Fix unit test for date-time check due to changing timezone

# gitdown 0.1.5

* Change unit tests

# gitdown 0.1.4

* Allow tests to start a day and finish the following one

# gitdown 0.1.3

* Protect tests from bad time rounding

# gitdown 0.1.2

* Protect from older Pandoc versions

# gitdown 0.1.1

* Improve documentation

# gitdown 0.1.0

* Get ready for CRAN

# gitdown 0.0.1

Breaking changes
* `names_section` parameter is replaced by a named vector in `pattern`

Other changes
* List all patterns of `pattern.table` even if no commits associated
* Add parameter `pattern.table` to allow for user specific pattern titles
* `create_vignette_last_modif()` creates a vignette with dates of files modification in git
* Added summary tables for each section
* Added a `NEWS.md` file to track changes to the package.
