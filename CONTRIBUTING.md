# Contributing to osrm

This outlines how to propose a change to osrm. 

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

*   Fork the package and clone onto your computer. 

*   Install all development dependencies, and then make sure the package passes R CMD check. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). 

*   Make your changes, commit to git, and then create a PR.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). 

### Code style

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2) for documentation.  

*  We use [tinytest](https://cran.r-project.org/package=tinytest) for unit tests. 
   Contributions with test cases included are easier to accept.  

### Support

You can ask questions about the package in the [Issues section](https://github.com/riatelab/osrm/issues) of this repository.
