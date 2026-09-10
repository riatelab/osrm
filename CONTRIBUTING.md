# Contributing to osrm

This outlines how to propose a change to `osrm`. 

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, you should first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).


## Code style

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2) for documentation.  

*  We use [tinytest](https://cran.r-project.org/package=tinytest) for unit tests. 
   Contributions with test cases included are easier to accept.  
   
*  We use [styler](https://CRAN.R-project.org/package=styler) with current default values for code style.


## Support

You can ask questions about the package in the [Issues section](https://github.com/riatelab/osrm/issues) of this repository.

## AI policy

For ethical reasons, this project does not accept AI generated contributions. 