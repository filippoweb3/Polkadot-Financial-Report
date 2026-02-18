## Instructions

### Installation

To run the package, you need to [install R and RStudio (recommended)](https://posit.co/download/rstudio-desktop/). Download the package folder from GitHub. In RStudio, choose Session > Set Working Directory > Choose Directory, then select the downloaded package folder.

Make sure you have the `devtools` package installed by running `install.packages("devtools")`, and then run:

```
devtools::install()
```

This will install the R package and make sure you have all dependencies. Then run:

```
devtools::load_all()
```

### Helper Functions

Use the help function to read the documentation for each helper function in the `helpers.R` script:

```
help("fetch_chain_data")
```

### Report generation

To generate the report, select the Future of Stake report (`FoS_report.Rmd`) and Cmd + Opt + K to compile the PDF document.

Before generating the report, you can run the `global_variables.R` script to update the `global_variables` object.
