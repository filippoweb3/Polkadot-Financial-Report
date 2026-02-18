## Summary

This report explores how an institution or individual can maximize income from staking activities on Polkadot in line with the [**DAP and Future of Stake roadmap**](https://forum.polkadot.network/t/the-roadmap-for-the-dynamic-allocation-pool-dap/16511). The roadmap will be divided into different phases:

- **Phase 1: Will be enacted just before the 14th of March 2026.** Key changes to staking include a minimum self-stake of 10,000 DOT for validators and a minimum commission of 10%. Nominators will incur no slashing and can unbond as soon as their stake is no longer backing any validator (1-2 days).
- **Phase 2: Will be enacted at some point after the 14th of March 2026.** The changes included in Phase II are proposed [**here**](https://forum.polkadot.network/t/proposal-dynamic-allocation-pool-dap/15878). Phase II will allocate the newly minted tokens, departing from the old 15/85 treasury/stakers split.

This package provides tooling to explore the impact of changes to the staking system during Phases I and II and is intended to guide decision-making.

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
