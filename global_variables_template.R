library(dplyr)
library(kableExtra)
library(lubridate)

global_variables <- list(
  current_issuance = 120000000, # current token annual issuance
  stock = 200000, # held tokens
  bonded_pct = 0.9, # % of stock bonded in stacking
  budget = 10000, # annual budget requested in USD
  nodes = 600, # number of active nodes
  node_reward = 2000, # reward for running a node in USD/month
  node_cost = 950, # cost per node in USD/month
  FTE = 2*200000, # annual cost for full time engineers, 2x, in USD
  overcoll = 1, # overcollateralization factor for minting DOT-native stablecoin pUSD
  p2_start = "2026-08-14", # start date of Phase 2, realistically P2 will start some time after the 14th of March,
  non_reserve_to_tresury = 0.207, # % of non-reserve issuance going to treasury, default: 0.207
  non_reserve_to_resilience = 0.1348 # % of non-reserve issuance going to resilience, default: 0.1348
)

subscan_data <- fetch_chain_data(api_key = global_variables$api_key) # fetching

bonded_staking <- subscan_data$validator_bonded + subscan_data$nominator_bonded # total bonded in staking
staking_rate <- bonded_staking/subscan_data$total_issuance # current staking rate

global_variables <- append(global_variables, c(supply = subscan_data$total_issuance, # append Subscan data to global variables
                                               bonded_staking = bonded_staking,
                                               staking_rate = staking_rate,
                                               price = subscan_data$price))

global_variables$api_key <- NULL # Subscan API key

usethis::use_data(global_variables, overwrite = T) # save/overwrite global variables data
