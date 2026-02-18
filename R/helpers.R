#' Fetches on-chain data
#'
#' @param chain Network where data are fetched (Polkadot or Kusama).
#' @param api_key Key for Subscan API
#' @returns A list containing the following objects:
#' - `total issuance`: the total token supply
#' - `nominator_bonded`: the tokens bonded in staking by nominators
#' - `validator_bonded`: the tokens bonded in staking by validators
#' - `price`: the token price in USD
#' @export
fetch_chain_data <- function(chain = "Polkadot", api_key) {

  if(chain == "Polkadot"){
    api_url <- "https://assethub-polkadot.api.subscan.io/api/scan/token"

    scale <- 10^10

    body <- list(
      symbol = "DOT",
      page = 0,
      row = 50
    )

  } else if(chain == "Kusama"){
    api_url <- "https://assethub-kusama.api.subscan.io/api/scan/token"

    scale <- 10^12

    body <- list(
      symbol = "KSM",
      page = 0,
      row = 50
    )
  }

  res <- httr::POST(
    url = api_url,
    httr::add_headers(
      `Content-Type` = "application/json",
      `x-api-key` = api_key
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  content <- httr::content(res, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(content)

  results <- list(
    total_issuance = as.numeric(data$data$detail$DOT$total_issuance)/scale,
    validator_bonded = as.numeric(data$data$detail$DOT$validator_bonded)/scale,
    nominator_bonded = as.numeric(data$data$detail$DOT$nominator_bonded)/scale,
    price = as.numeric(data$data$detail$DOT$price)
  )

  return(results)

}


#' Calculates Token Issuance and Allocations
#'
#' @param current_supply The current token total supply
#' @param current_issuance The current token annual issuance
#' @param save Option to save the output as a .csv (FALSE by default)
#' @param token_price The token price in USD
#' @param today Today's Date (or any date, keep in mind to adjust `current supply` and `current issuance`)
#' @param end_date Date to end the fetched data
#' @param nodes The total number of nodes producing blocks
#' @param phase2.opt A list of the following objects:
#' - `phase2_start` A date for the start of Phase II
#' - `node_reward` rewards in USD per node per month
#' - `overcollateral_factor` Over-collateralization for minting stablecoins to pay for operations
#' - `to_resilience` Fraction of the non-reserve to be allocated for resilience
#' - `to_treasury` Fraction of the non-reserve to be allocated for the Treasury
#' @returns A dataframe with daily values for:
#' - date, year, month and day
#' - issuance: the daily issuance of newly minted tokens
#' - supply: to total token circulating supply
#' - reserve: the daily issuance for reserve
#' - non-reserve: the daily issuance not diverted to the reserve
#' - to_treasury: the daily non-reserve budget allocated to the treasury
#' - operations: the daily non-reserve budget allocated to the operations
#' - to_resilience: the daily non-reserve budget allocated to the resilience
#' - to_stakers: the daily non-reserve budget allocated to the stakers
#' @export
get_issuance <- function(current_supply, current_issuance, today, end_date, nodes, token_price, save = FALSE, phase2.opt){

  max_supply <- 2100000000 # 2.1B DOT hard cap

  phase2_start <- phase2.opt$phase2_start
  node_reward <- phase2.opt$node_reward
  overcollateral_factor <- phase2.opt$overcollateral_factor
  to_resilience <- phase2.opt$to_resilience
  to_treasury <- phase2.opt$to_treasury

  first_halving <- as.Date("2026-03-14", format="%Y-%m-%d") #first halving supposed to happen on the 14th of Match see https://github.com/polkadot-fellows/runtimes/pull/898/changes
  halvings <- seq(first_halving, by = '2 years', to = as.Date("2161-03-14", format="%Y-%m-%d")) # reduction in annual issuance happens every 2 years

  issuance <- c(current_issuance/365) # vec for daily issuance
  supply <- c(current_supply) # vec for total supply updated daily

  today <- as.Date(today, format="%Y-%m-%d") # today's date
  next_halvings <- halvings[today < halvings] # remaining halvings

  dates <- seq(today, by = 'days', to = tail(next_halvings, n = 1)) # dates from today until last halving
  inflation_data <- data.frame(date = dates)
  inflation_data$year <- format(inflation_data$date, "%Y")
  inflation_data$month <- format(inflation_data$date, "%m")
  inflation_data$day <- format(inflation_data$date, "%d")

  for (i in 2:length(dates)){ # loop updating supply and issuance vectors

    supply[i] <- supply[i - 1] + issuance[i - 1] # supply at day i is calculated as the supply of the previous day + the daily issuance of the previous day

    if(dates[i] %in% halvings){ # cond statement for issuance reduction
      issuance[i] <- (max_supply - supply[i])*0.1314/365 # at halving dates the new issuance is the 13.14% of the remaining supply
    } else {
      issuance[i] <- issuance[i - 1] # outside halving dates daily issuance stays the same
    }

  }

  inflation_data$issuance <- issuance
  inflation_data$supply <- supply

  non_reserve <- c() # empty vec for non-reserve values

  phase2_start <- as.Date(phase2_start, format = "%Y-%m-%d") # start date for Phase II

  for (i in 1:length(dates)){

    index <- floor((dates[i] - phase2_start)[[1]]/365) # index for years since Phase II start, first year 0 and so on

    if(index < 0){
      non_reserve[i] <- 0
    } else {
      non_reserve[i] <- (40056081*(0.91^index))/365 # equation provided here https://forum.polkadot.network/t/proposal-dynamic-allocation-pool-dap/15878#:~:text=be%20calculated%20by-,C_t%20%3D%20C_0%20*%200.91%5Et,-.
    }

  }

  inflation_data$non_reserve <- non_reserve
  inflation_data$reserve <- ifelse(non_reserve == 0, 0, inflation_data$issuance - non_reserve) # reserve is the issuance - the non-reserve
  inflation_data$to_treasury <- to_treasury*non_reserve # treasury is 20.7% of the non-reserve: (55561162*0.15)/40056081

  operations_budget <- node_reward*nodes*12/token_price*overcollateral_factor # 2000 USD/month per node, see https://forum.polkadot.network/t/proposal-dynamic-allocation-pool-dap/15878#:~:text=fixed%20payment%20of-,%242000%20per%20node,-.%20As%20the%20stablecoin
  inflation_data$operations <- ifelse(non_reserve == 0, 0, operations_budget/365) # total budget for operations

  inflation_data <- mutate(inflation_data,
                           to_resilience = to_resilience*non_reserve, # assumes 5’400’000 DOT needed in year 0, 13.48% of non-reserve budget
                           to_stakers = non_reserve - to_treasury - operations - to_resilience) # stakers receive the remaining after subtracting budgets for resilience, ops and treasury from the non-reserve

  inflation_data$to_stakers[inflation_data$non_reserve == 0] <- 0.85*inflation_data$issuance[inflation_data$non_reserve == 0] # overwriting stakers budget before Phase II (85% of issuance)
  inflation_data$to_treasury[inflation_data$non_reserve == 0] <- 0.15*inflation_data$issuance[inflation_data$non_reserve == 0] # overwriting treasury budget before Phase II (15% of issuance)

  inflation_data <- inflation_data[inflation_data$date <= end_date,]

  if(save){
    write.csv(inflation_data, "inflation_data.csv", row.names = FALSE)
  }

  return(inflation_data)

}

#' Calculates Summary Statistics
#'
#' @param data Output from the `get_issuance` function
#' @param year Year to calculate the summary statistics
#' @param staking_rate A vector containing staking rates values
#' @param stock_info A list containing the held token supply (`held`) and which percentage is bonded in staking (`bonded_pct`)
#' @param token_price A vector containing token price values in USD
#' @param budget The requested annual budget in USD
#' @param nodes The total number of nodes producing blocks
#' @param phase2.opt A list of the following objects:
#' - `self_stakes` a vector for the distribution of self stakes (length must match `nodes`)
#' - `self_stake` average own self stake across multiple nodes
#' - `pct_own_nodes` percentage of own nodes
#' - `node_cost` costs per node per month (servers, electricity, etc.)
#' - `FTE` costs for full time engineers
#' - `node_reward` rewards in USD per node per month
#' - `overcollateral_factor` Over-collateralization for minting stablecoins to pay for operations
#' - `start_date` Start date of Phase II
#'
#' Default is `NULL`, meaning it assumes no Phase II.
#' @export
get_summary <- function(data, year, staking_rate, stock_info, token_price, budget, nodes, phase2.opt = NULL){

  in_staking <- stock_info$held*stock_info$bonded_pct # own tokens in staking

  out_filter <- list()

  for (i in 1:length(staking_rate)){ # loop over specified staking rates

    data_price <- list()

    for (j in 1:length(token_price)){ # loop over specified token prices

      if(!is.null(phase2.opt)){ # condition with Phase II

        self_stake <- phase2.opt$self_stake
        self_stakes <- phase2.opt$self_stakes
        pct_own_nodes <- phase2.opt$pct_own_nodes
        node_cost <- phase2.opt$node_cost
        FTE <- phase2.opt$FTE
        node_reward <- phase2.opt$node_reward
        overcollateral_factor <- phase2.opt$overcollateral_factor
        start_date <- as.Date(phase2.opt$start_date, format="%Y-%m-%d")

        output <- mutate(data,
                         total_bonded = staking_rate[i]*supply,  # total supply bonded in staking
                         avg_stake_node = total_bonded/nodes, # average total stake per active node
                         min_stake_node = 0.85*avg_stake_node, # minimum stake to get a node into the active set (~85% of the average total stake per active node)
                         nodes_active = floor(in_staking/min_stake_node), # maximum number of nodes that can be pushed into the active set with own bonded balance
                         price = token_price[j],
                         operations = ifelse(date >= start_date, node_reward*nodes*12/token_price[j]*overcollateral_factor/365, 0), # update operations based on DOT price
                         to_stakers = ifelse(date >= start_date, non_reserve - to_treasury - operations - to_resilience, 0.85*issuance), # update stakers allocation based on DOT price
                         gross_income_ops = ifelse(date >= start_date & pct_own_nodes > 0, operations*(nodes_active*pct_own_nodes/nodes), 0), # daily income from running own nodes, calculated as a percentage of the maximum number of nodes that can be pushed into the active set
                         nodes_costs = ifelse(date >= start_date & pct_own_nodes > 0, node_cost*(nodes_active*pct_own_nodes)*12/365/token_price[j], 0),
                         FTE_costs = ifelse(date >= start_date & pct_own_nodes > 0, FTE/365/token_price[j], 0),
                         income_staking = ifelse(date < start_date & date >= as.Date("2026-03-14", format="%Y-%m-%d"), 0.9*to_stakers*((in_staking - 10000*nodes_active)/total_bonded), to_stakers*(in_staking/total_bonded)))

        out_summary <- group_by(output, year) %>%
          mutate(staking_rate = round(staking_rate[i]*100, 2)) %>%
          summarize(supply = max(supply),
                    issuance = sum(issuance),
                    reserve = sum(reserve),
                    non_reserve = sum(non_reserve),
                    to_treasury = sum(to_treasury),
                    to_resilience = sum(to_resilience),
                    price = max(price),
                    to_ops = sum(operations),
                    to_stakers = sum(to_stakers),
                    staking_rate = max(staking_rate),
                    total_bonded = max(total_bonded),
                    avg_stake_node = max(avg_stake_node),
                    min_stake_node = max(min_stake_node),
                    nodes_active = min(nodes_active),
                    income_resilience = get_self_stake_rewards(self_stake = self_stake,
                                                               stakes = c(self_stakes[c(1:(nodes - nodes_active))], rep(self_stake, nodes_active)), # updates self stakes
                                                               daily_payout = to_resilience)*nodes_active,
                    income_staking = sum(income_staking),
                    nodes_costs = sum(nodes_costs),
                    FTE_costs = sum(FTE_costs), # full time engineers costs
                    gross_income_ops = sum(gross_income_ops)) %>%
          mutate(ops_costs = sprintf(
                    "\\makecell[l]{\\\\ Nodes: %s \\\\ FTEs: %s}",
                    format(nodes_costs, big.mark = ","),
                    format(FTE_costs, big.mark = ",")),
                 tot_income = gross_income_ops - nodes_costs - FTE_costs + income_staking + income_resilience, # total income that can be liquidated
                 apy = tot_income/in_staking*100,
                 rewards_USD = tot_income*token_price[j],
                 budget_gap = rewards_USD - budget,
                 from_stock = ifelse(budget_gap < 0, abs(budget_gap)/token_price[j], 0),
                 from_stock_pct = round(from_stock/stock_info$held*100, 2)
          )

      } else { # condition with no Phase II

        output <- mutate(data,
                         total_bonded = staking_rate[i]*supply, # total supply bonded in staking
                         avg_stake_node = total_bonded/nodes, # average total stake per active node
                         min_stake_node = 0.85*avg_stake_node, # minimum stake to get a node into the active set (~85% of the average total stake per active node)
                         nodes_active = floor(in_staking/min_stake_node),
                         tot_income = ifelse(date < as.Date("2026-03-14", format="%Y-%m-%d"), to_stakers*(in_staking/total_bonded), 0.9*to_stakers*((in_staking - 10000*nodes_active)/total_bonded))
                         ) # maximum number of nodes that can be pushed into the active set with own bonded balance


        out_summary <- group_by(output, year) %>%
          mutate(staking_rate = round(staking_rate[i]*100, 2)) %>%
          summarize(supply = max(supply), # total token supply by the EOY
                    issuance = sum(issuance), # total annual issuance
                    to_stakers = sum(to_stakers), # total annual issuance going to stakers
                    staking_rate = max(staking_rate),
                    total_bonded = max(total_bonded), # total supply bonded in staking by EOY
                    avg_stake_node = max(avg_stake_node),
                    min_stake_node = max(min_stake_node),
                    nodes_active = min(nodes_active),
                    tot_income = sum(tot_income)) %>% # staking income with own bonded balance (in USD) by EOY
          mutate(apy = tot_income/in_staking*100,
                 price = token_price[j],
                 rewards_USD = tot_income*token_price[j],
                 budget_gap = rewards_USD - budget, # budget gap to be filled
                 from_stock = ifelse(budget_gap < 0, abs(budget_gap)/token_price[j], 0), # tokens to be potentially liquidated
                 from_stock_pct = round(from_stock/stock_info$held*100, 2) # % to be potentially liquidated from stock
          )

      }

      out_summary <- na.omit(as.data.frame(out_summary[out_summary$year == year,])) # filter by requested year

      data_price[[j]] <- out_summary

    }

    data_price <- do.call(rbind, data_price)
    data_price <- data_price[order(data_price$rewards_USD),]

    out_filter[[i]] <- data_price

  }

  data <- as.data.frame(do.call(rbind, out_filter))

  return(data)

}

#' @export
get_self_stake_weight <- function(T = 30000, C = 100000, k = 0.5, stake){ # calculates the weight for self stake using the w(s_i) function provided here: https://forum.polkadot.network/t/proposal-dynamic-allocation-pool-dap/15878#:~:text=The%20following-,piece%2Dwise%20function,-captures%20these%20goals
# param values for T, C, and k see https://forum.polkadot.network/t/proposal-dynamic-allocation-pool-dap/15878#:~:text=requirements%20imposed%20by-,research%20on%20ELVES%3A,-T%3D30000%2C%20C
  if(stake >= 0 & stake <= T){
    w <- sqrt(stake)
  } else if (stake > T & stake <= C){
    w <- sqrt(T + k^2*(stake - T))
  } else if (stake > C) {
    w <- sqrt(T + k^2*(C - T))
  }

  return(w)

}

#' @export
get_self_stake_rewards <- function(self_stake, stakes, daily_payout){ # calculates the portion of self-stake rewards using the pi_i function provided here: https://forum.polkadot.network/t/proposal-dynamic-allocation-pool-dap/15878#:~:text=The%20following-,piece%2Dwise%20function,-captures%20these%20goals

  weight <- get_self_stake_weight(stake = self_stake)

  weights <- sapply(stakes, function(x) get_self_stake_weight(stake = x))

  reward <- daily_payout*weight/sum(weights)

  return(reward)

}

