
#CLEAN WORKSPACE AND LOAD PACKAGES
rm(list = ls())
library(apollo)
library(readr)
library(dplyr)
# 1. LOAD AND PREPARE DATA
setwd("~/Desktop")   
apollo_initialise()
database <- read_csv("Car.csv")
# Add ID: each row is one individual (no ID in original dataset)
database$ID <- 1:nrow(database)
# Convert choice string to numeric
database$choice_num <- as.numeric(sub("choice", "", database$choice))

# CAR CHOICE                                 
# Dataset: Car         
#N = 4654 individuals, one choice each                   
# 6 alternatives per choice task                                                                           
#Attributes included:                                     
#- type:     body type (categorical, baseline = regcar)   
#- fuel:     fuel type (categorical, baseline = gasoline)
#- price:    price / log(income) -> income-adjusted price  
# - range:    hundreds of miles between refueling          
# - acc:      acceleration (tens of seconds to 30mph)      
# - speed:    top speed (hundreds of mph)                  
# - pollution: tailpipe emissions as fraction of gas car   
# - size:     vehicle size (0=mini to 3=mid/large)         
# - space:    luggage space as fraction of gas car         
# - cost:     cost per mile (tens of cents)                
# - station:  fraction of stations that can refuel         
                                                     
# NOTE ON PRICE--> pricez = vehicle price / log(household income)       
# This normalisation means price sensitivity decline with income,so WTP cannot be computed directly.
# Elasticities can be computed via 1% price scenario.     
                                                         
#############################################################################################################################################################################################################################################################
#CAR DATASET - EXPLORATORY ANALYSIS                      
# Body type and fuel type assignment across alternatives   
# Goal: document the structural asymmetry in the design   
{
  library(readr)
  library(dplyr)
#1. CHOICE SHARE BY ALTERNATIVE
  choice_shares <- data.frame(
    alternative = paste0("alt", 1:6),
    chosen      = sapply(1:6, function(j) sum(database$choice_num == j)),
    available   = nrow(database)
  )
  choice_shares$share <- round(100 * choice_shares$chosen / choice_shares$available, 2)
  choice_shares
  #how many time the alternative are choosen (odds alternative choosen more)

#2. BODY TYPE DISTRIBUTION BY ALTERNATIVE
  cat("\n=== BODY TYPE DISTRIBUTION BY ALTERNATIVE ===\n")
  for(j in 1:6){
    cat(paste0("\nalt", j, ":\n"))
    tbl <- table(database[[paste0("type", j)]])
    pct <- round(100 * prop.table(tbl), 1)
    df  <- data.frame(body_type = names(tbl), count = as.integer(tbl), pct = as.numeric(pct))
    df  <- df[order(-df$count), ]
    print(df, row.names = FALSE)
  }
  #For each alternative from 1 to 6, computes how often each body type (regcar, truck, van, etc.) 
  #appears and its percentage share. This documents the structural asymmetry of 
  #the stated preference experiment design: odd alternatives are dominated by regular cars, while even 
  #alternatives are dominated by trucks, vans and station wagons.

#3. FUEL TYPE DISTRIBUTION BY ALTERNATIVE
  cat("\n=== FUEL TYPE DISTRIBUTION BY ALTERNATIVE ===\n")
  for(j in 1:6){
    cat(paste0("\nalt", j, ":\n"))
    tbl <- table(database[[paste0("fuel", j)]])
    pct <- round(100 * prop.table(tbl), 1)
    df  <- data.frame(fuel_type = names(tbl), count = as.integer(tbl), pct = as.numeric(pct))
    df  <- df[order(-df$count), ]
    print(df, row.names = FALSE)
  }
  
#4. IS FUEL IDENTICAL WITHIN PAIRS?
  cat("\n=== IS FUEL IDENTICAL WITHIN EACH PAIR? ===\n")
  pair12 <- sum(database$fuel1 == database$fuel2)
  pair34 <- sum(database$fuel3 == database$fuel4)
  pair56 <- sum(database$fuel5 == database$fuel6)
    cat("fuel1 == fuel2:", pair12, "/", nrow(database),
      paste0("(", round(100*pair12/nrow(database),1), "%)"), "\n")
  cat("fuel3 == fuel4:", pair34, "/", nrow(database),
      paste0("(", round(100*pair34/nrow(database),1), "%)"), "\n")
  cat("fuel5 == fuel6:", pair56, "/", nrow(database),
      paste0("(", round(100*pair56/nrow(database),1), "%)"), "\n")
 # IS FUEL IDENTICAL WITHIN PAIRS? --> YES

#5. ARE ALL CONTINUOUS ATTRIBUTES PAIRED?
 cat("\n=== ARE CONTINUOUS ATTRIBUTES IDENTICAL WITHIN PAIRS? ===\n")
   attrs <- c("price", "range", "acc", "speed", "pollution", "size", "space", "cost", "station")
    for(attr in attrs){
    v12 <- sum(database[[paste0(attr,"1")]] == database[[paste0(attr,"2")]])
    v34 <- sum(database[[paste0(attr,"3")]] == database[[paste0(attr,"4")]])
    v56 <- sum(database[[paste0(attr,"5")]] == database[[paste0(attr,"6")]])
    all_paired <- (v12 == nrow(database)) & (v34 == nrow(database)) & (v56 == nrow(database))
    cat(sprintf("%-10s: pair1=%d/%d  pair2=%d/%d  pair3=%d/%d  -> fully paired: %s\n",
                attr, v12, nrow(database), v34, nrow(database), v56, nrow(database),
                ifelse(all_paired, "YES", "NO")))
    }
   #ARE ALL CONTINUOUS ATTRIBUTES PAIRED?--> YES
  
#6. CROSS-TABULATION: BODY TYPE x ALTERNATIVE
  #    Shows the full picture of assignment asymmetry
  cat("\n=== CROSS-TABULATION: BODY TYPE x ALTERNATIVE (percentages) ===\n")
  all_types <- c("regcar","sportuv","sportcar","stwagon","truck","van")
  # Build matrix
  mat <- matrix(NA, nrow=length(all_types), ncol=6,
                dimnames=list(all_types, paste0("alt",1:6)))
  for(j in 1:6){
    tbl <- table(factor(database[[paste0("type",j)]], levels=all_types))
    mat[,j] <- round(100 * as.numeric(tbl) / nrow(database), 1)
  }
  print(mat)

#7. SUMMARY: ODD vs EVEN POSITION COMPARISON
  cat("\n=== ODD vs EVEN POSITION SUMMARY ===\n")
  # Average choice share
  odd_share  <- mean(choice_shares$share[c(1,3,5)])
  even_share <- mean(choice_shares$share[c(2,4,6)])
  cat(sprintf("Average choice share - odd  positions (1,3,5): %.2f%%\n", odd_share))
  cat(sprintf("Average choice share - even positions (2,4,6): %.2f%%\n", even_share))
  cat(sprintf("Ratio odd/even: %.1fx\n", odd_share/even_share))
  # regcar frequency
  for(j in 1:6){
    rc <- round(100 * mean(database[[paste0("type",j)]] == "regcar"), 1)
    cat(sprintf("alt%d - regcar share: %.1f%%\n", j, rc))
  }
  
}

#####################################################################################################################################################################################################################
#MODEL1
#Attributes included:                                     
#- type:     body type (categorical, baseline = regcar)   
#- fuel:     fuel type (categorical, baseline = gasoline)
# - price:    price / log(income) - income-adjusted price  
#- range:    hundreds of miles between refueling          
#- acc:      acceleration (tens of seconds required to reach 30 mph from stop)      
#- speed:    top speed (hundreds of mph)                  
#- pollution: tailpipe emissions as fraction of gas car   
#- size:     vehicle size (0=mini to 3=mid/large)         
# - space:    luggage space as fraction of gas car         
#- cost:     cost per mile (tens of cents)                
# - station:  fraction of stations that can refuel         
{
#2. APOLLO CONTROL
apollo_control <- list(
  modelName       = "MNL_car_full_baseline",
  modelDescr      = "Baseline MNL on full Car dataset with all attributes",
  indivID         = "ID",
  outputDirectory = "output"
)
#3. DEFINE PARAMETERS
apollo_beta <- c(

  # BODY TYPE (baseline = regcar)
  b_regcar    = 0,
  b_sportuv   = 0,
  b_sportcar  = 0,
  b_stwagon   = 0,
  b_truck     = 0,
  b_van       = 0,
  # FUEL TYPE (baseline = gasoline)
  b_gasoline  = 0,
  b_methanol  = 0,
  b_cng       = 0,
  b_electric  = 0,
  # SIZE (baseline = 0 = mini)
  b_size0     = 0,
  b_size1     = 0,
  b_size2     = 0,
  b_size3     = 0,
  # CONTINUOUS ATTRIBUTES
  b_price     = 0,   # price / log(income) - expect negative
  b_range     = 0,   # hundreds of miles between refueling - expect positive
  b_acc       = 0,   # tens of seconds to reach 30mph - expect negative (slower = worse)
  b_speed     = 0,   # hundreds of mph top speed - expect positive
  b_pollution = 0,   # emissions fraction - expect negative
  b_space     = 0,   # luggage space fraction - expect positive
  b_cost      = 0,   # cost per mile (tens of cents) - expect negative
  b_station   = 0    # fraction of stations - expect positive
)
apollo_fixed <- c("b_regcar", "b_gasoline", "b_size0")

# 4. VALIDATE INPUTS
apollo_inputs <- apollo_validateInputs()

# 5. DEFINE PROBABILITIES

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Utility of alternative 1
  V[["alt1"]] =
    # BODY TYPE
    b_regcar    * (type1 == "regcar") +
    b_sportuv   * (type1 == "sportuv") +
    b_sportcar  * (type1 == "sportcar") +
    b_stwagon   * (type1 == "stwagon") +
    b_truck     * (type1 == "truck") +
    b_van       * (type1 == "van") +
    # FUEL TYPE
    b_gasoline  * (fuel1 == "gasoline") +
    b_methanol  * (fuel1 == "methanol") +
    b_cng       * (fuel1 == "cng") +
    b_electric  * (fuel1 == "electric") +
    # SIZE
    b_size0     * (size1 == 0) +
    b_size1     * (size1 == 1) +
    b_size2     * (size1 == 2) +
    b_size3     * (size1 == 3) +
    # CONTINUOUS
    b_price     * price1 +
    b_range     * range1 +
    b_acc       * acc1 +
    b_speed     * speed1 +
    b_pollution * pollution1 +
    b_space     * space1 +
    b_cost      * cost1 +
    b_station   * station1
  

  # Utility of alternative 2
  V[["alt2"]] =
    b_regcar    * (type2 == "regcar") +
    b_sportuv   * (type2 == "sportuv") +
    b_sportcar  * (type2 == "sportcar") +
    b_stwagon   * (type2 == "stwagon") +
    b_truck     * (type2 == "truck") +
    b_van       * (type2 == "van") +
    b_gasoline  * (fuel2 == "gasoline") +
    b_methanol  * (fuel2 == "methanol") +
    b_cng       * (fuel2 == "cng") +
    b_electric  * (fuel2 == "electric") +
    b_size0     * (size2 == 0) +
    b_size1     * (size2 == 1) +
    b_size2     * (size2 == 2) +
    b_size3     * (size2 == 3) +
    b_price     * price2 +
    b_range     * range2 +
    b_acc       * acc2 +
    b_speed     * speed2 +
    b_pollution * pollution2 +
    b_space     * space2 +
    b_cost      * cost2 +
    b_station   * station2
  
  # Utility of alternative 3
  V[["alt3"]] =
    b_regcar    * (type3 == "regcar") +
    b_sportuv   * (type3 == "sportuv") +
    b_sportcar  * (type3 == "sportcar") +
    b_stwagon   * (type3 == "stwagon") +
    b_truck     * (type3 == "truck") +
    b_van       * (type3 == "van") +
    b_gasoline  * (fuel3 == "gasoline") +
    b_methanol  * (fuel3 == "methanol") +
    b_cng       * (fuel3 == "cng") +
    b_electric  * (fuel3 == "electric") +
    b_size0     * (size3 == 0) +
    b_size1     * (size3 == 1) +
    b_size2     * (size3 == 2) +
    b_size3     * (size3 == 3) +
    b_price     * price3 +
    b_range     * range3 +
    b_acc       * acc3 +
    b_speed     * speed3 +
    b_pollution * pollution3 +
    b_space     * space3 +
    b_cost      * cost3 +
    b_station   * station3
  
  # Utility of alternative 4
  V[["alt4"]] =
    b_regcar    * (type4 == "regcar") +
    b_sportuv   * (type4 == "sportuv") +
    b_sportcar  * (type4 == "sportcar") +
    b_stwagon   * (type4 == "stwagon") +
    b_truck     * (type4 == "truck") +
    b_van       * (type4 == "van") +
    b_gasoline  * (fuel4 == "gasoline") +
    b_methanol  * (fuel4 == "methanol") +
    b_cng       * (fuel4 == "cng") +
    b_electric  * (fuel4 == "electric") +
    b_size0     * (size4 == 0) +
    b_size1     * (size4 == 1) +
    b_size2     * (size4 == 2) +
    b_size3     * (size4 == 3) +
    b_price     * price4 +
    b_range     * range4 +
    b_acc       * acc4 +
    b_speed     * speed4 +
    b_pollution * pollution4 +
    b_space     * space4 +
    b_cost      * cost4 +
    b_station   * station4
  
  # Utility of alternative 5
  V[["alt5"]] =
    b_regcar    * (type5 == "regcar") +
    b_sportuv   * (type5 == "sportuv") +
    b_sportcar  * (type5 == "sportcar") +
    b_stwagon   * (type5 == "stwagon") +
    b_truck     * (type5 == "truck") +
    b_van       * (type5 == "van") +
    b_gasoline  * (fuel5 == "gasoline") +
    b_methanol  * (fuel5 == "methanol") +
    b_cng       * (fuel5 == "cng") +
    b_electric  * (fuel5 == "electric") +
    b_size0     * (size5 == 0) +
    b_size1     * (size5 == 1) +
    b_size2     * (size5 == 2) +
    b_size3     * (size5 == 3) +
    b_price     * price5 +
    b_range     * range5 +
    b_acc       * acc5 +
    b_speed     * speed5 +
    b_pollution * pollution5 +
    b_space     * space5 +
    b_cost      * cost5 +
    b_station   * station5
  
  # Utility of alternative 6
  V[["alt6"]] =
    b_regcar    * (type6 == "regcar") +
    b_sportuv   * (type6 == "sportuv") +
    b_sportcar  * (type6 == "sportcar") +
    b_stwagon   * (type6 == "stwagon") +
    b_truck     * (type6 == "truck") +
    b_van       * (type6 == "van") +
    b_gasoline  * (fuel6 == "gasoline") +
    b_methanol  * (fuel6 == "methanol") +
    b_cng       * (fuel6 == "cng") +
    b_electric  * (fuel6 == "electric") +
    b_size0     * (size6 == 0) +
    b_size1     * (size6 == 1) +
    b_size2     * (size6 == 2) +
    b_size3     * (size6 == 3) +
    b_price     * price6 +
    b_range     * range6 +
    b_acc       * acc6 +
    b_speed     * speed6 +
    b_pollution * pollution6 +
    b_space     * space6 +
    b_cost      * cost6 +
    b_station   * station6
  
  # MNL SETTINGS
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3, alt4 = 4, alt5 = 5, alt6 = 6),
    choiceVar    = choice_num,
    utilities    = V
  )
  
  # CHOICE PROBABILITIES
   P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  
  # No apollo_panelProd: one observation per individual
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# 6. ESTIMATE MODEL
model1 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# 7. OUTPUT
modelOutput_settings <- list(printPVal = 2)
apollo_modelOutput(model1, modelOutput_settings)
apollo_saveOutput(model1, modelOutput_settings)

}

#########################################################################################################################################################################################################
# MODEL 1B - BASELINE MNL WITH ASCs                      
#Same as Model 1 but adds Alternative Specific Constants (ASCs) 
#Motivation: Rho² vs observed shares was negative in M1 without ASCs, indicating that a model with only ASCs
# fits better than one with only attributes. 
#NB: alternatives are unlabelled (alt1-alt6), so ASCs do not have a direct behavioural interpretation like    
#in labelled designs. 
#Baseline alternative: alt6 (ASC fixed to 0)             
{

# 2. APOLLO CONTROL
apollo_control <- list(
  modelName       = "MNL_car_full_M1b_ASC",
  modelDescr      = "Baseline MNL with ASCs and all attributes",
  indivID         = "ID",
  outputDirectory = "output"
)

# 3. DEFINE PARAMETERS

apollo_beta <- c(

  # ASCs (baseline = alt6, fixed to 0)
  asc_alt1 = 0,
  asc_alt2 = 0,
  asc_alt3 = 0,
  asc_alt4 = 0,
  asc_alt5 = 0,
  asc_alt6 = 0,   # baseline - fixed
  
  # BODY TYPE (baseline = regcar)
  b_regcar    = 0,
  b_sportuv   = 0,
  b_sportcar  = 0,
  b_stwagon   = 0,
  b_truck     = 0,
  b_van       = 0,

  # FUEL TYPE (baseline = gasoline)
 
  b_gasoline  = 0,
  b_methanol  = 0,
  b_cng       = 0,
  b_electric  = 0,
  
  # SIZE (baseline = 0 = mini)
  b_size0     = 0,
  b_size1     = 0,
  b_size2     = 0,
  b_size3     = 0,
  
  # CONTINUOUS ATTRIBUTES
  b_price     = 0,
  b_range     = 0,
  b_acc       = 0,
  b_speed     = 0,
  b_pollution = 0,
  b_space     = 0,
  b_cost      = 0,
  b_station   = 0
)

apollo_fixed <- c("b_regcar", "b_gasoline", "b_size0", "asc_alt6")

# 4. VALIDATE INPUTS
apollo_inputs <- apollo_validateInputs()

# 5. DEFINE PROBABILITIES
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  

  # Utility of alternative 1
  V[["alt1"]] =
    asc_alt1 +
    b_regcar    * (type1 == "regcar") +
    b_sportuv   * (type1 == "sportuv") +
    b_sportcar  * (type1 == "sportcar") +
    b_stwagon   * (type1 == "stwagon") +
    b_truck     * (type1 == "truck") +
    b_van       * (type1 == "van") +
    b_gasoline  * (fuel1 == "gasoline") +
    b_methanol  * (fuel1 == "methanol") +
    b_cng       * (fuel1 == "cng") +
    b_electric  * (fuel1 == "electric") +
    b_size0     * (size1 == 0) +
    b_size1     * (size1 == 1) +
    b_size2     * (size1 == 2) +
    b_size3     * (size1 == 3) +
    b_price     * price1 +
    b_range     * range1 +
    b_acc       * acc1 +
    b_speed     * speed1 +
    b_pollution * pollution1 +
    b_space     * space1 +
    b_cost      * cost1 +
    b_station   * station1

  # Utility of alternative 2
  V[["alt2"]] =
    asc_alt2 +
    b_regcar    * (type2 == "regcar") +
    b_sportuv   * (type2 == "sportuv") +
    b_sportcar  * (type2 == "sportcar") +
    b_stwagon   * (type2 == "stwagon") +
    b_truck     * (type2 == "truck") +
    b_van       * (type2 == "van") +
    b_gasoline  * (fuel2 == "gasoline") +
    b_methanol  * (fuel2 == "methanol") +
    b_cng       * (fuel2 == "cng") +
    b_electric  * (fuel2 == "electric") +
    b_size0     * (size2 == 0) +
    b_size1     * (size2 == 1) +
    b_size2     * (size2 == 2) +
    b_size3     * (size2 == 3) +
    b_price     * price2 +
    b_range     * range2 +
    b_acc       * acc2 +
    b_speed     * speed2 +
    b_pollution * pollution2 +
    b_space     * space2 +
    b_cost      * cost2 +
    b_station   * station2
  
  # Utility of alternative 3
  V[["alt3"]] =
    asc_alt3 +
    b_regcar    * (type3 == "regcar") +
    b_sportuv   * (type3 == "sportuv") +
    b_sportcar  * (type3 == "sportcar") +
    b_stwagon   * (type3 == "stwagon") +
    b_truck     * (type3 == "truck") +
    b_van       * (type3 == "van") +
    b_gasoline  * (fuel3 == "gasoline") +
    b_methanol  * (fuel3 == "methanol") +
    b_cng       * (fuel3 == "cng") +
    b_electric  * (fuel3 == "electric") +
    b_size0     * (size3 == 0) +
    b_size1     * (size3 == 1) +
    b_size2     * (size3 == 2) +
    b_size3     * (size3 == 3) +
    b_price     * price3 +
    b_range     * range3 +
    b_acc       * acc3 +
    b_speed     * speed3 +
    b_pollution * pollution3 +
    b_space     * space3 +
    b_cost      * cost3 +
    b_station   * station3
  
  # Utility of alternative 4
  V[["alt4"]] =
    asc_alt4 +
    b_regcar    * (type4 == "regcar") +
    b_sportuv   * (type4 == "sportuv") +
    b_sportcar  * (type4 == "sportcar") +
    b_stwagon   * (type4 == "stwagon") +
    b_truck     * (type4 == "truck") +
    b_van       * (type4 == "van") +
    b_gasoline  * (fuel4 == "gasoline") +
    b_methanol  * (fuel4 == "methanol") +
    b_cng       * (fuel4 == "cng") +
    b_electric  * (fuel4 == "electric") +
    b_size0     * (size4 == 0) +
    b_size1     * (size4 == 1) +
    b_size2     * (size4 == 2) +
    b_size3     * (size4 == 3) +
    b_price     * price4 +
    b_range     * range4 +
    b_acc       * acc4 +
    b_speed     * speed4 +
    b_pollution * pollution4 +
    b_space     * space4 +
    b_cost      * cost4 +
    b_station   * station4
  

  # Utility of alternative 5
  V[["alt5"]] =
    asc_alt5 +
    b_regcar    * (type5 == "regcar") +
    b_sportuv   * (type5 == "sportuv") +
    b_sportcar  * (type5 == "sportcar") +
    b_stwagon   * (type5 == "stwagon") +
    b_truck     * (type5 == "truck") +
    b_van       * (type5 == "van") +
    b_gasoline  * (fuel5 == "gasoline") +
    b_methanol  * (fuel5 == "methanol") +
    b_cng       * (fuel5 == "cng") +
    b_electric  * (fuel5 == "electric") +
    b_size0     * (size5 == 0) +
    b_size1     * (size5 == 1) +
    b_size2     * (size5 == 2) +
    b_size3     * (size5 == 3) +
    b_price     * price5 +
    b_range     * range5 +
    b_acc       * acc5 +
    b_speed     * speed5 +
    b_pollution * pollution5 +
    b_space     * space5 +
    b_cost      * cost5 +
    b_station   * station5
  
  # Utility of alternative 6
  V[["alt6"]] =
    asc_alt6 +
    b_regcar    * (type6 == "regcar") +
    b_sportuv   * (type6 == "sportuv") +
    b_sportcar  * (type6 == "sportcar") +
    b_stwagon   * (type6 == "stwagon") +
    b_truck     * (type6 == "truck") +
    b_van       * (type6 == "van") +
    b_gasoline  * (fuel6 == "gasoline") +
    b_methanol  * (fuel6 == "methanol") +
    b_cng       * (fuel6 == "cng") +
    b_electric  * (fuel6 == "electric") +
    b_size0     * (size6 == 0) +
    b_size1     * (size6 == 1) +
    b_size2     * (size6 == 2) +
    b_size3     * (size6 == 3) +
    b_price     * price6 +
    b_range     * range6 +
    b_acc       * acc6 +
    b_speed     * speed6 +
    b_pollution * pollution6 +
    b_space     * space6 +
    b_cost      * cost6 +
    b_station   * station6
  
  # MNL SETTINGS
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3, alt4 = 4, alt5 = 5, alt6 = 6),
    choiceVar    = choice_num,
    utilities    = V
  )
  

  # CHOICE PROBABILITIES
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# 6. ESTIMATE MODEL
model1B <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# 7. OUTPUT
modelOutput_settings <- list(printPVal = 2)
apollo_modelOutput(model1B, modelOutput_settings)
apollo_saveOutput(model1B, modelOutput_settings)

}

# #########################################################################################################################################################################################################
#MODEL 2 - MNL WITH BODY-TYPE HOUSEHOLD                 
# ADD INTERACTIONS (i tried other model with different interaction that were theoretically correct but no significance, so at the end i keep those)                                            
##1. pollution x college                                
#Hypothesis: 
#college-educated respondents are mores sensitive to tailpipe emissions                     
#stwagon x hsg2: respondents in hsg2 = 1 are relatively more favorable toward station wagons (it's usually seen as family car)
# van x hsg2: respondents in hsg2 = 1 are relatively more favorable toward vans (big car that can also be used in a family)
#body-type heterogeneity linked to household composition.                
{

# 2. APOLLO CONTROL
apollo_control <- list(
  modelName       = "MNL_car_full_M5_interactions",
  modelDescr      = "MNL with ASCs, pollution x college, stwagon x hsg2 and van x hsg2",
  indivID         = "ID",
  outputDirectory = "output"
)

# 3. DEFINE PARAMETERS

apollo_beta <- c(
  # ASCs (baseline = alt6, fixed to 0)
  asc_alt1 = 0,
  asc_alt2 = 0,
  asc_alt3 = 0,
  asc_alt4 = 0,
  asc_alt5 = 0,
  asc_alt6 = 0,
  
  # BODY TYPE (baseline = regcar)
  b_regcar    = 0,
  b_sportuv   = 0,
  b_sportcar  = 0,
  b_stwagon   = 0,
  b_truck     = 0,
  b_van       = 0,
  
  # FUEL TYPE (baseline = gasoline)
  b_gasoline  = 0,
  b_methanol  = 0,
  b_cng       = 0,
  b_electric  = 0,
  
  # SIZE (baseline = 0 = mini)
  b_size0     = 0,
  b_size1     = 0,
  b_size2     = 0,
  b_size3     = 0,
  
  # CONTINUOUS ATTRIBUTES
  b_price     = 0,
  b_range     = 0,
  b_acc       = 0,
  b_speed     = 0,
  b_pollution = 0,
  b_space     = 0,
  b_cost      = 0,
  b_station   = 0,
  
  # INTERACTIONS
  b_pollution_college = 0,   
  b_stwagon_hsg2      = 0,   
  b_van_hsg2          = 0    
)

apollo_fixed <- c("b_regcar", "b_gasoline", "b_size0", "asc_alt6")

# 4. VALIDATE INPUTS

apollo_inputs <- apollo_validateInputs()

# 5. DEFINE PROBABILITIES
apollo_probabilities_m2 <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()

  # SYSTEMATIC TASTE HETEROGENEITY THROUGH OBSERVED INTERACTIONS
  # Pollution sensitivity: higher for college-educated respondents
  b_pollution_eff <- b_pollution + b_pollution_college * college
  
  # body-type utility for hsg2 = 1 respondents
  b_stwagon_eff   <- b_stwagon   + b_stwagon_hsg2 * hsg2
  b_van_eff       <- b_van       + b_van_hsg2     * hsg2
  
  # Utility of alternative 1
  V[["alt1"]] =
    asc_alt1 +
    b_regcar      * (type1 == "regcar") +
    b_sportuv     * (type1 == "sportuv") +
    b_sportcar    * (type1 == "sportcar") +
    b_stwagon_eff * (type1 == "stwagon") +
    b_truck       * (type1 == "truck") +
    b_van_eff     * (type1 == "van") +
    b_gasoline    * (fuel1 == "gasoline") +
    b_methanol    * (fuel1 == "methanol") +
    b_cng         * (fuel1 == "cng") +
    b_electric    * (fuel1 == "electric") +
    b_size0       * (size1 == 0) +
    b_size1       * (size1 == 1) +
    b_size2       * (size1 == 2) +
    b_size3       * (size1 == 3) +
    b_price         * price1 +
    b_range         * range1 +
    b_acc           * acc1 +
    b_speed         * speed1 +
    b_pollution_eff * pollution1 +
    b_space         * space1 +
    b_cost          * cost1 +
    b_station       * station1
  
  # Utility of alternative 2
  V[["alt2"]] =
    asc_alt2 +
    b_regcar      * (type2 == "regcar") +
    b_sportuv     * (type2 == "sportuv") +
    b_sportcar    * (type2 == "sportcar") +
    b_stwagon_eff * (type2 == "stwagon") +
    b_truck       * (type2 == "truck") +
    b_van_eff     * (type2 == "van") +
    b_gasoline    * (fuel2 == "gasoline") +
    b_methanol    * (fuel2 == "methanol") +
    b_cng         * (fuel2 == "cng") +
    b_electric    * (fuel2 == "electric") +
    b_size0       * (size2 == 0) +
    b_size1       * (size2 == 1) +
    b_size2       * (size2 == 2) +
    b_size3       * (size2 == 3) +
    b_price         * price2 +
    b_range         * range2 +
    b_acc           * acc2 +
    b_speed         * speed2 +
    b_pollution_eff * pollution2 +
    b_space         * space2 +
    b_cost          * cost2 +
    b_station       * station2

  # Utility of alternative 3
  V[["alt3"]] =
    asc_alt3 +
    b_regcar      * (type3 == "regcar") +
    b_sportuv     * (type3 == "sportuv") +
    b_sportcar    * (type3 == "sportcar") +
    b_stwagon_eff * (type3 == "stwagon") +
    b_truck       * (type3 == "truck") +
    b_van_eff     * (type3 == "van") +
    b_gasoline    * (fuel3 == "gasoline") +
    b_methanol    * (fuel3 == "methanol") +
    b_cng         * (fuel3 == "cng") +
    b_electric    * (fuel3 == "electric") +
    b_size0       * (size3 == 0) +
    b_size1       * (size3 == 1) +
    b_size2       * (size3 == 2) +
    b_size3       * (size3 == 3) +
    b_price         * price3 +
    b_range         * range3 +
    b_acc           * acc3 +
    b_speed         * speed3 +
    b_pollution_eff * pollution3 +
    b_space         * space3 +
    b_cost          * cost3 +
    b_station       * station3
  
  # Utility of alternative 4
  V[["alt4"]] =
    asc_alt4 +
    b_regcar      * (type4 == "regcar") +
    b_sportuv     * (type4 == "sportuv") +
    b_sportcar    * (type4 == "sportcar") +
    b_stwagon_eff * (type4 == "stwagon") +
    b_truck       * (type4 == "truck") +
    b_van_eff     * (type4 == "van") +
    b_gasoline    * (fuel4 == "gasoline") +
    b_methanol    * (fuel4 == "methanol") +
    b_cng         * (fuel4 == "cng") +
    b_electric    * (fuel4 == "electric") +
    b_size0       * (size4 == 0) +
    b_size1       * (size4 == 1) +
    b_size2       * (size4 == 2) +
    b_size3       * (size4 == 3) +
    b_price         * price4 +
    b_range         * range4 +
    b_acc           * acc4 +
    b_speed         * speed4 +
    b_pollution_eff * pollution4 +
    b_space         * space4 +
    b_cost          * cost4 +
    b_station       * station4
  
  # Utility of alternative 5
  V[["alt5"]] =
    asc_alt5 +
    b_regcar      * (type5 == "regcar") +
    b_sportuv     * (type5 == "sportuv") +
    b_sportcar    * (type5 == "sportcar") +
    b_stwagon_eff * (type5 == "stwagon") +
    b_truck       * (type5 == "truck") +
    b_van_eff     * (type5 == "van") +
    b_gasoline    * (fuel5 == "gasoline") +
    b_methanol    * (fuel5 == "methanol") +
    b_cng         * (fuel5 == "cng") +
    b_electric    * (fuel5 == "electric") +
    b_size0       * (size5 == 0) +
    b_size1       * (size5 == 1) +
    b_size2       * (size5 == 2) +
    b_size3       * (size5 == 3) +
    b_price         * price5 +
    b_range         * range5 +
    b_acc           * acc5 +
    b_speed         * speed5 +
    b_pollution_eff * pollution5 +
    b_space         * space5 +
    b_cost          * cost5 +
    b_station       * station5
  
  # Utility of alternative 6
  V[["alt6"]] =
    asc_alt6 +
    b_regcar      * (type6 == "regcar") +
    b_sportuv     * (type6 == "sportuv") +
    b_sportcar    * (type6 == "sportcar") +
    b_stwagon_eff * (type6 == "stwagon") +
    b_truck       * (type6 == "truck") +
    b_van_eff     * (type6 == "van") +
    b_gasoline    * (fuel6 == "gasoline") +
    b_methanol    * (fuel6 == "methanol") +
    b_cng         * (fuel6 == "cng") +
    b_electric    * (fuel6 == "electric") +
    b_size0       * (size6 == 0) +
    b_size1       * (size6 == 1) +
    b_size2       * (size6 == 2) +
    b_size3       * (size6 == 3) +
    b_price         * price6 +
    b_range         * range6 +
    b_acc           * acc6 +
    b_speed         * speed6 +
    b_pollution_eff * pollution6 +
    b_space         * space6 +
    b_cost          * cost6 +
    b_station       * station6
  
  # MNL SETTINGS
  
  mnl_settings <- list(
    alternatives = c(alt1=1, alt2=2, alt3=3, alt4=4, alt5=5, alt6=6),
    choiceVar    = choice_num,
    utilities    = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# 6. ESTIMATE MODEL
model2 <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities_m2,
  apollo_inputs
)

# 7. OUTPUT

modelOutput_settings <- list(printPVal = 2)
apollo_modelOutput(model2, modelOutput_settings)
apollo_saveOutput(model2, modelOutput_settings)


}

#########################################################################################################################################################################################################
#MIXED LOGIT: RANDOM COST  (NORMAL)              
#Same structure as the final mixed logit, but with a NORMAL random coefficient for cost instead of       
#a negative log-normal.                                   
#This version is useful to compute the share of individuals with negative / positive cost coefficient.
{
# 2. APOLLO CONTROL

apollo_control = list(
  modelName       = "Mixed_logit_car_cost_normal",
  modelDescr      = "Mixed logit with normal random cost ",
  indivID         = "ID",
  outputDirectory = "output",
  mixing          = TRUE,
  nCores          = 2
)

# 3. DEFINE PARAMETERS
apollo_beta = c(

  # ASCs (baseline = alt6, fixed to 0)
  asc_alt1 = 0,
  asc_alt2 = 0,
  asc_alt3 = 0,
  asc_alt4 = 0,
  asc_alt5 = 0,
  asc_alt6 = 0,
  
  # RANDOM COEFFICIENT: COST 
  mu_b_cost    = 0,
  sigma_b_cost = 0,
  
  # BODY TYPE (baseline = regcar)
  b_regcar    = 0,
  b_sportuv   = 0,
  b_sportcar  = 0,
  b_stwagon   = 0,
  b_truck     = 0,
  b_van       = 0,
  
  # FUEL TYPE (baseline = gasoline)
  b_gasoline  = 0,
  b_methanol  = 0,
  b_cng       = 0,
  b_electric  = 0,
  

  # SIZE (baseline = 0 = mini)
  b_size0     = 0,
  b_size1     = 0,
  b_size2     = 0,
  b_size3     = 0,
  
  # FIXED CONTINUOUS ATTRIBUTES
  b_price     = 0,
  b_range     = 0,
  b_acc       = 0,
  b_speed     = 0,
  b_pollution = 0,
  b_space     = 0,
  b_station   = 0,
  
  # HETEROGENEITY INTERACTIONS (kept from M2)
  b_pollution_college = 0,
  b_stwagon_hsg2      = 0,
  b_van_hsg2          = 0
)

apollo_fixed = c("b_regcar", "b_gasoline", "b_size0", "asc_alt6")


# 4. SIMULATION DRAWS
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 200,
  interUnifDraws = c(),
  interNormDraws = c("draws_cost")
)


# 5. RANDOM COEFFICIENTS


apollo_randCoeff = function(apollo_beta, apollo_inputs){
  
  randcoeff = list()
  # Normal random cost coefficient
  randcoeff[["b_cost_r"]] = mu_b_cost + sigma_b_cost * draws_cost
  return(randcoeff)
}

# If sigma is estimated negative, the effective standard deviation is abs(sigma), as in the lab.

# 6. VALIDATE INPUTS
apollo_inputs = apollo_validateInputs()


# 7. DEFINE PROBABILITIES
apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  V = list()
  

  # OBSERVED + RANDOM HETEROGENEITY
  
  # Pollution
  b_pollution_eff = b_pollution + b_pollution_college * college
  # Body-type interactions from M2
  b_stwagon_eff   = b_stwagon + b_stwagon_hsg2 * hsg2
  b_van_eff       = b_van     + b_van_hsg2     * hsg2
  # Random cost coefficient
  b_cost_eff      = b_cost_r
  

  # Utility of alternative 1
  V[["alt1"]] =
    asc_alt1 +
    b_regcar      * (type1 == "regcar") +
    b_sportuv     * (type1 == "sportuv") +
    b_sportcar    * (type1 == "sportcar") +
    b_stwagon_eff * (type1 == "stwagon") +
    b_truck       * (type1 == "truck") +
    b_van_eff     * (type1 == "van") +
    b_gasoline    * (fuel1 == "gasoline") +
    b_methanol    * (fuel1 == "methanol") +
    b_cng         * (fuel1 == "cng") +
    b_electric    * (fuel1 == "electric") +
    b_size0       * (size1 == 0) +
    b_size1       * (size1 == 1) +
    b_size2       * (size1 == 2) +
    b_size3       * (size1 == 3) +
    b_price         * price1 +
    b_range         * range1 +
    b_acc           * acc1 +
    b_speed         * speed1 +
    b_pollution_eff * pollution1 +
    b_space         * space1 +
    b_cost_eff      * cost1 +
    b_station       * station1
  
  
  # Utility of alternative 2
  V[["alt2"]] =
    asc_alt2 +
    b_regcar      * (type2 == "regcar") +
    b_sportuv     * (type2 == "sportuv") +
    b_sportcar    * (type2 == "sportcar") +
    b_stwagon_eff * (type2 == "stwagon") +
    b_truck       * (type2 == "truck") +
    b_van_eff     * (type2 == "van") +
    b_gasoline    * (fuel2 == "gasoline") +
    b_methanol    * (fuel2 == "methanol") +
    b_cng         * (fuel2 == "cng") +
    b_electric    * (fuel2 == "electric") +
    b_size0       * (size2 == 0) +
    b_size1       * (size2 == 1) +
    b_size2       * (size2 == 2) +
    b_size3       * (size2 == 3) +
    b_price         * price2 +
    b_range         * range2 +
    b_acc           * acc2 +
    b_speed         * speed2 +
    b_pollution_eff * pollution2 +
    b_space         * space2 +
    b_cost_eff      * cost2 +
    b_station       * station2
  

  # Utility of alternative 3
  V[["alt3"]] =
    asc_alt3 +
    b_regcar      * (type3 == "regcar") +
    b_sportuv     * (type3 == "sportuv") +
    b_sportcar    * (type3 == "sportcar") +
    b_stwagon_eff * (type3 == "stwagon") +
    b_truck       * (type3 == "truck") +
    b_van_eff     * (type3 == "van") +
    b_gasoline    * (fuel3 == "gasoline") +
    b_methanol    * (fuel3 == "methanol") +
    b_cng         * (fuel3 == "cng") +
    b_electric    * (fuel3 == "electric") +
    b_size0       * (size3 == 0) +
    b_size1       * (size3 == 1) +
    b_size2       * (size3 == 2) +
    b_size3       * (size3 == 3) +
    b_price         * price3 +
    b_range         * range3 +
    b_acc           * acc3 +
    b_speed         * speed3 +
    b_pollution_eff * pollution3 +
    b_space         * space3 +
    b_cost_eff      * cost3 +
    b_station       * station3
  

  # Utility of alternative 4
  V[["alt4"]] =
    asc_alt4 +
    b_regcar      * (type4 == "regcar") +
    b_sportuv     * (type4 == "sportuv") +
    b_sportcar    * (type4 == "sportcar") +
    b_stwagon_eff * (type4 == "stwagon") +
    b_truck       * (type4 == "truck") +
    b_van_eff     * (type4 == "van") +
    b_gasoline    * (fuel4 == "gasoline") +
    b_methanol    * (fuel4 == "methanol") +
    b_cng         * (fuel4 == "cng") +
    b_electric    * (fuel4 == "electric") +
    b_size0       * (size4 == 0) +
    b_size1       * (size4 == 1) +
    b_size2       * (size4 == 2) +
    b_size3       * (size4 == 3) +
    b_price         * price4 +
    b_range         * range4 +
    b_acc           * acc4 +
    b_speed         * speed4 +
    b_pollution_eff * pollution4 +
    b_space         * space4 +
    b_cost_eff      * cost4 +
    b_station       * station4
  
  # Utility of alternative 5
  V[["alt5"]] =
    asc_alt5 +
    b_regcar      * (type5 == "regcar") +
    b_sportuv     * (type5 == "sportuv") +
    b_sportcar    * (type5 == "sportcar") +
    b_stwagon_eff * (type5 == "stwagon") +
    b_truck       * (type5 == "truck") +
    b_van_eff     * (type5 == "van") +
    b_gasoline    * (fuel5 == "gasoline") +
    b_methanol    * (fuel5 == "methanol") +
    b_cng         * (fuel5 == "cng") +
    b_electric    * (fuel5 == "electric") +
    b_size0       * (size5 == 0) +
    b_size1       * (size5 == 1) +
    b_size2       * (size5 == 2) +
    b_size3       * (size5 == 3) +
    b_price         * price5 +
    b_range         * range5 +
    b_acc           * acc5 +
    b_speed         * speed5 +
    b_pollution_eff * pollution5 +
    b_space         * space5 +
    b_cost_eff      * cost5 +
    b_station       * station5
  
  # Utility of alternative 6
  V[["alt6"]] =
    asc_alt6 +
    b_regcar      * (type6 == "regcar") +
    b_sportuv     * (type6 == "sportuv") +
    b_sportcar    * (type6 == "sportcar") +
    b_stwagon_eff * (type6 == "stwagon") +
    b_truck       * (type6 == "truck") +
    b_van_eff     * (type6 == "van") +
    b_gasoline    * (fuel6 == "gasoline") +
    b_methanol    * (fuel6 == "methanol") +
    b_cng         * (fuel6 == "cng") +
    b_electric    * (fuel6 == "electric") +
    b_size0       * (size6 == 0) +
    b_size1       * (size6 == 1) +
    b_size2       * (size6 == 2) +
    b_size3       * (size6 == 3) +
    b_price         * price6 +
    b_range         * range6 +
    b_acc           * acc6 +
    b_speed         * speed6 +
    b_pollution_eff * pollution6 +
    b_space         * space6 +
    b_cost_eff      * cost6 +
    b_station       * station6
  
  # MNL SETTINGS
  mnl_settings = list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3, alt4 = 4, alt5 = 5, alt6 = 6),
    choiceVar    = choice_num,
    utilities    = V
  )
  
  # CHOICE PROBABILITIES
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  # Cross-section: no apollo_panelProd
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}


# 8. ESTIMATE MODEL
model_cost_normal = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

# 9. OUTPUT
modelOutput_settings = list(printPVal = 2)
apollo_modelOutput(model_cost_normal, modelOutput_settings)
apollo_saveOutput(model_cost_normal, modelOutput_settings)

#SHARE OF NEGATIVE / POSITIVE INDIVIDUAL BETAS          
model3A = model_cost_normal
# Share of people with beta < 0 for travel cost
share_neg_cost = pnorm(
  0,
  mean = model3A[["estimate"]][["mu_b_cost"]],
  sd   = abs(model3A[["estimate"]][["sigma_b_cost"]])
)

# Share of people with beta > 0 for travel cost
share_pos_cost = 1 - share_neg_cost

share_neg_cost
share_pos_cost #--> too high, impose a negative log-normal


# under the NORMAL assumption, a non-negligible minority  have a positive cost coefficient
# this is one reason why cost is  modeled as negative log-normal  in the preferred final specification

}

##########################################################################################################################################################################################################                              ####
#Model 3b. FINAL MIXED LOGIT
# Keeps the significant observed heterogeneity from M3 (pollution x college, stwagon x hsg2, van x hsg2) + cost ->negative log-normal
#Motivation:
#- cost is the only coefficient that consistently showed significant random heterogeneity across individuals
#- negative log-normal ensures a negative cost effect for all individuals

{

# 2. APOLLO CONTROL
apollo_control = list(
  modelName       = "Mixed_logit_car_final_costlog",
  modelDescr      = "Mixed logit with negative log-normal cost only",
  indivID         = "ID",
  outputDirectory = "output",
  mixing          = TRUE,
  nCores          = 2
)

# 3. DEFINE PARAMETERS

apollo_beta = c(
  # ASCs (baseline = alt6, fixed to 0)
  asc_alt1 = 0,
  asc_alt2 = 0,
  asc_alt3 = 0,
  asc_alt4 = 0,
  asc_alt5 = 0,
  asc_alt6 = 0,

  # RANDOM COEFFICIENT
  # Cost: negative log-normal
  mu_log_b_cost    = 0,
  sigma_log_b_cost = 0,
  
  # BODY TYPE (baseline = regcar)
  b_regcar    = 0,
  b_sportuv   = 0,
  b_sportcar  = 0,
  b_stwagon   = 0,
  b_truck     = 0,
  b_van       = 0,
  
  # FUEL TYPE (baseline = gasoline)
  b_gasoline  = 0,
  b_methanol  = 0,
  b_cng       = 0,
  b_electric  = 0,
  
  # SIZE (baseline = 0 = mini)
  b_size0     = 0,
  b_size1     = 0,
  b_size2     = 0,
  b_size3     = 0,
  
  # FIXED CONTINUOUS ATTRIBUTES
  b_price     = 0,
  b_range     = 0,
  b_acc       = 0,
  b_speed     = 0,
  b_pollution = 0,
  b_space     = 0,
  b_station   = 0,
  
  # OBSERVED HETEROGENEITY INTERACTIONS (kept from M5)
  b_pollution_college = 0,
  b_stwagon_hsg2      = 0,
  b_van_hsg2          = 0
)

apollo_fixed = c("b_regcar", "b_gasoline", "b_size0", "asc_alt6")

# 4. SIMULATION DRAWS
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 200,
  interUnifDraws = c(),
  interNormDraws = c("draws_cost")
)

# Notes:
# - cost uses a negative log-normal coefficient
# - this guarantees a negative cost effect for every individual

# 5. RANDOM COEFFICIENTS
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  
  randcoeff = list()
  
  # Negative log-normal cost coefficient
  # Always negative for every individual
  randcoeff[["b_cost_r"]] =
    -exp(mu_log_b_cost + sigma_log_b_cost * draws_cost)
  
  return(randcoeff)
}

# 6. VALIDATE INPUTS
apollo_inputs = apollo_validateInputs()

# 7. DEFINE PROBABILITIES
apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  V = list()
  
  # Pollution interactions
  b_pollution_eff = b_pollution + b_pollution_college * college
   # Body-type interactions 
  b_stwagon_eff   = b_stwagon + b_stwagon_hsg2 * hsg2
  b_van_eff       = b_van     + b_van_hsg2     * hsg2
  # Random cost coefficient
  b_cost_eff      = b_cost_r

  # Utility of alternative 1
  V[["alt1"]] =
    asc_alt1 +
    b_regcar      * (type1 == "regcar") +
    b_sportuv     * (type1 == "sportuv") +
    b_sportcar    * (type1 == "sportcar") +
    b_stwagon_eff * (type1 == "stwagon") +
    b_truck       * (type1 == "truck") +
    b_van_eff     * (type1 == "van") +
    b_gasoline    * (fuel1 == "gasoline") +
    b_methanol    * (fuel1 == "methanol") +
    b_cng         * (fuel1 == "cng") +
    b_electric    * (fuel1 == "electric") +
    b_size0       * (size1 == 0) +
    b_size1       * (size1 == 1) +
    b_size2       * (size1 == 2) +
    b_size3       * (size1 == 3) +
    b_price         * price1 +
    b_range         * range1 +
    b_acc           * acc1 +
    b_speed         * speed1 +
    b_pollution_eff * pollution1 +
    b_space         * space1 +
    b_cost_eff      * cost1 +
    b_station       * station1
  
  # Utility of alternative 2
  V[["alt2"]] =
    asc_alt2 +
    b_regcar      * (type2 == "regcar") +
    b_sportuv     * (type2 == "sportuv") +
    b_sportcar    * (type2 == "sportcar") +
    b_stwagon_eff * (type2 == "stwagon") +
    b_truck       * (type2 == "truck") +
    b_van_eff     * (type2 == "van") +
    b_gasoline    * (fuel2 == "gasoline") +
    b_methanol    * (fuel2 == "methanol") +
    b_cng         * (fuel2 == "cng") +
    b_electric    * (fuel2 == "electric") +
    b_size0       * (size2 == 0) +
    b_size1       * (size2 == 1) +
    b_size2       * (size2 == 2) +
    b_size3       * (size2 == 3) +
    b_price         * price2 +
    b_range         * range2 +
    b_acc           * acc2 +
    b_speed         * speed2 +
    b_pollution_eff * pollution2 +
    b_space         * space2 +
    b_cost_eff      * cost2 +
    b_station       * station2
  

  # Utility of alternative 3
  V[["alt3"]] =
    asc_alt3 +
    b_regcar      * (type3 == "regcar") +
    b_sportuv     * (type3 == "sportuv") +
    b_sportcar    * (type3 == "sportcar") +
    b_stwagon_eff * (type3 == "stwagon") +
    b_truck       * (type3 == "truck") +
    b_van_eff     * (type3 == "van") +
    b_gasoline    * (fuel3 == "gasoline") +
    b_methanol    * (fuel3 == "methanol") +
    b_cng         * (fuel3 == "cng") +
    b_electric    * (fuel3 == "electric") +
    b_size0       * (size3 == 0) +
    b_size1       * (size3 == 1) +
    b_size2       * (size3 == 2) +
    b_size3       * (size3 == 3) +
    b_price         * price3 +
    b_range         * range3 +
    b_acc           * acc3 +
    b_speed         * speed3 +
    b_pollution_eff * pollution3 +
    b_space         * space3 +
    b_cost_eff      * cost3 +
    b_station       * station3
  
  # Utility of alternative 4
  V[["alt4"]] =
    asc_alt4 +
    b_regcar      * (type4 == "regcar") +
    b_sportuv     * (type4 == "sportuv") +
    b_sportcar    * (type4 == "sportcar") +
    b_stwagon_eff * (type4 == "stwagon") +
    b_truck       * (type4 == "truck") +
    b_van_eff     * (type4 == "van") +
    b_gasoline    * (fuel4 == "gasoline") +
    b_methanol    * (fuel4 == "methanol") +
    b_cng         * (fuel4 == "cng") +
    b_electric    * (fuel4 == "electric") +
    b_size0       * (size4 == 0) +
    b_size1       * (size4 == 1) +
    b_size2       * (size4 == 2) +
    b_size3       * (size4 == 3) +
    b_price         * price4 +
    b_range         * range4 +
    b_acc           * acc4 +
    b_speed         * speed4 +
    b_pollution_eff * pollution4 +
    b_space         * space4 +
    b_cost_eff      * cost4 +
    b_station       * station4
  
  # Utility of alternative 5
  V[["alt5"]] =
    asc_alt5 +
    b_regcar      * (type5 == "regcar") +
    b_sportuv     * (type5 == "sportuv") +
    b_sportcar    * (type5 == "sportcar") +
    b_stwagon_eff * (type5 == "stwagon") +
    b_truck       * (type5 == "truck") +
    b_van_eff     * (type5 == "van") +
    b_gasoline    * (fuel5 == "gasoline") +
    b_methanol    * (fuel5 == "methanol") +
    b_cng         * (fuel5 == "cng") +
    b_electric    * (fuel5 == "electric") +
    b_size0       * (size5 == 0) +
    b_size1       * (size5 == 1) +
    b_size2       * (size5 == 2) +
    b_size3       * (size5 == 3) +
    b_price         * price5 +
    b_range         * range5 +
    b_acc           * acc5 +
    b_speed         * speed5 +
    b_pollution_eff * pollution5 +
    b_space         * space5 +
    b_cost_eff      * cost5 +
    b_station       * station5
  
  # Utility of alternative 6
  V[["alt6"]] =
    asc_alt6 +
    b_regcar      * (type6 == "regcar") +
    b_sportuv     * (type6 == "sportuv") +
    b_sportcar    * (type6 == "sportcar") +
    b_stwagon_eff * (type6 == "stwagon") +
    b_truck       * (type6 == "truck") +
    b_van_eff     * (type6 == "van") +
    b_gasoline    * (fuel6 == "gasoline") +
    b_methanol    * (fuel6 == "methanol") +
    b_cng         * (fuel6 == "cng") +
    b_electric    * (fuel6 == "electric") +
    b_size0       * (size6 == 0) +
    b_size1       * (size6 == 1) +
    b_size2       * (size6 == 2) +
    b_size3       * (size6 == 3) +
    b_price         * price6 +
    b_range         * range6 +
    b_acc           * acc6 +
    b_speed         * speed6 +
    b_pollution_eff * pollution6 +
    b_space         * space6 +
    b_cost_eff      * cost6 +
    b_station       * station6
  
  # MNL SETTINGS
  mnl_settings = list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3, alt4 = 4, alt5 = 5, alt6 = 6),
    choiceVar    = choice_num,
    utilities    = V
  )
  
  # CHOICE PROBABILITIES
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  # Cross-section: no apollo_panelProd
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}

# 8. ESTIMATE MODEL

model_3B = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

# 9. OUTPUT
modelOutput_settings = list(printPVal = 2)
apollo_modelOutput(model_3B, modelOutput_settings)
apollo_saveOutput(model_3B, modelOutput_settings)
}

# 1. COST COEFFICIENT
# In the final model, cost is specified as NEGATIVE LOG-NORMAL:
# b_cost_n = -exp(mu_log_b_cost + sigma_log_b_cost * draw_n)
#
# Therefore, the cost coefficient is ALWAYS negative for every individual.
#share_neg_cost = 1
#share_pos_cost = 0

################################################################################################################################################################################################################################################
#MODEL COMPARISON: GOODNESS OF FIT
#MODEL 1   -> baseline MNL without ASCs
#MODEL 1b  -> baseline MNL with ASCs
#MODEL 2   ->  MNL with significance interaction 
#MODEL 3B -> Mixed Logit 

# RULES:
#- LR test for nested models
#- BAS test for non-nested models on the same dataset
#- AIC / BIC / LL as additional evidence

{
# 1. NESTED MODEL COMPARISONS -> LR TEST
apollo_lrTest(model1, model1B)
# Interpretation:
# H0: the additional ASC parameters in Model 1b are jointly equal to 0
# If p-value is small -> reject H0-> ASCs significantly improve model fit


apollo_lrTest(model1B, model2)
# Interpretation:
# H0: the additional interaction parameters in Model 2 are jointly equal to 0
# If p-value is small -> reject H0 -> at least one added interaction improves fit

# 2. NON-NESTED MODEL COMPARISON -> BAS TEST
apollo_basTest(model2, model_3B)
# Interpretation:
# The two models are non-nested, so LR test cannot be used.
# If p-value is small -> reject H0 that the lower-fit model is the true one-> prefer the model with the higher adjusted fit
#report LL, AIC and BIC
}

#########################################################################################################################################################################################################################################
#POST-ESTIMATION ANALYSIS              
#Predictions, elasticities, scenario simulation
#Preferred model: 3B, Mixed Logit 
#The post-estimation analysis translates the estimated preference parameters into policy-relevant quantities.
#The dataset was designed to inform California's alternative fuel vehicle productor: which vehicle
#attributes drive consumer adoption of non-gasoline vehicles, and 
#which policy instruments are most effective at closing the gap?

#All elasticities are computed via scenario simulation
# rather than the analytical MNL formula, because purchase price variable is specified as price/log(income), making individual analytical
#elasticities uninterpretable as pure price sensitivity measures, since they confound price
#and income differences across individuals

#Research questions 
#1.What are the predicted market shares by fuel type and body type under the preferred model?
#Do college-educated consumers and larger households show different predicted shares, consistently
#with the significant interactions found in estimation?
#WHY IT MATTERS?:
#Before evaluating policy scenarios, it is essential to establish the baseline market structure implied
#by the model. 
#Predicted market shares by fuel type quantify how far alternative fuels are from gasoline
#under current conditions, providing the reference point against which any policy intervention is
#evaluated. 
#Segmented predicted shares by college education and household size translate the significant
# interactions found in estimation into concrete market quantities. 
#Rather than reporting only that college consumers are more pollution sensitive
#or that larger households prefer functional body types, predicted shares quantify how large these
#differences are in market share terms, providing a more policy relevant picture of preference
#heterogeneity across consumer segments.

#2. How price-sensitive is electric vehicle demand?
#We simulate a 1% decrease in the income-adjusted price variable for EV alternatives, which
#approximates a 1% decrese in purchase price holding household income constant. Own and cross
#price elasticities are computed for all fuel types.
#WHY IT MATTERS:
#The own price elasticity of EV demand, computed by simulating a 1% reduction in the income-adjusted
#price variable directly informs producers about how much market share they can gain by reducing
#the price of electric vehicles keeping the income household fixed. A high elasticity means that even a 
#modest price cut significantly shifts consumer demand toward EVs, making it easier and less costly for producers to
#meet the mandate. 
#A low elasticity instead implies that large price reductions and significant margin losses would be 
#required to generate sufficient voluntary adoption.                                  ####
#Cross elasticities reveal which competing fuels would lose share when EV prices decrease, 
#showing  whether consumers substitute away from gasoline or  other alternative fuels, a key consideration for
#producers managing a mixed portfolio of vehicles under the mandate.
#Note: in the Mixed Logit, cross elasticities may differ across competing fuels, unlike in the MNL
#where IIA would impose equal relative cross effects.
#This richer substitution pattern is a direct consequence of the unobserved heterogeneity in
#cost sensitivity captured by the preferred model.

#3. Do predicted EV market shares respond differently to a reduction in the income-adjusted price variable
#across education segments?   
#WHY IT MATTERS
#The estimation results show that college-educated consumers are more sensitive to tailpipe emissions
#(pollution x college interaction), which translates into a higher baseline predicted EV share for this
#segment.
#Examine whether the aggregate market elasticity differs across segments as a consequence
#of their different baseline EV shares: even with the same marginal sensitivity to the income-adjusted
#price variable, segments starting from different baseline probabilities may show different aggregate
#responses to a 1% reduction in that variable.
#This has implications for producers trying to understand which consumer segments are more
#responsive to  price reductions.



#4. What is the market share impact of expanding EV charging infrastructure by 10 percentage points?
#The estimated model finds that station availability has a positive and significant effect on vehicle
#choice (b_station = +0.390, p < 0.001), meaning that consumers are more likely to choose a vehicle
#when a higher fraction of stations can refuel or recharge it. We simulate a 10 percentage point
#increase in station availability for EV alternatives only, translating this coefficient into a concrete
#market share change.         
#Since the interaction between station availability and electric fuel type was not significant, the
#scenario applies the same marginal utility of infrastructure as estimated for all fuel types.
#The result is therefore best interpreted as a lower bound on the true infrastructure effect
#if EV-specific infrastructure were valued more than general refuelling availability.
# WHY IT MATTERS
#Infrastructure investment is a key alternative for promoting EV adoption.
# Comparing the market share change from this scenario with the price elasticity from Q2
#allows a direct evaluation of which instrument is more effective at increasing EV adoption,         
#informing the allocation of investment.
#WHY IT MATTERS:
#While question 2 examines a producers reducing  prices to stimulate demand,
#this scenario examines a instrument available to government: investing in charging
#infrastructure to make electric vehicles more convenient and practical for consumers.
# Station availability is positive and significant     ####
            

#5. Are consumers more sensitive to the income-adjusted price variable or to ongoing running costs?
#We simulate a 1% decrease in running cost for EV alternatives and compare the resulting elasticity
#with the income-adjusted price elasticity from question 2.
#WHY IT MATTERS
#The income-adjusted price variable and running cost represent two distinct levers to increase EV.
#: the first acts on the vehicle cost, the second on the ongoing cost per
#mile — which for EVs reflects recharging costs.
#The preferred model finds significant unobserved heterogeneity in running cost sensitivity, meaning
##that consumers differ substantially in how much they care about ongoing costs. 
#Comparing the two elasticities directly answers which lever matters
#more for EV adoption: if the running cost elasticity exceeds the price elasticity in absolute
#value, ongoing cost reductions drive voluntary EV
#demand more effectively than list price reductions,
#and vice versa.                  

####
#IMP: run AFTER the estimation script
#(model_3B, apollo_probabilities, apollo_inputs must already be in your R environment)                    


#1: BASE PREDICTIONS AND MARKET SHARES
#1. What are the predicted market shares by fuel type and body type?
{
database <- read_csv("Car.csv")
database$ID         <- 1:nrow(database)
database$choice_num <- as.numeric(sub("choice", "", database$choice))
apollo_inputs       <- apollo_validateInputs()

# Base predictions
predictions_base <- apollo_prediction(
  model_3B,
  apollo_probabilities,
  apollo_inputs
)
predictions_base <- as.data.frame(predictions_base)

# Average predicted probabilities per alternative
# Same as mean(prediction$car) in Lab 2
mean(predictions_base$alt1)
mean(predictions_base$alt2)
mean(predictions_base$alt3)
mean(predictions_base$alt4)
mean(predictions_base$alt5)
mean(predictions_base$alt6)

# Predicted number of choices per alternative
sum(predictions_base$alt1)
sum(predictions_base$alt2)
sum(predictions_base$alt3)
sum(predictions_base$alt4)
sum(predictions_base$alt5)
sum(predictions_base$alt6)

# In my dataset each individual has exactly one choice (no repeated tasks)
# so the Observation column in predictions_base corresponds 1-to-1 with ID
# Merge predictions with original database by ID
database_pred <- merge(
  x    = database,
  y    = predictions_base,
  by.x = "ID",
  by.y = "ID"
)

# Check: should have same number of rows as original database
nrow(database_pred) == nrow(database)

# Helper functions (same logic as brand_share in Lab 5)

fuel_share <- function(data_pred, fuel_name) {
  probs <- c(
    data_pred$alt1[data_pred$fuel1 == fuel_name],
    data_pred$alt2[data_pred$fuel2 == fuel_name],
    data_pred$alt3[data_pred$fuel3 == fuel_name],
    data_pred$alt4[data_pred$fuel4 == fuel_name],
    data_pred$alt5[data_pred$fuel5 == fuel_name],
    data_pred$alt6[data_pred$fuel6 == fuel_name]
  )
  return(sum(probs) / nrow(data_pred))
}

body_share <- function(data_pred, body_name) {
  probs <- c(
    data_pred$alt1[data_pred$type1 == body_name],
    data_pred$alt2[data_pred$type2 == body_name],
    data_pred$alt3[data_pred$type3 == body_name],
    data_pred$alt4[data_pred$type4 == body_name],
    data_pred$alt5[data_pred$type5 == body_name],
    data_pred$alt6[data_pred$type6 == body_name]
  )
  return(sum(probs) / nrow(data_pred))
}

# Market shares by fuel type
share_gasoline <- fuel_share(database_pred, "gasoline")
share_methanol <- fuel_share(database_pred, "methanol")
share_cng      <- fuel_share(database_pred, "cng")
share_electric <- fuel_share(database_pred, "electric")

#PREDICTED MARKET SHARES BY FUEL TYPE 
share_gasoline
share_methanol
share_cng
share_electric

# Check that brand shares sum to 1
share_gasoline + share_methanol + share_cng + share_electric 

# Market shares by body type
share_regcar   <- body_share(database_pred, "regcar")
share_sportuv  <- body_share(database_pred, "sportuv")
share_sportcar <- body_share(database_pred, "sportcar")
share_stwagon  <- body_share(database_pred, "stwagon")
share_truck    <- body_share(database_pred, "truck")
share_van      <- body_share(database_pred, "van")

#PREDICTED MARKET SHARES BY BODY TYPE 
share_regcar
share_sportuv
share_sportcar
share_stwagon
share_truck
share_van

# Check that brand shares sum to 1
share_regcar + share_sportuv + share_sportcar + share_stwagon+ share_truck+ share_van

# 1. EV share by college (motivated by pollution x college interaction)
# Then compute fuel-level shares by segment using fuel_share (cosa nuova)
share_ev_college   <- fuel_share(database_pred[database_pred$college == 1, ],
                                 "electric")
share_ev_nocollege <- fuel_share(database_pred[database_pred$college == 0, ],
                                 "electric")

#PREDICTED EV SHARE BY EDUCATION LEVEl
share_ev_college
share_ev_nocollege

# 2. Station wagon share by household size
# (motivated by stwagon x hsg2 interaction)
share_stwagon_hsg2   <- body_share(database_pred[database_pred$hsg2 == 1, ], "stwagon")
share_stwagon_nohsg2 <- body_share(database_pred[database_pred$hsg2 == 0, ], "stwagon")

share_stwagon_hsg2
share_stwagon_nohsg2

# 3. Van share by household size
# (motivated by van x hsg2 interaction)
share_van_hsg2   <- body_share(database_pred[database_pred$hsg2 == 1, ], "van")
share_van_nohsg2 <- body_share(database_pred[database_pred$hsg2 == 0, ], "van")

share_van_hsg2
share_van_nohsg2

}

###############################################################
#2:SCENARIO : 1% DECREASE IN EV PURCHASE PRICE                                                                                                                    ####
#Steps:
#1. Restore original database and compute base predictions
#2. Increase price by 1% for EV alternatives only
#3. Recompute predictions under the new scenario
#4. Restore original database
#5. Compute elasticities as log ratio / log(1.01)          
# How price-sensitive is EV demand?  --> elesticities
#(own and cross price elasticities via scenario)  
{

# STEP 1: Base predictions
# We need to recompute base predictions from the clean database
# before applying any scenario change.
database <- read_csv("Car.csv")
database$ID         <- 1:nrow(database)
database$choice_num <- as.numeric(sub("choice", "", database$choice))
apollo_inputs       <- apollo_validateInputs()

predictions_base <- apollo_prediction(
  model_3B,
  apollo_probabilities,
  apollo_inputs
)
predictions_base <- as.data.frame(predictions_base)

# Merge predictions with original database
# Here each individual has only one choice so we merge by ID only
database_pred <- merge(
  x    = database,
  y    = predictions_base,
  by   = "ID"
)

# Base fuel-level market shares
share_ev_base   <- fuel_share(database_pred, "electric")
share_gas_base  <- fuel_share(database_pred, "gasoline")
share_cng_base  <- fuel_share(database_pred, "cng")
share_meth_base <- fuel_share(database_pred, "methanol")

share_ev_base
share_gas_base
share_cng_base
share_meth_base


# STEP 2: SCENARIO: 1% DECREASE IN EV PRICE

# Pair 2: CNG or electric
database$price3[database$fuel3 == "electric"] <- 0.99 * database$price3[database$fuel3 == "electric"]
database$price4[database$fuel4 == "electric"] <- 0.99 * database$price4[database$fuel4 == "electric"]

# Pair 3: gasoline or electric
database$price5[database$fuel5 == "electric"] <- 0.99 * database$price5[database$fuel5 == "electric"]
database$price6[database$fuel6 == "electric"] <- 0.99 * database$price6[database$fuel6 == "electric"]


# STEP 3: Recompute predictions under the scenario
apollo_inputs_new <- apollo_validateInputs()

predictions_new <- apollo_prediction(
  model_3B,
  apollo_probabilities,
  apollo_inputs_new
)
predictions_new <- as.data.frame(predictions_new)

# Merge new predictions with the modified database
database_pred_new <- merge(
  x    = database,
  y    = predictions_new,
  by   = "ID"
)

# New fuel-level market shares after the price increase
share_ev_new   <- fuel_share(database_pred_new, "electric")
share_gas_new  <- fuel_share(database_pred_new, "gasoline")
share_cng_new  <- fuel_share(database_pred_new, "cng")
share_meth_new <- fuel_share(database_pred_new, "methanol")

share_ev_new
share_gas_new
share_cng_new
share_meth_new

# STEP 4: Restore original database
database <- read_csv("Car.csv")
database$ID         <- 1:nrow(database)
database$choice_num <- as.numeric(sub("choice", "", database$choice))
apollo_inputs       <- apollo_validateInputs()

# Dopo il restore in STEP 4 di Part A, ricreare database_pred
# per Part B con le predizioni baseline pulite
database_pred <- merge(
  x  = database,
  y  = predictions_base,
  by = "ID"
)

# STEP 5: Compute market elasticities
# Same formula as Lab 2 and Lab 5:elasticity = log(share_new / share_base) / log(1.01)
# Own elasticity: how EV demand responds to EV price DECRESE-> own elasticity expected negative (lower price increse demand)
#
# Cross elasticities: how competing fuels respond to EV price decrese-> should be positive 
#(competitors loose share when EV becomes cheaper)

# Own price elasticity for EV
elas_ev_own <- log(share_ev_new / share_ev_base) / log(0.99)

# Cross price elasticities for competing fuels
elas_gas_cross  <- log(share_gas_new  / share_gas_base)  / log(0.99)
elas_cng_cross  <- log(share_cng_new  / share_cng_base)  / log(0.99)
elas_meth_cross <- log(share_meth_new / share_meth_base) / log(0.99)

elas_ev_own
elas_gas_cross
elas_cng_cross
elas_meth_cross


# Note: in MNL cross elasticities would be equal across alternatives (IIA property). 
#In the Mixed Logit they can differ because of individual heterogeneity in cost sensitivity.
}


###############################################################
# 2B. Segmented price elasticity: college vs non-college
# SEGMENTED MARKET ELASTICITIES: COLLEGE VS NON-COLLEGE
# Following the segmentation logic used in Lab 2 and the scenario-based market elasticity approach of Lab 5.
# We split the sample by college education 
# The aim is to assess whether the predicted EV demand response to an adjusted price income decrease differs across education groups.
{

# STEP 1: Base predictions on clean database
# Same as before - always start from the original data

database <- read_csv("Car.csv")
database$ID         <- 1:nrow(database)
database$choice_num <- as.numeric(sub("choice", "", database$choice))
set.seed(123)
apollo_inputs       <- apollo_validateInputs()

predictions_base <- apollo_prediction(
  model_3B,
  apollo_probabilities,
  apollo_inputs
)
predictions_base <- as.data.frame(predictions_base)

# Merge predictions with original database
database_pred <- merge(
  x  = database,
  y  = predictions_base,
  by = "ID"
)

# Base EV shares by segment

share_ev_base_college   <- fuel_share(database_pred[database_pred$college == 1, ],
                                      "electric")
share_ev_base_nocollege <- fuel_share(database_pred[database_pred$college == 0, ],
                                      "electric")

share_ev_base_college
share_ev_base_nocollege


# STEP 2: Apply scenario - 1% price decrease for EV alternatives

# Pair 2: CNG or electric
database$price3[database$fuel3 == "electric"] <- 0.99 * database$price3[database$fuel3 == "electric"]
database$price4[database$fuel4 == "electric"] <- 0.99 * database$price4[database$fuel4 == "electric"]
database$price5[database$fuel5 == "electric"] <- 0.99 * database$price5[database$fuel5 == "electric"]
database$price6[database$fuel6 == "electric"] <- 0.99 * database$price6[database$fuel6 == "electric"]


# STEP 3: New predictions under the scenario
set.seed(123)
apollo_inputs_new <- apollo_validateInputs()

predictions_new <- apollo_prediction(
  model_3B,
  apollo_probabilities,
  apollo_inputs_new
)
predictions_new <- as.data.frame(predictions_new)

# Merge new predictions with the modified database
database_pred_new <- merge(
  x  = database,
  y  = predictions_new,
  by = "ID"
)

# New EV shares by segment after price decrease
share_ev_new_college   <- fuel_share(database_pred_new[database_pred_new$college == 1, ],
                                     "electric")
share_ev_new_nocollege <- fuel_share(database_pred_new[database_pred_new$college == 0, ],
                                     "electric")

share_ev_new_college
share_ev_new_nocollege

# STEP 4: Restore original database
database <- read_csv("Car.csv")
database$ID         <- 1:nrow(database)
database$choice_num <- as.numeric(sub("choice", "", database$choice))
set.seed(123)
apollo_inputs       <- apollo_validateInputs()

# STEP 5: Segmented market elasticities

elas_ev_college   <- log(share_ev_new_college / share_ev_base_college) / log(0.99)
elas_ev_nocollege <- log(share_ev_new_nocollege / share_ev_base_nocollege) / log(0.99)

elas_ev_college
elas_ev_nocollege

}



# ############################################################### #
#SECTION 3: STATION AVAILABILITY SCENARIO (+10percentage points for EVs)
#WHY THIS SCENARIO MAKES SENSE?
#b_station = +0.390 
#A positive and significant station coefficient implies that vehicles with broader compatible station 
#coverage are more attractive. For EVs, this can be interpreted as a proxy for charging 
#infrastructure availability.
#This is therefore a natural policy scenario:
#how much would predicted EV demand increase if EV-compatible charging coverage increased by 10 percentage points?
# This scenario can then be compared qualitatively with the EV price scenario
# to assess which policy lever generates a larger increase in predicted EV adoption
# under the specific shocks considered.
{
# STEP 1: Base predictions on clean database

database <- read_csv("Car.csv")
database$ID         <- 1:nrow(database)
database$choice_num <- as.numeric(sub("choice", "", database$choice))
set.seed(123)
apollo_inputs       <- apollo_validateInputs()

predictions_base <- apollo_prediction(
  model_3B,
  apollo_probabilities,
  apollo_inputs
)
predictions_base <- as.data.frame(predictions_base)

# Merge with original database
database_pred <- merge(
  x  = database,
  y  = predictions_base,
  by = "ID"
)

# Base shares
share_ev_base_st  <- fuel_share(database_pred, "electric")
share_gas_base_st <- fuel_share(database_pred, "gasoline")

share_ev_base_st
share_gas_base_st

# Check current station availability for EV alternatives
# This is important to motivate the +10pp scenario:
# if current availability is already close to 1, the scenario
# has little room to operate. If it is low, the scenario is
# more realistic and policy-relevant.

# Electric appears only in pair 2 (alt3, alt4) and pair 3 (alt5, alt6)
# so we check only those four alternatives.

ev_station_base <- c(
  database$station3[database$fuel3 == "electric"],
  database$station4[database$fuel4 == "electric"],
  database$station5[database$fuel5 == "electric"],
  database$station6[database$fuel6 == "electric"]
)

mean(ev_station_base)
min(ev_station_base)
max(ev_station_base)

# Interpretation:
# If the mean is around 0.25-0.40, the +10pp scenario is realistic
# and represents a meaningful infrastructure investment.
# pmin(1, ...) ensures the station variable stays within [0,1].


# STEP 2: Apply scenario
# Increase station availability by 10 percentage points for EVs
# Electric appears only in alt3, alt4 (pair 2) and alt5, alt6 (pair 3)
# Same explicit approach as Lab 5 - no loops
# pmin(1, ...) caps the value at 1 to respect the variable definition


# Pair 2: CNG or electric
database$station3[database$fuel3 == "electric"] <-
  pmin(1, database$station3[database$fuel3 == "electric"] + 0.10)

database$station4[database$fuel4 == "electric"] <-
  pmin(1, database$station4[database$fuel4 == "electric"] + 0.10)

# Pair 3: gasoline or electric
database$station5[database$fuel5 == "electric"] <-
  pmin(1, database$station5[database$fuel5 == "electric"] + 0.10)

database$station6[database$fuel6 == "electric"] <-
  pmin(1, database$station6[database$fuel6 == "electric"] + 0.10)


# STEP 3: New predictions under the scenario

set.seed(123)
apollo_inputs_new <- apollo_validateInputs()

predictions_new <- apollo_prediction(
  model_3B,
  apollo_probabilities,
  apollo_inputs_new
)
predictions_new <- as.data.frame(predictions_new)

# Merge new predictions with modified database
database_pred_new <- merge(
  x  = database,
  y  = predictions_new,
  by = "ID"
)

# New shares after scenario
share_ev_new_st  <- fuel_share(database_pred_new, "electric")
share_gas_new_st <- fuel_share(database_pred_new, "gasoline")

share_ev_new_st
share_gas_new_st


# STEP 4: Restore original database


database <- read_csv("Car.csv")
database$ID         <- 1:nrow(database)
database$choice_num <- as.numeric(sub("choice", "", database$choice))
set.seed(123)
apollo_inputs       <- apollo_validateInputs()


# STEP 5: Results
# We report absolute and relative changes in EV market share,
# and the displacement effect on gasoline vehicles.
# This allows a direct comparison with the price scenario:
# which policy instrument moves EV share more?

# Absolute change in market share (percentage points)
change_ev_pp  <- (share_ev_new_st - share_ev_base_st) * 100

# Relative change (percentage)
change_ev_pct <- 100 * (share_ev_new_st - share_ev_base_st) / share_ev_base_st

# Displacement of gasoline share
change_gas_pp <- (share_gas_new_st - share_gas_base_st) * 100

change_ev_pp
change_ev_pct
change_gas_pp

}

#SECTION 4: RUNNING COST ELASTICITY

#WHY THIS SECTION MAKES SENSE:
#The preferred model has two cost-related attributes:
#- price (purchase price / log income): fixed coefficient
#- cost (running cost per mile): random coefficient

#computes the market elasticity of EV demand
#with respect to running cost via scenario simulation.     
#Comparing this with the price elasticity from Section 2   
#answers: do consumers respond more to purchase price      
#or to ongoing running costs?                             


{

#### RUNNING COST ELASTICITY VIA SCENARIO

# STEP 1: Base predictions

database <- read_csv("Car.csv")
database$ID         <- 1:nrow(database)
database$choice_num <- as.numeric(sub("choice", "", database$choice))

set.seed(123)
apollo_inputs       <- apollo_validateInputs()

predictions_base <- apollo_prediction(
  model_3B,
  apollo_probabilities,
  apollo_inputs
)
predictions_base <- as.data.frame(predictions_base)

database_pred <- merge(
  x  = database,
  y  = predictions_base,
  by = "ID"
)

share_ev_base_cost <- fuel_share(database_pred, "electric")
share_ev_base_cost

# STEP 2: Apply scenario
# 1% increase in running cost for EV alternatives only.
# WHY running cost?
# Running cost (cost per mile) is a different policy lever than
# adjusted income price, it reflects home charging costs. 
#Comparing the two elasticities tells
# us whether consumers are more sensitive to upfront costs
# or to ongoing usage costs, key question for policy design.

# Pair 2: CNG or electric
database$cost3[database$fuel3 == "electric"] <-
  1.01 * database$cost3[database$fuel3 == "electric"]

database$cost4[database$fuel4 == "electric"] <-
  1.01 * database$cost4[database$fuel4 == "electric"]

# Pair 3: gasoline or electric
database$cost5[database$fuel5 == "electric"] <-
  1.01 * database$cost5[database$fuel5 == "electric"]

database$cost6[database$fuel6 == "electric"] <-
  1.01 * database$cost6[database$fuel6 == "electric"]

# STEP 3: New predictions under the scenario

set.seed(123)
apollo_inputs_new <- apollo_validateInputs()

predictions_new <- apollo_prediction(
  model_3B,
  apollo_probabilities,
  apollo_inputs_new
)
predictions_new <- as.data.frame(predictions_new)

database_pred_new <- merge(
  x  = database,
  y  = predictions_new,
  by = "ID"
)

share_ev_new_cost <- fuel_share(database_pred_new, "electric")
share_ev_new_cost

# STEP 4: Restore original database

database <- read_csv("Car.csv")
database$ID         <- 1:nrow(database)
database$choice_num <- as.numeric(sub("choice", "", database$choice))
set.seed(123)
apollo_inputs       <- apollo_validateInputs()

# STEP 5: Running cost elasticity

elas_ev_cost <- log(share_ev_new_cost / share_ev_base_cost) / log(1.01)
elas_ev_cost

# Interpretation:
# elas_ev_cost should be negative: a higher EV running cost reduces predicted EV demand.
# Compare this with elas_ev_own from Section 2.
# If |elas_ev_cost| > |elas_ev_own|, EV demand is more responsive
# to running-cost changes than to income-adjusted purchase-price changes.
# If |elas_ev_cost| < |elas_ev_own|, EV demand is more responsive
# to income-adjusted purchase-price changes.
# This comparison helps assess whether ongoing usage costs
# or upfront purchase costs are the stronger demand lever
# within the preferred model.


# ############################################################### #
#### SECTION 5: SUMMARY FOR PAPER                              ####
# ############################################################### #

cat("\n")
cat("=================================================================\n")
cat("SUMMARY: KEY QUANTITATIVE RESULTS FOR PAPER\n")
cat("=================================================================\n")

cat("\n1. PREDICTED MARKET SHARES (baseline)\n")
cat("   Gasoline:", round(share_gasoline, 4),
    "| Methanol:", round(share_methanol, 4),
    "| CNG:", round(share_cng, 4),
    "| Electric:", round(share_electric, 4), "\n")

cat("\n2. EV PRICE ELASTICITY\n")
cat("   All consumers:   ", round(elas_ev_own,       4), "\n")
cat("   College:         ", round(elas_ev_college,   4), "\n")
cat("   Non-college:     ", round(elas_ev_nocollege, 4), "\n")

cat("\n3. STATION AVAILABILITY SCENARIO (+10pp)\n")
cat("   EV share increase:",
    round((share_ev_new_st - share_ev_base_st) * 100, 2), "pp (",
    round(100*(share_ev_new_st-share_ev_base_st)/share_ev_base_st, 1), "%)\n")

cat("\n4. RUNNING COST ELASTICITY (EV)\n")
cat("   Own cost elasticity:", round(elas_ev_cost, 4), "\n")

cat("\n5. POLICY COMPARISON: price vs cost sensitivity\n")
cat("   |Price elasticity|:", round(abs(elas_ev_own),  4), "\n")
cat("   |Cost elasticity|: ", round(abs(elas_ev_cost), 4), "\n")
if (abs(elas_ev_cost) > abs(elas_ev_own)) {
  cat("   -> Running costs matter MORE than purchase price.\n")
  cat("      Fuel/electricity pricing is more effective than subsidies.\n")
} else {
  cat("   -> Purchase price matters MORE than running costs.\n")
  cat("      Purchase subsidies are more effective than fuel tax policy.\n")
}
cat("=================================================================\n")
}
