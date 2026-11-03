
# ====== Get the tailored teat going ==================================================

library(openMSE)
library(ClimateTest)
#setup()

load(file = "C:/GitHub/ClimateTest/data/SAS.rda") # SAS class om
load(file = "C:/GitHub/ClimateTest/data/TT.rda")  # TT 6D array

OM_list = list(SAS)                # A list of operating models
Hist_list = CT_1_prep(OM_list)     # Same OMs but without climate impacts and inclu

Data = Hist_list[[1]]@Data$`1`$`South African Sardine`

Ir2 = ClimateTest::Ir2
It2 = ClimateTest::It2
#mse = Project(Hist_list[[1]],"IndexRate")

MPs = c("Ir2","It2"); type = "SSB"; horizon = 20;  MP_par_nams = c("tunepar","tunepar"); MP_par_intervals = NA; near_enough = 1E-4; tol = 0.005; parallel = TRUE

sfInit(parallel = T, cpus=8)
MPs_tuned = CT_2_tune(Hist_list,
                      MPs = c("Ir2","It2"), # Names of MPs to be tuned and tested
                      type = "SSB",              # Spawning stock biomass tuning
                      horizon = 20,
                      MP_par_nams = c("tunepar","tunepar"),
                      parallel=T)


# ------ Exploratory test ---------------------------------------------------------------------

tests = c(S = 200,           # Up to 200% increase in catchability over the specified horizon
          M = 25,            # Up to 25% increases in natural mortality over the specified horizon
          R = 50,            # Up to 50% decreases in recruitment strength over the specified horizon
          K = 75,            # Up to 75% decreases in somatic growth over the specified horizon
          C = 75)            # Up to 75% decrease in condition factor (W / L^b) over the specified horizon

nlev = 8                     # Number of levels for each test (to interpolate over)

CT_data = CT_3_test(Hist_list,  # From Step 1
                    MPs_tuned,  # From Step 2
                    nlev, horizon, tests) # Over 8 increments for 30 projected years

saveRDS(CT_data,"C:/temp/CT_data.rds")
saveRDS(MPs_tuned,"C:/temp/MPs_tuned.rds")
saveRDS(tests,"C:/temp/tests.rds")
saveRDS(nlev,"C:/temp/nlev.rds")


CT_data = readRDS("C:/temp/CT_data.rds")
MPs_tuned = readRDS("C:/temp/MPs_tuned.rds")
tests = readRDS("C:/temp/tests.rds")
nlev = readRDS("C:/temp/nlev.rds")
for(i in 1:length(MPs_tuned))assign(names(MPs_tuned)[i],MPs_tuned[i])

RT = 0.85
horizon = 20
CT_proj(CT_data, horizon, "M", RT = RT)              # Explanatory figure
CT_4_summary(CT_data, tests = c("M","R","K","C"),RT = RT, horizon = horizon)    # Summary figure


# ----- Tailored Test ---------------------------------------------------------------------------

library(openMSE)
library(ClimateTest)
#setup()

load(file = "C:/GitHub/ClimateTest/data/SAS.rda") # SAS class om
load(file = "C:/GitHub/ClimateTest/data/TT.rda")  # TT 6D array

OM_list = list(SAS)                # A list of operating models
Hist_list = CT_1_prep(OM_list)     # Same OMs but without climate impacts and inclu


CT_data = readRDS("C:/temp/CT_data.rds")
MPs_tuned = readRDS("C:/temp/MPs_tuned.rds")
tests = readRDS("C:/temp/tests.rds")
nlev = readRDS("C:/temp/nlev.rds")
for(i in 1:length(MPs_tuned))assign(names(MPs_tuned)[i],MPs_tuned[i])

# M, K & R
dm = dimnames(TT)                                               # The names of available Tailored Tests
modno = match("ABACuS", dm[[1]])                                # Model
specno = match("FPL", dm[[2]])                                  # Species
levs = match(c("SSP585_IPSL","SSP126_GFDL"), dm[[3]])           # Levels
tests = TT[modno, specno, levs,,,,drop=F]                       # Levels, sim, parameter (e.g., M,K,R), year
CT_data = CT_3_test(Hist_list, MPs_tuned, nlev, horizon, tests) # Tailored Climate Test
results = CT_metrics(CT_data, horizon=20)                       # Calculate SSB outcomes
results$SSB_relative                                            # Print outputs




# Only K & R
tests = TT[modno, specno, levs,,2:3,,drop=F]                    # Levels, sim, parameter (e.g., K,R), year
CT_data = CT_3_test(Hist_list, MPs_tuned, nlev, horizon, tests) # Tailored Climate Test
results2 = CT_metrics(CT_data, horizon=20)                      # Calculate SSB outcomes
results2$SSB_relative                                           # Print outputs



