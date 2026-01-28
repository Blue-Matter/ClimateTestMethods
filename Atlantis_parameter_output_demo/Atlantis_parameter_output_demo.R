

# === A script to create a blank Atlantic model output array and then fill it with some made-up data ============================

# Tom Carruthers
# 28 January 2026

# --------- Prerequisites 

library(mvtnorm)                                                      # For generating correlated multivariate errors
library(plotly)                                                       # For plotting multivariate data

setwd("C:/GitHub/ClimateTestMethods/Atlantis_parameter_output_demo")  # Working directory
source('Source.R')                                                    # Loads some code for inventing correlated projected parameters

# ---------- The array to be filled --------------------------------------------

n_model = 2                                           # Here we are assuming there are two different Atlantis models              
n_species = 2                                         # Predictions of population parameters are provided for two species
n_climate_level = 10                                  # The number of increments for climate severity, e.g. model predictions for a 0.1, 0.2, 0.3, 0.4.... 1 degree ocean warming scenarios
n_sim = 50                                            # Stochasticity in the Atlantic model predictions is accounted for across multiple simulations
n_param = 3                                           # The number of parameters predicted such as M, K, recruitment strength
n_projection_y = 50                                   # The number of projection years for which parameter predictions are made - has to be long enough to create parameter changes that can potentially affect MP performance
year_labs = 2025+1:n_projection_y                     # Projection year labels  
ATL_output = array(NA, c(n_model, n_species, n_climate_level, n_sim,  n_param, n_projection_y))

dimnames(ATL_output) = list(paste0("Model_",1:n_model),
                            paste0("Species_",1:n_species),
                            paste0("Level_",1:n_climate_level),
                            paste0("Sim_",1:n_sim),
                            paste0("Parameter_",1:n_param),
                            year_labs)

saveRDS(ATL_output, file="ATL_output_blank.rds")

# ----------- Populate the array -----------------------------------------------

for(mm in 1:n_model){
  for(ss in 1:n_species){
    for(ll in 1:n_climate_level){
      for(sim in 1:nsim){
        ATL_output[mm, ss, ll, sim,,] = 
          
          invent_cor_pars(seed = sim+sim*ll, n_param, n_projection_y,     # Dimensions, random seed
                          param_names=c("M","K","R"),              # Parameter labels (natural mortality rate, somatic growth, mean recruitment strength)
                          param_dir = c(1,-1,-1),                  # Direction of climate impact (increase, decrease, decrease)
                          param_start = c(0.2, 0.3, 1),            # Value at start of projection
                          param_dev = c(0.2, 0.1, 0.5),            # Maximum degree of change in parameter
                          level = rep(ll/n_climate_level, n_param),# increase in climate effect (corresponds with increment for climate severity)                       
                          param_cv = rep(0.3,3),                   # assumed 30% errors in variables
                          param_cor = 0.75,                        # have them correlated (param_dir takes care of negative correlation)
                          plot = F)                                # should this set of invented data be plotted?
        
}}}}


# ------------ Take a look at some of the invented data ------------------------


mm = 1; ss = 1; sim = 1 # What to plot?

col = rev(rainbow(n_climate_level,start=0,end=0.35))
M = as.vector(ATL_output[mm,ss,,sim,1,])
K = as.vector(ATL_output[mm,ss,,sim,2,])

data = data.frame(M = M, K = K, Year = rep(year_labs, each = n_climate_level), color = col)

plot_ly(data, x = ~K, y = ~Year, z = ~M, type = 'scatter3d', mode = 'markers',
        opacity = 0.6, marker = list(size = 4.5, color = ~color, reverscale = FALSE))



# === END ===================================================================================================================


