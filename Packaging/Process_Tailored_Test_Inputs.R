
# ===== Script to convert Atlantis outputs to values that can be used in OpenMSE projections =================

# Tom Carruthers

setwd("C:/GitHub/ClimateTestMethods")

# Atlantis: southern Benguela marine ecosystem
SB = readRDS("ATL_final_FPL.rds") # Model, Species, Level, Sim, Parameter, Yr
# Model, Species, Level, Sim, Parameter, Yr
dd = dim(SB); nmod = dd[1]; nspec = dd[2]; nlev = dd[3]; nsim = dd[4]; npar = dd[5]; nyr = dd[6] 


# ------ Convert M to relative -----------------------------------------------------------------
# M is a relative value - here we assume the mean is the current level. 
Mdim = match("M",dimnames(SB)[[5]]) 
SB[,1,,,Mdim,] = SB[,1,,,Mdim,]/mean(SB[,1,,,Mdim,]) # conv to mean 1


# ------ Convert K to relative -----------------------------------------------------------------

est_len = as.data.frame(readxl::read_xlsx('docs/OMs/Sardine/Data/SA_Sardine_2025_estimates.xlsx', 'ByAge'))

nage = nrow(est_len)
lenvec = est_len[2:nage,2]

getmuwt = function(lenvec,Z=1.2){
  age = 1:length(lenvec)
  S = exp(-Z*age)
  est_wt = 0.0000071118 * lenvec ^ 3.181 * 1000
  sum(est_wt * S)/sum(S)
}

# Make % modification to K to get the g dif in mean weight  (K is expressed in gram difference in mean weight)

getmuwt(lenvec)
agevec = (1:nage)-0.5
Ktrial = 1.08
lenvec_trial = 19.7 * (1-exp(-Ktrial*agevec+0.5))
matplot(cbind(est_len[,2],lenvec_trial),type="l",lty=1)
legend('bottomright',legend=round(getmuwt(lenvec_trial[2:nage]),2))

# get adjusted K (factor that gets to the 5g change)

Kadj = 1.1667
lenvec_adj = 19.7 * (1-exp(-Kadj*agevec+0.5))
matplot(cbind(lenvec_adj,lenvec_trial),type="l",lty=1)
legend('bottomright',legend=round(getmuwt(lenvec_trial[2:nage])-getmuwt(lenvec_adj[2:nage]),2))

Keq = (1-Ktrial/Kadj) * 100 # % change in K to get a 5g difference in mean weight

Kdim = match("K",dimnames(SB)[[5]]) 
SB[,1,,,Kdim,] = SB[,1,,,Kdim,]/5 * Keq  # conv to mean 1


# ----- Convert to recruitment relative to today -----------------------------------------------

Rdim = match("R",dimnames(SB)[[5]]) 
SB[,1,,,Rdim,] = SB[,1,,,Rdim,]/SB[,1,,,Rdim,1] # conv to mean 1


# ----- Now visualize --------------------------------------------------------------------------
  
plot_TT_input(SB)
TT = SB
save(TT, file = "C:/GitHub/ClimateTest/data/TT.rda")


# ==== End of Script =============================================================================













