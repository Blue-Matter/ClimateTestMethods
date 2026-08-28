setwd("C:/GitHub/ClimateTestMethods")
AO = readRDS("ATL_final_FPL.rds")



dimnames(ATL_output) = list(paste0("Model_",1:n_model),
                            paste0("Species_",1:n_species),
                            paste0("Level_",1:n_climate_level),
                            paste0("Sim_",1:n_sim),
                            paste0("Parameter_",1:n_param),
                            year_labs)


M = AO[1,1,,1,1,]
K = AO[1,1,,1,2,]
R = AO[1,1,,1,3,]

par(mfrow=c(2,2),mai=c(0.5,0.8,0.1,0.05))
cols = c("black","green","red","blue")
matplot(t(M),lty=1, lwd=1.5, type="l", ylab = "M", col=cols);grid()
matplot(t(K),lty=1, lwd=1.5, type="l", ylab = "K", col=cols);grid()
matplot(t(R),lty=1, lwd=1.5, type="l", ylab = "R", col=cols);grid()
legend('topright',legend=dimnames(AO)[[3]], text.col=cols, bty="n")



