

VB = function(par, ages, lens, lsd=0.2, plot=F, opt = T){
  tpars = exp(par)
  predlens = tpars[3] *( 1 - exp(-tpars[2]*(ages+tpars[1])))
  nll = abs(lens-predlens) # -dnorm(lens, predlens, lsd, log=T)
  if(plot){ plot(lens, main=paste(round(nll,3), collapse = ", ")); lines(predlens,col="red")}
  if(opt) return(sum(nll))
  if(!opt) return(predlens)
}

opt = optim(par = log(c(0.1,0.9,20)), VB, ages = est_age$Age, lens = est_age$Length, method = "L-BFGS-B",lower = log(c(1E-5,0.4, 18)), upper = log(c(1,1.3,22)))
VB(opt$par, ages = est_age$Age, lens = est_age$Length, plot=T)

1/(1+exp(-(Classes - 11.88)/1.93))





B = est_yr$B_west + est_yr$B_south
R = est_yr$R_west + est_yr$R_south # Billions of fish

par = c(0,log(max(R,na.rm=T)/2))

BH<-function(par, B, R, mode='opt', plot = F, tplot = F, curyr = 2024, pmu = 0.6, pcv = 0.5){
  
  h = 0.2 + 0.8 * (1/(1+exp(par[1])))
  R0 = exp(par[2])
  BpR = max(B,na.rm=T) / R0
  Rpred<-((0.8*R0*h*B)/(0.2*BpR*R0*(1-h)+(h-0.2)*B))
  if(tplot) {yrs = curyr + 1 - length(B) : 1; plot(yrs, R,pch=19); points(yrs, Rpred,col='red')}
  if(plot) {plot(B,R,pch=19); points(B, Rpred,col='red'); legend('topleft',legend=c(paste("h =",round(h,3)),paste("R0 =",round(R0,3))),text.col="red",bty="n")}
  
  if(mode=='opt')return(-sum(dnorm(log(Rpred)-log(R),0,0.5,log=T), na.rm=T)-dnorm(pmu,0,pcv,log=T)) # add a vague prior on h = 0.8
  if(mode != "opt") return(R - Rpred)
  
  
}

opt = optim(par = c(0,log(max(R,na.rm=T)/2)), BH, 
            B =  B, R = R, method = "L-BFGS-B",
            lower = c(-2,log(10)), 
            upper = c(2,log(50)))

BH(opt$par, B, R, plot=T)


B = est_yr$B_west + est_yr$B_south # Biomass Mt (could be unitless, R0 and steepness h are the targets)
R = est_yr$R_west + est_yr$R_south # Billions of fish

ctrl = SimControl(DynamicUnfished = FALSE,
                     RefLandings = FALSE,
                     RefRemovals = FALSE,
                     ConditionObs = FALSE,
                     EstimateBeta = FALSE,
                     GenerateData = FALSE,
                     MSYRefs = FALSE,
                     RefPoints = FALSE,
                     MGT = FALSE,
                     BLow = FALSE)


Apply_pars = function(om, pars){
  R0mult = exp(pars[1])
  Emult = exp(-pars[1]) * exp(pars[2])                      # fried egg please!
  om@Stock@SRR@R0 = om@Stock@SRR@R0 * R0mult
  om@Fleet@Effort@Effort =  om@Fleet@Effort@Effort * Emult
  om
}


doplot = function(om, B, R, Best, Rest, obj){
  par(mfrow =c(1,2),mai=c(0.5,0.5,0.2,0.05))
  ind = !is.na(R); yrs = Years(om, 'h')[ind]
  matplot(yrs, cbind(R[ind],Rest[ind]),col=c("black","red"),lty=1,lwd=1.5,type = 'l');grid()
  legend('topleft',legend=c("Assessment","OpenMSE"),text.col = c("black","red"), bty="n")
  matplot(yrs,cbind(B[ind],Best[ind]),col=c("black","red"),lty=1,lwd=1.5,type = 'l');grid()
  legend('topleft',legend=c(paste("R0 multiplier =",round(exp(pars[1]),3)),
                            paste("Effort multiplier =",round(exp(pars[2]),3)),
                            paste("Obj. F. =", round(obj,5))), bty="n")
}  

Int_Scale = function(pars, om, B, R, mode = 'opt', ctrl, plot=F){
  om = Apply_pars(om, pars)
  hist = Simulate(om, control = ctrl)
  Best = hist@Biomass[1,1,] * 1E-9
  Rest = hist@Number[[1]][1,1,,1] * 1E-9
  if(mode == 'opt')  obj = sum(abs(log(Best)-log(B)),na.rm=T) + sum(abs(log(Rest)-log(R)),na.rm=T)
  if(mode == 'optB')  obj = sum(abs(log(Best)-log(B)),na.rm=T)
  if(plot) doplot(om, B, R, Best, Rest, obj)
  if(grepl("opt", mode)){
    return(obj)
  }else{
    pars
  }
} 

opt = optim(c(0,0), Int_Scale, 
      om = om, B = B, R = R, ctrl = ctrl, plot=T,
      method = "L-BFGS-B", control = list(factr = 1E12),
      lower = c(-2, -2), upper = c(2, 2))

om2 = Apply_pars(om,opt$par)

for(i in 1:2){
  hist = Simulate(om2, control=control)
  Best = hist@Biomass[1,1,] * 1E-9
  Rest = hist@Number[[1]][1,1,,1] * 1E-9
  doplot(om2, B, R, Best, Rest, obj = 0)
  ratio = matrix(R/Rest,byrow=T,nrow=om@nSim,ncol=om@nYear)
  ratio[is.na(ratio)] = 1
  R0_adj = exp(log(ratio)*0.5)
  om2@Stock@SRR@RecDevHist[] = om2@Stock@SRR@RecDevHist[] * R0_adj
}

opt = optim(c(0,0), Int_Scale, 
            om = om2, B = B, R = R, ctrl = ctrl, mode = "optB", plot=T,
            method = "L-BFGS-B", control = list(factr = 1E12),
            lower = c(-2, -2), upper = c(2, 2))


om3 = Apply_pars(om2,opt$par)


