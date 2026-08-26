

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


