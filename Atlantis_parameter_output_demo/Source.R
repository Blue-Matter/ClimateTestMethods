
# very basic function for inventing correlated parameters to demonstrate filling the 6D array
invent_cor_pars = function(seed = 1, n_param=3, n_projection_y, 
                           param_names=c("M","K","R"), param_dir = c(1,-1,-1), 
                           param_start = c(0.2, 0.3, 1), param_dev = c(0.2, 0.1, 0.5),
                           level = c(1,1,1),
                           param_cv = rep(0.3,3), param_cor = 0.75,
                           plot=F){
  
  set.seed(seed)
  if(is.na(param_names[1])) param_names=paste0("Parameter_",1:n_param)
  
  # Trend
  trend = sin((1:n_projection_y)/(n_projection_y/1.5))
  trend_array = array(rep(trend, each=n_param),c(n_param,n_projection_y))
  
  # Correlated errors
  cor =  array(param_cor, c(n_param,n_param))
  diag(cor) = 1
  var <- param_cv %*% t(param_cv) 
  sigma = cor * var
  devs = rmvnorm(n_projection_y, mean=rep(1,n_param), sigma = sigma)
  
  # Generate values
  val = t(param_start + trend_array * param_dir * param_dev * level * t(devs))
  colnames(val) = param_names
  
  if(plot){
    par(mfrow=c(2,2),mai=c(0.8,0.8,0.1,0.1))
    matplot(val,type="l",col=c("red","green","blue"),lty=1,lwd=2)
    legend('topright',legend=param_names,text.col=c("red","green","blue"),bty='n',text.font=2)
    plot(val[,1], val[,2],xlab=param_names[1],ylab=param_names[2],pch=19)
    plot(val[,2], val[,3],xlab=param_names[2],ylab=param_names[3],pch=19)
    plot(val[,1], val[,3],xlab=param_names[1],ylab=param_names[3],pch=19)
  }
  
  t(val)
  
}
