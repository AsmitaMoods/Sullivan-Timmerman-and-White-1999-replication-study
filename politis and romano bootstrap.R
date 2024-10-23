#just using indexing 
PRB = function(returns, q){ 

  n = nrow(returns) #set index
  boots_ft = matrix(0, nrow=n, ncol=1) #store indices
  bigR= 1  #start
  bigT= n #end 
  t = bigR #set t to R to ensure make n bootstraps 
  theta_t = sample(bigR:bigT, size = 1) #index position to start at
  boots_ft[1] = theta_t  
  
  while(t < bigT){
    
    t = t+1 #increment 
    U = runif(1, 0, 1)  #randomly generate from a standard uniform
    
    if(U <= q){
    
    theta_t = sample(bigR:bigT, size = 1) #sample
    boots_ft[t] = theta_t 

    }else{
      
      if(theta_t < n){
        
        theta_t = theta_t + 1 #expand the block 
    
      }else{
        
        theta_t = bigR  #reset the block 
        
      }  
      
      boots_ft[t] = theta_t #store current value
      
    }

  } 
  
  
  boot_returns = returns[boots_ft, ,drop =F]
  return(boot_returns) #retun bootstrap returns. 
}



