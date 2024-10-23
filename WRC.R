# Function to perform the White Reality Check for performance evaluation
WhiteRealityCheck = function(highest_stat, q, b, performance_matrix, t, rf) {
  
  n = nrow(performance_matrix)  # Get the number of rows in the performance matrix (number of observations)
  
  # Check if t (a parameter indicating whether to scale by 252) is 0
  if(t == 0) {
    Vl = sqrt(n) * highest_stat  # Calculate the test statistic Vl for t = 0
    Vl_star = matrix(0, nrow = b, ncol = 1)  # Initialize matrix to store bootstrapped values
    
    # Bootstrap loop
    for(i in 1:b) {
      boot_returns = PRB(performance_matrix, q)  # Generate bootstrapped returns
      expected_returns = colMeans(boot_returns)   # Calculate expected returns from bootstrapped returns 
      
      # Find the maximum expected return
      boot_max_return = expected_returns[which.max(expected_returns)] 
      
      # Calculate bootstrapped statistic and store it
      Vl_star[i, ] = sqrt(n) * boot_max_return - Vl 
    }  
    
    # Calculate p-value based on the proportion of bootstrapped statistics greater than Vl
    p_value = round(sum(as.numeric(Vl_star > Vl)) / b, 15)  
    return(list(Vl_star, p_value))  # Return the bootstrapped values and the p-value
    
  } else { 
    # If t is not 0, scale the statistic by the number of trading days (252)
    Vl = highest_stat * sqrt(252)  # Scale Vl for annualized performance
    Vl_star = matrix(0, nrow = b, ncol = 1)  # Initialize matrix to store bootstrapped Sharpe ratios
    
    # Bootstrap loop for Sharpe ratios
    for(i in 1:b) {
      boot_ytSk = PRB(performance_matrix, q)  # Generate bootstrapped returns
      sharpe_ratios = apply(boot_ytSk, 2, function(returns) sharpe(returns, rf))  # Calculate Sharpe ratios for bootstrapped returns
      
      # Find the maximum Sharpe ratio
      boot_max_SR = sharpe_ratios[which.max(sharpe_ratios)]
      
      # Calculate bootstrapped statistic and store it
      Vl_star[i, ] = boot_max_SR * sqrt(252) - Vl
    }
    
    # Calculate p-value based on the proportion of bootstrapped Sharpe ratios greater than Vl
    p_value = round(sum((Vl_star > Vl)) / b, 15)  
    return(list(Vl_star, p_value))  # Return the bootstrapped Sharpe ratios and the p-value
  }   
}


