# Function to calculate the Sharpe ratio
sharpe = function(returns, rf) {
  
  h1 = returns      # Assign daily returns to h1
  h2 = returns^2    # Calculate the square of returns and assign to h2
  h3 = rf           # Assign the risk-free rate to h3
  
  # Calculate the Sharpe ratio
  # The Sharpe ratio is computed as the excess return (mean return - mean risk-free return)
  # divided by the standard deviation of the excess returns.
  sharpe_mat = (mean(h1) - mean(h3)) / sqrt(mean(h2) - mean(h1)^2)
  
  return(sharpe_mat)  # Return the calculated Sharpe ratio
}


