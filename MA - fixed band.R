library(quantmod) 


#parameters
#n = number of moving averages 
#b = fixed band filter

# Function to calculate SMA strategies with fast and slow moving averages and bands
Sma_fsb = function(data, n, b){
  
  prices = data$Close  # Extract closing prices from the input data
  p = nrow(data)       # Total number of rows (days) in the data
  q = choose(length(n), 2) * length(b)  # Number of unique (n fast, n slow) combinations times the number of bands
  counter = 1          # Initialize a counter for tracking strategy columns
  strategies = matrix(0, p, q, byrow = F)  # Matrix to hold strategy signals (0: no position, 1: buy, -1: sell)
  col_names = vector("character", q)  # Vector to hold names for the strategies' columns
  
  # Loop over each fast moving average period
  for(nval in n){
    sma_fast = SMA(prices, n = nval)  # Calculate the fast SMA for the current period
    
    # Loop over each slow moving average period
    for(i in n){
      if(i > nval){  # Ensure that the slow period is greater than the fast period
        sma_slow = SMA(prices, n = i)  # Calculate the slow SMA for the current period
        
        # Loop over each band value
        for(bval in b){
          position = 0  # Initialize position (0: no position, 1: long, -1: short)
          
          # Name the strategy column based on fast, slow, and band values
          col_names[counter] = paste0("SMA_fast_", nval, "_slow_", i, "_band_", bval)
          
          # Iterate through the price data starting from the maximum of fast/slow SMA lengths
          for(w in (max(nval, i)):p){
            if(position == 0){  # No current position
              if(sma_fast[w] > (1 + bval) * sma_slow[w]){  # Buy signal
                position = 1  # Set position to long
                strategies[w, counter] = position
              } else if(sma_fast[w] < (1 - bval) * sma_slow[w]){  # Sell signal
                position = -1  # Set position to short
                strategies[w, counter] = position
              } else {
                strategies[w, counter] = position  # No action
              }
            } else if(position == 1){  # Already in a long position
              if(sma_fast[w] < (1 - bval) * sma_slow[w] && sma_fast[w-1] >= (1 + bval) * sma_slow[w-1]){ 
                position = -1  # Switch to short position
                strategies[w, counter] = position 
              } else {
                strategies[w, counter] = position  # Hold position
              }
            } else if(position == -1) {  # Already in a short position
              if(sma_fast[w] > (1 + bval) * sma_slow[w]){
                position = 1  # Switch to long position
                strategies[w, counter] = position 
              } else {
                strategies[w, counter] = position  # Hold position
              }
            }
          }
          counter = counter + 1  # Increment counter for the next strategy column
        }
      }
    }
  }
  colnames(strategies) <- col_names  # Assign column names to the strategies matrix
  return(strategies)  # Return the strategies matrix
}

# Function to calculate SMA strategies based on a single moving average and bands
Sma_b = function(data, n, b) {
  
  prices = data$Close  # Extract closing prices from the input data
  p = length(prices)   # Total number of rows (days) in the data
  q = length(n) * length(b)  # Number of combinations of n and b
  strategies = matrix(0, p, q, byrow=F)  # Matrix to hold strategy signals (0: no position, 1: buy, -1: sell)
  counter = 1  # Initialize a counter for tracking strategy columns
  
  # Loop over each moving average period
  for(nval in n) {
    for(bval in b) {  # Loop over each band value
      position = 0  # Initialize position (0: no position, 1: long, -1: short)
      sma = SMA(prices, n=nval)  # Calculate the SMA for the current period
      
      # Iterate through the price data starting from the current SMA length
      for(i in (nval):p) {
        if(position == 0) {  # No current position
          if((1 + bval) * sma[i] < prices[i]) {  # Buy signal
            position = 1  # Set position to long
            strategies[i, counter] = position
          } else if((1 - bval) * sma[i] > prices[i]) {  # Sell signal
            position = -1  # Set position to short
            strategies[i, counter] = position
          } else {
            strategies[i, counter] = position  # No action
          }
        } else if(position == 1) {  # Already in a long position
          if(prices[i] < (1 - bval) * sma[i]) {  # Condition to switch from buy to sell
            position = -1  # Switch to short position
            strategies[i, counter] = position 
          } else {
            strategies[i, counter] = position  # Hold position
          }
        } else if(position == -1) {  # Already in a short position
          if(prices[i] > (1 + bval) * sma[i]){  # Condition to switch from sell to buy
            position = 1  # Switch to long position
            strategies[i, counter] = position  
          } else {
            strategies[i, counter] = position  # Hold position
          }
        }
      }
      counter = counter + 1  # Increment counter for the next strategy column
    }
  }
  
  # Assigning column names based on n and b values
  col_names <- character(ncol(strategies))  # Initialize vector for column names
  index <- 1  # Index for naming columns
  
  for (nval in n) {
    for (bval in b) {
      col_names[index] <- paste0("SMA_", nval, "_fb_", bval)  # Create unique column name
      index <- index + 1  # Increment index for next column
    }
  }
  
  colnames(strategies) <- col_names  # Assign column names to the strategies matrix
  return(strategies)  # Return the strategies matrix
}
