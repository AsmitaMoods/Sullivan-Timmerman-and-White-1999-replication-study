# FS-SMA Function: Fast-Slow Simple Moving Average Strategy
Sma_fs = function(data, n) {
  
  prices = data$Close  # Extract the closing prices from the data
  p = nrow(data)       # Total number of rows (daily data points)
  q = choose(length(n), 2)  # Calculate the number of combinations of SMA periods
  counter = 1          # Initialize a counter for the strategy combinations
  strategies = matrix(0, p, q, byrow = FALSE)  # Initialize a matrix to store positions
  
  # Loop over each fast SMA period
  for(nval in n) {
    sma_fast = SMA(prices, n = nval)  # Calculate the fast SMA for the current period
    
    # Loop over each slow SMA period
    for(i in n) {
      if(i > nval) {  # Ensure the slow SMA period is greater than the fast SMA period
        sma_slow = SMA(prices, n = i)  # Calculate the slow SMA for the current period
        position = 0  # Start with no position (0 = neutral)
        
        # Start iterating from the maximum of the two SMA periods + 1
        for(w in i:p) {
          if(position == 0) {  # No current position
            if(sma_fast[w] > sma_slow[w]) {  # Buy signal: fast SMA crosses above slow SMA
              position = 1 
              strategies[w, counter] = position  # Record the position in the strategies matrix
            } else if(sma_fast[w] < sma_slow[w]) {  # Sell signal: fast SMA crosses below slow SMA
              position = -1 
              strategies[w, counter] = position  # Record the position
            } else {
              strategies[w, counter] = position  # Hold position
            }
          } else if(position == 1) {  # Currently long position
            if(sma_fast[w] < sma_slow[w] && sma_fast[w-1] >= sma_slow[w-1]) {  # Signal to exit long
              position = -1  
              strategies[w, counter] = position  # Record position as short
            } else {
              strategies[w, counter] = position  # Hold position
            }
          } else if(position == -1) {  # Currently short position
            if(sma_fast[w] > sma_slow[w] && sma_fast[w-1] <= sma_slow[w-1]) {  # Signal to exit short
              position = 1  
              strategies[w, counter] = position  # Record position as long
            } else {
              strategies[w, counter] = position  # Hold position
            }
          }
        } 
        
        counter = counter + 1  # Move to the next strategy combination
      }
    }
  } 
  
  # Create column names for the strategies matrix
  col_names <- character(ncol(strategies))
  index <- 1
  
  for (nval in n) {
    for (i in n) {
      if (i > nval) {
        col_names[index] <- paste0("SMA_", nval, "_", i)  # Naming strategy columns based on SMA periods
        index <- index + 1
      }
    }
  }
  
  colnames(strategies) <- col_names  # Assign the column names to the strategies matrix
  return(strategies)  # Return the strategies matrix
}

# SMA Function: Simple Moving Average Strategy
Sma = function(data, n) {
  
  prices = data$Close  # Obtain daily closing prices  
  p = nrow(data)       # Total number of rows (daily data)
  q = length(n)        # Number of different SMA windows
  counter = 1          # Initialize counter for each SMA
  strategies = matrix(0, p, q, byrow = FALSE)  # Matrix to store positions
  
  # Loop over each SMA period
  for(nval in n) { 
    sma = SMA(prices, n=nval)  # Compute the simple moving average for the current period
    position = 0  # Start with no position (0 = neutral, 1 = long, -1 = short)
    
    # Start after the first nval prices
    for(i in (nval + 1):p) {  
      if(position == 0) {  # No position
        if(prices[i] > sma[i]) {  # Buy signal: price crosses above SMA
          position = 1
        } else if(prices[i] < sma[i]) {  # Sell signal: price crosses below SMA
          position = -1
        }
        strategies[i, counter] = position  # Save the new position
        
      } else if(position == 1) {  # Currently long
        if(prices[i] <= sma[i]) {  # Exit long, enter short: price crosses below SMA
          position = -1
        }
        strategies[i, counter] = position  # Hold or update position
        
      } else if(position == -1) {  # Currently short
        if(prices[i] >= sma[i]) {  # Exit short, enter long: price crosses above SMA
          position = 1
        }
        strategies[i, counter] = position  # Hold or update position
      }
      
    }
    counter = counter + 1  # Move to the next SMA window length
  } 
  
  # Assign column names to identify the SMA window lengths
  colnames(strategies) <- paste0("SMA_", n)
  
  return(strategies)  # Return the strategies matrix
}  
