
#define the filter used in STW(1999): 
# we need closing price data for this. 

#parameters
#data -  dataset with pricing data 
#x - change in security price required to iniatie a position (x * price)
#y - change in security price required to liquidate a position (y * price)
#c - number of days a position is held
#e - alternative defn of extrema where the low/high is the most recent 
#    closing pirce that is less/greater than the previous closing price
filter_signal_x = function(data, x){
  
  n = nrow(data)
  close = data$Close  # Extract closing prices
  counter = 1  
  signals_comb1 = matrix(0, n, length(x))  # Initialize signal matrix
  
  # Loop through each x value
  for (xval in x) {  
    
    position = 0  # Initial position (0: no position, 1: long, -1: short)
    sub_high = 0
    sub_low = 0
    
    # Loop through the data points
    for (i in 2:n) { 
      
      if (position == 0) {  # No position yet
        if (close[i] >= (1 + xval) * close[i-1]) {  # Buy signal
          sub_high = close[i]
          position = 1
          signals_comb1[i, counter] = position
        } else if (close[i] <= (1 - xval) * close[i-1]) {  # Sell signal
          sub_low = close[i]
          position = -1
          signals_comb1[i, counter] = position
        } else {
          signals_comb1[i, counter] = position
        }
        
      } else if (position == 1) {  # Already in a long position
        if (close[i] <= (1 - xval) * sub_high) {  # Exit long position and go short
          sub_high = 0
          position = -1
          signals_comb1[i, counter] = position
        } else {  # Stay in long position
          sub_high = max(close[i], sub_high)
          signals_comb1[i, counter] = position
        }
        
      } else if (position == -1) {  # Already in a short position
        if (close[i] >= (1 + xval) * sub_low) {  # Exit short position and go long
          sub_low = 0
          position = 1
          signals_comb1[i, counter] = position
        } else {  # Stay in short position
          sub_low = min(close[i], sub_low)
          signals_comb1[i, counter] = position
        }
      }
    }
    
    counter = counter + 1  # Move to the next x value
  }
  
  # Set column names for x values
  colnames(signals_comb1) <- paste("Signal_x_", x, sep = "")
  
  return(signals_comb1)
}


    
#case 2 
filter_signal_x_e = function(data, x, e){
  
  n = nrow(data)  
  p = length(x)
  q = length(e)
  close = data$Close  
  signals_comb2 = matrix(0, n, p*q, byrow=F)  # Initialize signal matrix
  
  counter = 1  # Initialize counter for x and e combinations
  
  # Loop through x and e combinations
  for(xval in x){
    for(eval in e){
      
      position = 0
      for(i in (eval+1):n){  # Loop from eval+1 to n
        if(position == 0){  # No position
          if(close[i] >= (1 + xval) *max(close[i-1:eval])){  # Buy signal
            position = 1 
            signals_comb2[i, counter] = position
            
          } else if(close[i] <= (1 - xval) * min(close[i-1:eval])){  # Sell signal
            position = -1
            signals_comb2[i, counter] = position
            
          } else {
            signals_comb2[i, counter] = position
          }
          
        } else if(position == 1){  # Already in a long position
          if(close[i] <= (1 - xval) * max(close[(i-eval):(i-1)])){
            position = -1  
            signals_comb2[i, counter] = position
            
          }else{  # Exit long and go short
            signals_comb2[i, counter] = position
            
          } 
          
        }else if(position == -1){  # Already in a short position
          if(close[i] >= (1 + xval) * min(close[(i-eval):(i-1)])){ 
            position = 1
            signals_comb2[i, counter] = position
            
          } else{  # Exit short and go long
            signals_comb2[i, counter] = position
          }
        }
      } 
      
      counter = counter + 1  # Move to the next combination of x and e
    }
  }   
  
  # Set column names based on x and e values
  colnames(signals_comb2) <- paste("Signal_x_", rep(x, each=q), "_e_", rep(e, times=p), sep = "")
  
  return(signals_comb2)
}


#case 3
filter_signal_x_y = function(data, x, y) {
  
  n = nrow(data)
  close = data$Close  # Extract closing prices
  
  # Create a data frame of all combinations where y < x
  combinations = expand.grid(x = x, y = y)
  valid_combinations = combinations[combinations$y < combinations$x, ]
  count_combinations = nrow(valid_combinations)
  
  # Initialize signals_comb1 matrix with the correct number of columns
  signals_comb1 = matrix(0, n, count_combinations)
  
  # Loop through each valid combination of x and y
  for (counter in 1:count_combinations) {
    xval = valid_combinations$x[counter]
    yval = valid_combinations$y[counter]
    
    position = 0  # Initial position (0: no position, 1: long, -1: short)
    highest_price = 0 # Keep track of the highest price in a long position
    lowest_price = 0   # Keep track of the lowest price in a short position
    
    # Loop through the data points
    for (i in 2:n) { 
      
      if (position == 0) {  # No position yet
        if (close[i] >= (1 + xval) * close[i-1]) {  # Buy signal
          position = 1
          highest_price = close[i]  # Reset the highest price when entering a long position
          signals_comb1[i, counter] = position
        } else if (close[i] <= (1 - xval) * close[i-1]) {  # Sell signal
          position = -1
          lowest_price = close[i]  # Reset the lowest price when entering a short position
          signals_comb1[i, counter] = position
        }
        
      } else if (position == 1) {  # Already in a long position  
        
        if (close[i] <= (1 - yval)*highest_price  && close[i] >=  (1 - xval) * highest_price) {  # Sell condition
          position = 0
          signals_comb1[i, counter] = position
        } else if (close[i] < (1 - xval) * highest_price) {  # Exit long position
          position = -1 
          lowest_price = close[i]
          signals_comb1[i, counter] = position
        } else { 
          highest_price = max(highest_price, close[i]) 
          signals_comb1[i, counter] = position
        }
        
      } else if (position == -1) {  # Already in a short position
        if (close[i] >= (1 + yval) * lowest_price && close[i] <=  (1 + xval) * lowest_price) {  # Buy condition
          position = 0
          signals_comb1[i, counter] = position
        } else if (close[i] > (1 + xval) * lowest_price) {  # Exit short position
          position = 1
          highest_price = close[i]
          signals_comb1[i, counter] = position
        } else { 
          lowest_price = min(lowest_price, close[i]) 
          signals_comb1[i, counter] = position
        }
      }
    }
  }
  
  # Create column names for the signals_comb1 matrix
  colnames(signals_comb1) <- paste("Signal_x", valid_combinations$x, "y", valid_combinations$y, sep = "_")
  
  return(signals_comb1)
}













