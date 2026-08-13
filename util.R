# PLACE 8 RANDOM CORALS

# Step 1: logic
reef <- matrix(0, nrow = 5, ncol = 5)
random_coral <- sample(1:25, size = 8)
for (rc in random_coral) {
  rc_row <- (rc - 1) %/% 5 + 1
  rc_col <- (rc - 1) %% 5 + 1
  reef[rc_row, rc_col] <- 1
}
reef

# Step 2: Determine Inputs
  # How many corals we want to place
  # Size of the reef
  # Sample size

# Step 3: Write the function call

# reef <- random_corals(num_corals, reef_size)

# Step 4: Defining the function

random_corals <- function(num_corals, reef_size) {
  reef <- matrix(0, nrow = reef_size, ncol = reef_size)
  random_coral <- sample(1:(reef_size*reef_size), size = num_corals)
  for (rc in random_coral) {
  rc_row <- (rc - 1) %/% reef_size + 1
  rc_col <- (rc - 1) %% reef_size + 1
  reef[rc_row, rc_col] <- 1
  }
  return(reef)
}

# Growth Function 

growth <- function(reef, roll, row, col) {
  growth_row_offset <- c(-1, -1, -1, 0, 1, 1, 1, 0)
  growth_col_offset <- c(-1, 0, 1, 1, 1, 0, -1, -1)

  growth_row <- row + growth_row_offset[roll]
  growth_col <- col + growth_col_offset[roll]
  in_bounds <- growth_row >= 1 && growth_row <= 5 && growth_col >= 1 && growth_col <= 5
  if (in_bounds) {
   reef[growth_row, growth_col] <- 1 
  }
  return(reef)
}


roll2d6 <- function() {
  sum(sample(1:6, size = 2, replace = FALSE))
}
  