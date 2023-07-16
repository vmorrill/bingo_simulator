Bingo
================
Valerie Morrill
6/28/2023

Estimating the probability of getting a bingo
=============================================

To estimate the probability of getting a bingo after x numbers are called, we will make the following assumptions about the bingo game rules:

-   the bingo-card is a standard 5x5 bingo card with 24 unique numbers distributed across the grid
-   the bingo-card has a center free space
-   x unique, random numbers between 1 and 75 are called
-   to get a bingo, you must form a horizontal, vertical, or diagonal line

We will use a simulation approach
---------------------------------

``` r
library(dplyr)
library(ggplot2)
```

### Write a function to create a random bingo-card

``` r
create_bingo_card <- function() {
  # Initialize an empty matrix for the bingo-card
  card <- matrix(0, nrow = 5, ncol = 5)
  
  # Generate random numbers for each column
  for (col in 1:5) {
    # Calculate the range of numbers for the column
    min_num <- 15 * (col - 1) + 1
    max_num <- 15 * col
    
    # Sample 5 unique numbers for the column
    column_numbers <- sample(min_num:max_num, 5, replace = FALSE)
    
    # Assign the numbers to the bingo-card
    card[, col] <- column_numbers
  }
  
  # Return the generated bingo-card
  return(card)
}
```

### Create a random bingo-card

``` r
bingo_card <- create_bingo_card()

# Print the bingo-card
print(bingo_card)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1   16   39   52   61
    ## [2,]   10   30   38   59   63
    ## [3,]   12   18   44   58   70
    ## [4,]   11   27   43   54   75
    ## [5,]    9   28   40   56   68

### Write a function to create a record-card to mark the location of numbers called on your bingo-card

``` r
create_record_card <- function() {
        
record_card <- matrix(FALSE, nrow = 5, ncol = 5)

# Replace the center number with TRUE (free blank space)
record_card[3, 3] <- TRUE

# Return the generated record-card
  return(record_card)
}
```

### Create a record-card

``` r
record_card <- create_record_card()

# Print the record-card
print(record_card)
```

    ##       [,1]  [,2]  [,3]  [,4]  [,5]
    ## [1,] FALSE FALSE FALSE FALSE FALSE
    ## [2,] FALSE FALSE FALSE FALSE FALSE
    ## [3,] FALSE FALSE  TRUE FALSE FALSE
    ## [4,] FALSE FALSE FALSE FALSE FALSE
    ## [5,] FALSE FALSE FALSE FALSE FALSE

### Write a function to mark the location of numbers called on the record-card

``` r
update_record_card <- function(number) {

if (number %in% bingo_card) {

#make bingo card a data frame
 t <- data.frame(bingo_card)

 #extract row and column location of number called
 location <- which(t==number,arr.ind=TRUE)
 row <- location[1,1]
 col <- location[1,2]
 
record_card[row,col] <- TRUE

}
    
return(record_card)
    
}
```

### Mark if we have 1, 2, 3 or 4 on the record card

``` r
record_card<- update_record_card(1)
record_card<- update_record_card(2)
record_card<- update_record_card(3)
record_card<- update_record_card(4)
print(record_card)
```

    ##       [,1]  [,2]  [,3]  [,4]  [,5]
    ## [1,]  TRUE FALSE FALSE FALSE FALSE
    ## [2,] FALSE FALSE FALSE FALSE FALSE
    ## [3,] FALSE FALSE  TRUE FALSE FALSE
    ## [4,] FALSE FALSE FALSE FALSE FALSE
    ## [5,] FALSE FALSE FALSE FALSE FALSE

### Write a function to check if a bingo is achieved

``` r
check_bingo <- function(record_card) {
  # Check rows
  if (any(rowSums(record_card) == 5))
    return(TRUE)
  
  # Check columns
  if (any(colSums(record_card) == 5))
    return(TRUE)
  
  # Check diagonals
  if (sum(diag(record_card)) == 5 || sum(diag(apply(record_card, 2, rev))) == 5)
    return(TRUE)
  
  # No bingo
  return(FALSE)
}
```

### Check for a bingo

``` r
check_bingo(record_card) 
```

    ## [1] FALSE

### Call all the rest of the possible numbers in the first column, and then check for a bingo

``` r
record_card<- update_record_card(5)
record_card<- update_record_card(6)
record_card<- update_record_card(7)
record_card<- update_record_card(8)
record_card<- update_record_card(9)
record_card<- update_record_card(10)
record_card<- update_record_card(11)
record_card<- update_record_card(12)
record_card<- update_record_card(13)
record_card<- update_record_card(14)
record_card<- update_record_card(15)
print(record_card)
```

    ##      [,1]  [,2]  [,3]  [,4]  [,5]
    ## [1,] TRUE FALSE FALSE FALSE FALSE
    ## [2,] TRUE FALSE FALSE FALSE FALSE
    ## [3,] TRUE FALSE  TRUE FALSE FALSE
    ## [4,] TRUE FALSE FALSE FALSE FALSE
    ## [5,] TRUE FALSE FALSE FALSE FALSE

``` r
check_bingo(record_card) 
```

    ## [1] TRUE

### Write a function to simulate a bingo game with x numbers called

``` r
simulate_bingo <- function(x, bingo_card, record_card) {
        
        # Generate x random numbers between 1 and 75 without replacement (assuming a standard 75-ball bingo game)
        numbers <- sample(1:75, x, replace=FALSE)

        #For each number called
        for (i in numbers) {

                #if it is on the bingo card
                if (i %in% bingo_card) {

                        #extract row and column location of number called on the bingo card
                        t <- data.frame(bingo_card)
                        location <- which(t==i,arr.ind=TRUE)
                        row <- location[1,1]
                        col <- location[1,2]
                
                        #Mark that location on the record card
                        record_card[row,col] <- TRUE

                }
    
        }

#Check for a bingo
return(check_bingo(record_card))
    
}
```

### Simulate one bingo game with 30 numbers called. Did we get a bingo?

``` r
# Create a random bingo-card
bingo_card <- create_bingo_card()
# Create a blank record-card
record_card <- create_record_card() 

simulate_bingo(30, bingo_card, record_card)
```

    ## [1] FALSE

### Simulate 1000 bingo games with 30 numbers called. What proportion of games did we get a bingo?

``` r
# Set the number of simulations
num_simulations <- 1000

# Set x numbers
x<- 30

# Initialize a counter for successful bingos
bingo_counter <- 0

# Run the simulations
for (i in 1:num_simulations) {
        
        # Create a random bingo-card
        bingo_card <- create_bingo_card()

        # Create a blank record-card
        record_card <- create_record_card()
 
        #if bingo is true, add one to the bingo counter       
        if (simulate_bingo(x, bingo_card, record_card)) {
        bingo_counter <- bingo_counter + 1
        }
}

# Calculate the probability of getting a bingo after x numbers are called
probability <- bingo_counter / num_simulations

# Print the result
cat("Probability of getting a bingo after", x, " numbers are called:", probability, "\n")
```

    ## Probability of getting a bingo after 30  numbers are called: 0.141

### Simulate 1000 bingo games each with 1-75 numbers called. What proportion of games did we get a bingo?

``` r
# Create a data frame to store the results
bingo_results <- data.frame(matrix(ncol = 2, nrow = 75))
colnames(bingo_results) <- c("number_of_numbers", "probability_of_bingo")

# Set the number of simulations
num_simulations <- 1000
        
#For each number called
for (x in 1:75) {
        
        # Initialize a counter for successful bingos
        bingo_counter <- 0

        #For each simulation
        for (i in 1:num_simulations) {

        
                # Create a random bingo-card
                bingo_card <- create_bingo_card()

                # Create a blank record-card
                record_card <- create_record_card()
 
                #if bingo is true, add one to the bingo counter       
                if (simulate_bingo(x, bingo_card, record_card)) {
                bingo_counter <- bingo_counter + 1
                }
                
        #record the percent of bingos associated with the number of numbers called
        bingo_results[x,1]<- x
        bingo_results[x,2]<- bingo_counter / num_simulations
        }

}

# Print the result
bingo_results
```

    ##    number_of_numbers probability_of_bingo
    ## 1                  1                0.000
    ## 2                  2                0.000
    ## 3                  3                0.000
    ## 4                  4                0.000
    ## 5                  5                0.000
    ## 6                  6                0.000
    ## 7                  7                0.000
    ## 8                  8                0.000
    ## 9                  9                0.001
    ## 10                10                0.001
    ## 11                11                0.002
    ## 12                12                0.005
    ## 13                13                0.002
    ## 14                14                0.008
    ## 15                15                0.011
    ## 16                16                0.012
    ## 17                17                0.012
    ## 18                18                0.020
    ## 19                19                0.018
    ## 20                20                0.022
    ## 21                21                0.021
    ## 22                22                0.040
    ## 23                23                0.050
    ## 24                24                0.063
    ## 25                25                0.064
    ## 26                26                0.071
    ## 27                27                0.088
    ## 28                28                0.105
    ## 29                29                0.116
    ## 30                30                0.140
    ## 31                31                0.177
    ## 32                32                0.184
    ## 33                33                0.206
    ## 34                34                0.251
    ## 35                35                0.286
    ## 36                36                0.317
    ## 37                37                0.328
    ## 38                38                0.367
    ## 39                39                0.393
    ## 40                40                0.455
    ## 41                41                0.490
    ## 42                42                0.524
    ## 43                43                0.547
    ## 44                44                0.563
    ## 45                45                0.656
    ## 46                46                0.660
    ## 47                47                0.720
    ## 48                48                0.738
    ## 49                49                0.781
    ## 50                50                0.815
    ## 51                51                0.837
    ## 52                52                0.864
    ## 53                53                0.887
    ## 54                54                0.907
    ## 55                55                0.930
    ## 56                56                0.959
    ## 57                57                0.950
    ## 58                58                0.974
    ## 59                59                0.974
    ## 60                60                0.990
    ## 61                61                0.989
    ## 62                62                0.996
    ## 63                63                0.995
    ## 64                64                0.999
    ## 65                65                0.998
    ## 66                66                1.000
    ## 67                67                1.000
    ## 68                68                1.000
    ## 69                69                1.000
    ## 70                70                1.000
    ## 71                71                1.000
    ## 72                72                1.000
    ## 73                73                1.000
    ## 74                74                1.000
    ## 75                75                1.000

``` r
# Plot the result

plot(bingo_results$number_of_numbers, bingo_results$probability_of_bingo)
```

![](Bingo_github_files/figure-markdown_github/unnamed-chunk-15-1.png)

### What if there are 18 people playing bingo?

### Simulate 1000 bingo games with 30 numbers called. What proportion of games did we get a bingo?

``` r
# Set the number of simulations
num_simulations <- 1000

# Set x numbers
x<- 30

# Set the number of people
people <- 18

# Initialize a counter for successful bingos
bingo_counter <- 0

# Run the simulations
for (i in 1:num_simulations) {
        
        #For each person
              for (t in 1:people) {
                        
        
                        # Create a random bingo-card
                        bingo_card <- create_bingo_card()

                        # Create a blank record-card
                        record_card <- create_record_card()
                        
                        #If a person did not get bingo after x numbers were called, move to the next person
                        #If any person got bingo after x numbers are called, 
                        #add one to the bingo counter and stop looping over people
                        if (!simulate_bingo(x, bingo_card, record_card)) {
                        bingo_counter <- bingo_counter + 0
                        } else {
                        bingo_counter <- bingo_counter + 1
                        break
                        } 
                        
                  }

}

# Calculate the probability of getting a bingo after x numbers are called
probability <- bingo_counter / num_simulations

# Print the result
cat("Probability of getting a bingo after", x, " numbers are called when there are ", people , "people:", probability, "\n")
```

    ## Probability of getting a bingo after 30  numbers are called when there are  18 people: 0.946

### Simulate 100 (1000 takes too long) bingo games each with 1-75 numbers called when there are 18 people. What proportion of games did we get a bingo?

``` r
# Create a data frame to store the results
bingo_results <- data.frame(matrix(ncol = 2, nrow = 75))
colnames(bingo_results) <- c("number_of_numbers", "probability_of_bingo")

# Set the number of simulations
num_simulations <- 500

# Set the number of people
people <- 18

# For each number called
for (x in 1:75) {
        
        # Initialize a counter for successful bingos
        bingo_counter <- 0

        # For each simulation
        for (i in 1:num_simulations) {
                        
                for (t in 1:people) {
                        
        
                        # Create a random bingo-card
                        bingo_card <- create_bingo_card()

                        # Create a blank record-card
                        record_card <- create_record_card()
                        
                        #If a person did not get bingo after x numbers were called, move to the next person
                        #If any person got bingo after x numbers are called, add one to the bingo counter and stop looping over people
                        if (!simulate_bingo(x, bingo_card, record_card)) {
                        bingo_counter <- bingo_counter + 0
                        } else {
                        bingo_counter <- bingo_counter + 1
                        break
                        } 
                        
                  }
                        
                        
                

        }     

        #record the percent of bingos associated with the number of numbers called
        bingo_results[x,1]<- x
        bingo_results[x,2]<- bingo_counter / num_simulations
}



# Print the result
bingo_results
```

    ##    number_of_numbers probability_of_bingo
    ## 1                  1                0.000
    ## 2                  2                0.000
    ## 3                  3                0.000
    ## 4                  4                0.000
    ## 5                  5                0.002
    ## 6                  6                0.002
    ## 7                  7                0.002
    ## 8                  8                0.010
    ## 9                  9                0.016
    ## 10                10                0.018
    ## 11                11                0.026
    ## 12                12                0.026
    ## 13                13                0.060
    ## 14                14                0.074
    ## 15                15                0.116
    ## 16                16                0.164
    ## 17                17                0.194
    ## 18                18                0.216
    ## 19                19                0.306
    ## 20                20                0.346
    ## 21                21                0.436
    ## 22                22                0.444
    ## 23                23                0.540
    ## 24                24                0.618
    ## 25                25                0.696
    ## 26                26                0.748
    ## 27                27                0.818
    ## 28                28                0.876
    ## 29                29                0.906
    ## 30                30                0.946
    ## 31                31                0.964
    ## 32                32                0.982
    ## 33                33                0.986
    ## 34                34                0.994
    ## 35                35                0.998
    ## 36                36                1.000
    ## 37                37                1.000
    ## 38                38                1.000
    ## 39                39                1.000
    ## 40                40                1.000
    ## 41                41                1.000
    ## 42                42                1.000
    ## 43                43                1.000
    ## 44                44                1.000
    ## 45                45                1.000
    ## 46                46                1.000
    ## 47                47                1.000
    ## 48                48                1.000
    ## 49                49                1.000
    ## 50                50                1.000
    ## 51                51                1.000
    ## 52                52                1.000
    ## 53                53                1.000
    ## 54                54                1.000
    ## 55                55                1.000
    ## 56                56                1.000
    ## 57                57                1.000
    ## 58                58                1.000
    ## 59                59                1.000
    ## 60                60                1.000
    ## 61                61                1.000
    ## 62                62                1.000
    ## 63                63                1.000
    ## 64                64                1.000
    ## 65                65                1.000
    ## 66                66                1.000
    ## 67                67                1.000
    ## 68                68                1.000
    ## 69                69                1.000
    ## 70                70                1.000
    ## 71                71                1.000
    ## 72                72                1.000
    ## 73                73                1.000
    ## 74                74                1.000
    ## 75                75                1.000

``` r
# Plot the result

plot(bingo_results$number_of_numbers, bingo_results$probability_of_bingo)
```

![](Bingo_github_files/figure-markdown_github/unnamed-chunk-18-1.png)
