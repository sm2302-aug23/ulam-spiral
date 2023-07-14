library(tidyverse)
ubdblue <- "#325494"
ubdyellow <- "#F5C946"

# Source code from https://github.com/johnistan/ulam-spirals-R
# Formatted by me

Ulam.Spiral <- function(N) {
  
  if (N %% 2 == 0) {
    cat(sprintf("Error: function only accepts odd integers because it a poorly written and fragile piece of code.\n"))
  } else {
    
    m <- matrix(NA, nrow = N, ncol = N)
    
    top_left <- c(1, 1)
    bottom_right <- c(N, N)
    top_right <- c(1, N)
    bottom_left <- c(N, 1)
    
    n <- N
    a <- N
    m[median(1:N), median(1:N)] <- 1
    
    while(a >= 3) {
      
      # This is an adaptation of a Euler Problem 28 solution. It calculates the diaginals. 
      m[bottom_right[1], bottom_right[2]] <- a^2 #bottom right
      m[top_left[1], top_left[2]] <- a^2 - 2 * a + 2 # top left
      m[top_right[1], top_right[2]] <- a^2 - 3 * a + 3 # top right
      m[bottom_left[1], bottom_left[2]] <- a^2 - a + 1 #bottom left
      
      # Both Horizontal Rows
      m[top_left[1], (top_left[2] + 1):(top_right[2] - 1)] <- seq(m[top_left[1], top_left[2]] - 1, m[top_left[1], top_right[2]] + 1) #Fill in top row
      m[bottom_left[1], (bottom_left[2] + 1):(bottom_right[2] - 1)] <- seq(m[bottom_left[1], bottom_left[2]] + 1,  m[bottom_left[1], bottom_right[2]] - 1) #Fill in bottom row
      
      # Left Vertical Rows
      m[(top_left[1] + 1):(bottom_left[1] - 1), top_left[1]] <- seq(m[top_left[1], top_left[1]] + 1,  m[bottom_left[1], top_left[1]] - 1) #Left Hand Row
      
      # Right Verical Row
      m[(top_right[1] + 1):(bottom_right[2] - 1), top_right[2]] <- seq(m[top_right[1], top_right[2]] - 1 , m[top_right[1], top_right[2]] - (a - 2))
      
      # drop down on square and repeat
      a <- a - 2
      top_left <- c(top_left[1] + 1, top_left[2] + 1)
      bottom_right <- c(bottom_right[1] - 1, bottom_right[2] - 1)
      top_right <- c(top_right[1] + 1, top_right[2] - 1)
      bottom_left <- c(bottom_left[1] - 1, bottom_left[2] + 1)
      
    }
    
    return(m)   
  }
}

Prime.Marker <- function(N, ulamSpiral) {
  
  m <- matrix(NA, nrow = (N^2), ncol = 4)
  
  for (i in seq(N)) {
    for(j in seq(N)) {
      m[ulamSpiral[i, j], ] <- c(ulamSpiral[i, j], j, i, IsPrime(ulamSpiral[i, j]))
    }
  }
  
  m <- as.data.frame(m)
  colnames(m) <- c("n", "x", "y", "p")
  
  return(m)
}

# Prime Number Checker stolen from http://librestats.wordpress.com/2011/08/20/prime-testing-function-in-r/
# Thanks to librestats
IsPrime <- function(n) { 
  if ((n - floor(n)) > 0) {
    cat(sprintf("Error: function only accepts natural number inputs\n"))
  } else if (n < 1) {
    cat(sprintf("Error: function only accepts natural number inputs\n"))
  } else if (try(is.vector(primes), silent = TRUE)) {
    
    if (n %in% primes) {
      TRUE
    } else if (n < tail(primes, 1)) {
      FALSE
    } else if (n <= (tail(primes, 1))^2) {
      flag <- 0
      for (prime in primes) {
        if (n %% prime == 0) {
          flag <- 1
          break
        }
      }
      if (flag == 0) {
        TRUE
      } else {
        FALSE
      }
    } else {
      last_known <- tail(primes, 1)
      while ((last_known)^2 < n) {
        assign("primes", c(primes, GetNextPrime(primes)), envir = .GlobalEnv)
        last_known <- tail(primes, 1)
      }
      IsPrime(n)
    }
  } else {
    assign("primes", PrimesBelow(n, below.sqrt = TRUE), envir = .GlobalEnv)
    IsPrime(n)
  }
}

# Get next prime
GetNextPrime <- function(primes) { 
  i <- tail(primes, 1)
  while (TRUE) {
    flag <- 0
    i <- i + 2
    if (i %% 6 == 3) {
      flag <- 1
    }
    if (flag == 0) {
      s <- sqrt(i) + 1
      possible_primes <- primes[primes < s]
      for (prime in possible_primes) {
        if ((i %% prime == 0)) {
          flag <- 1
          break
        }
      }
      if (flag == 0) {
        break
      }
    }
  }
  i
}

# Primes below specified integer n; optionally only those below sqrt(n)
PrimesBelow <- function(n, below_sqrt = FALSE) {
  if (below_sqrt) {
    m <- ceiling(sqrt(n))
  } else {
    m <- n
  }
  
  primes <- c(2, 3)
  i <- 3
  while (i < m - 1) {
    flag <- 0
    i <- i + 2
    if (i %% 6 == 3) {
      flag <- 1
    }
    if (flag == 0) {
      s <- sqrt(i) + 1
      possible_primes <- primes[primes < s]
      for (prime in possible_primes) {
        if ((i %% prime == 0)) {
          flag <- 1
          break
        }
      }
      if (flag == 0) {
        primes <- c(primes, i)
      }
    }
  }
  primes
}


# Plot small Ulam spiral -------------------------------------------------------
N <- 5
dat <- Prime.Marker(N, Ulam.Spiral(N)) %>%
  mutate(p = factor(p))
ggplot(dat, aes(x, y, fill = p, label = n)) +
  geom_tile(stat = "identity") +
  geom_text(aes(col = p), size = 5) +
  coord_equal() +
  scale_fill_manual(values = c(ubdyellow, ubdblue)) +
  scale_colour_manual(values = c(ubdblue, ubdyellow)) +
  theme_void() +
  theme(legend.position = "none")

# Plot a bigger one ------------------------------------------------------------
# Almost same code, just change N. For slides I used N = 501
N <- 251
dat <- Prime.Marker(N, Ulam.Spiral(N)) 
dat %>%
  mutate(p = factor(p)) %>%
  ggplot(aes(x, y, fill = p, label = n)) +
  geom_tile(stat = "identity") +
  coord_equal() +
  scale_fill_manual(values = c(ubdyellow, ubdblue)) +
  theme_void() +
  theme(legend.position = "none") + 
  ggtitle("Ulam spiral") -> p1

# What if the primes are randomly scattered? -----------------------------------
dat %>%
  mutate(p = sample(c(0, 1), size = nrow(dat), replace = TRUE,
                    prob = c(1 - mean(dat$p), mean(dat$p)))) %>%
  ggplot(aes(x, y, fill = factor(p), label = n)) +
  geom_tile(stat = "identity") +
  coord_equal() +
  scale_fill_manual(values = c(ubdyellow, ubdblue)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Random points") -> p2

# Save the plot ----------------------------------------------------------------
ggsave(cowplot::plot_grid(p1, p2), filename = "spiral_random.png")
