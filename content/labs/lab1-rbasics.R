## ----arithmetic--------------------------------------------------------------------------
# addition
2 + 1 

# subtraction
2 - 1

# multiplication
2*3

# division
2/3

# exponentiation
3^2

# parentheses for order of operations: compare
(2 + 1)/4
2 + 1/4


## ----functions---------------------------------------------------------------------------
# square root
sqrt(9)

# exponential function e^x
exp(1)

# logarithm: compare, default base is e (i.e., natural log)
log(2, base = exp(1))
log(2)


## ----name assignment---------------------------------------------------------------------
# store a value
x <- log(2)

# display named value
x


## ----overwriting a name assignment-------------------------------------------------------
# assign a new value the name 'x': this overwrites the first value
x <- 4


## ----your turn 1-------------------------------------------------------------------------

# sum of ages of group members

# convert to months: multiply by 12

# average number of siblings among group members



## ----data types--------------------------------------------------------------------------
# numeric
class(12)

# character
class('text')

# logical
class(TRUE)

# integer
class(12L)


## ----mixed-type arithmetic---------------------------------------------------------------
# valid operations
12*12
12L*12
12L*12L
12*TRUE
12*FALSE
12L*TRUE
12L*FALSE


## ----character arithmetic, eval = T------------------------------------------------------
# invalid operation
'12'*'12'


## ----your turn 2-------------------------------------------------------------------------

# product of true and false

# add a logical and numeric value

# check data type of the above



## ----make a vector-----------------------------------------------------------------------
# concatenate values with c(...) to form a vector
c(1, 2, 3)

# store it
x <- c(1, 2, 3)

# data type
class(x)


## ----coercing mixed data types-----------------------------------------------------------
# integer and logical -> integer
class(c(2L, TRUE))

# integer and numeric -> numeric
class(c(2L, 12))

# numeric and logical -> numeric
class(c(12, TRUE))

# character and anything -> character
class(c('text', TRUE))
class(c('text', 2L))
class(c('text', 12))


## ----vectorization-----------------------------------------------------------------------
# equivalent
2*c(1, 2, 3) # vectorized
c(2, 2, 2)*c(1, 2, 3) # verbose
c(2*1, 2*2, 2*3) # verbose

# equivalent
c(1, 2, 3) + 1 # vectorized
c(1, 2, 3) + c(1, 1, 1) # verbose
c(1 + 1, 2 + 1, 3 + 1) # verbose

# equivalent
c(1, 2, 3)/3 # vectorized
c(1, 2, 3)/c(3, 3, 3) # verbose
c(1/3, 2/3, 3/3) # verbose

# equivalent
c(1, 2, 3)^2 # vectorized
c(1, 2, 3)^c(2, 2, 2) # verbose
c(1^2, 2^2, 3^2) # verbose


## ----operations between vectors of unequal lengths, eval = T-----------------------------
# can you figure out what calculation was performed?
c(1, 2, 3)*c(4, 5)


## ----vectorized functions----------------------------------------------------------------
sqrt(c(1, 4, 9))
log(c(1, 10, 100), base = 10)
exp(c(0, 1, 2))


## ----vector indexing---------------------------------------------------------------------
# define a vector
x <- c(10, 20, 30, 40)

# second element
x[2]

# first and third elements
x[c(1, 3)]

# second through fourth elements
x[2:4]


## ----missing values----------------------------------------------------------------------
# vector with a missing third element
c(1, 2, NA, 4)

# note: still numeric
class(c(1, 2, NA, 4))


## ----your turn 3-------------------------------------------------------------------------

# vector of ages in years

# convert to months with vectorized arithmetic

# pretend one person is absent: input a missing value

# repeat conversion to months: how is the NA handled?


## ----matrix, eval = T--------------------------------------------------------------------
x <- c(1, 2, 3, 4)
matrix(data = x, nrow = 2, ncol = 2, byrow = TRUE)


## ----array, eval = T---------------------------------------------------------------------
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
array(data = x, dim = c(2, 2, 2))


## ----list, eval = T----------------------------------------------------------------------
# make a list
list('anchor', c(22, 5), log)

# the list elements can be named, which makes for easy retrieval
my_list <- list(words = 'anchor', numbers = c(22, 5), functions = log)
my_list$words

# even so, indexing can still be used
my_list[[1]]


## ----data frames, eval = T---------------------------------------------------------------
# make a data frame
data.frame(col1 = c(1, 2, 3, 4), 
           col2 = c(T, F, T, T), 
           col3 = c('red', 'blue', 'green', 'yellow'))

# unlike matrices, however, columns can be retrieved by name
my_df <- data.frame(number = c(1, 2, 3, 4), 
                    truth = c(T, F, T, T), 
                    color = c('red', 'blue', 'green', 'yellow'))
my_df$color


## ----your turn 4-------------------------------------------------------------------------

# data frame with columns age, no. of siblings, and favorite singer; each row corresponds to one group member

