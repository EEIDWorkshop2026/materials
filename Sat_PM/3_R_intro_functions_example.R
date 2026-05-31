# Start from a fresh workspace
rm(list=ls()) # clear workspace
if (!is.null(dev.list())){dev.off()} # clear figures
cat("\014") # clear console

# Required Libraries
# none

# Functions take input and produce output

# Functions can take no arguments
hello_world <- function() {
  return(print("Hello world!"))
}
# call function
hello_world()
hello_world(2) # this gives an error

# Functions can take one argument
# Set default arguments for your functions
my_name_function <- function(name = "John Doe") {
  return(paste0("Hello, my name is ", name, "."))
}
# call function
my_name_function()
my_name_function("Mickey Mouse")


# Function that adds one
add_one_func <- function(x){
  y = x+1
  return(y)
}
# call function
add_one_func(2)
add_one_func(2,2)


# Functions can take two arguments
# Function that adds two numbers
add_func <- function(x,y){
  z = x + y
  return(z)
}
add_func(5,6)

a = 1
add_func_a <- function(x,y){
  z = x + y + a
  return(z)
}
add_func_a(5,6)

# Function that takes the difference of two numbers
diff_func <- function(x,y){
  # FILL IN
}


