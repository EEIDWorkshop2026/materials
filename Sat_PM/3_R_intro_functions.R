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
hello_world()

# Functions can take one argument
# Set default arguments for your functions
my_name_function <- function(name = "John Doe") {
  return(paste0("Hello, my name is ", name, "."))
}
my_name_function()
my_name_function("Mickey Mouse")


# Function that adds one
add_one_func <- function(x){
  new_value <- x + 1
  return(new_value)
}
add_one_func(3)
add_one_func(25)


# Functions can take two arguments
# Function that adds two numbers
add_func <- function(x,y){
  added_value <- x + y
  return(added_value)
}
add_func(3,4)
add_func(3) # only one input gives an error

# Function that takes the difference of two numbers
diff_func <- function(x,y){
  xydiff <- abs(x-y)
  return(xydiff)
}
diff_func(134,56)
diff_func(56,134)


