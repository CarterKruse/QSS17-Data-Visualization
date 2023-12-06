## Data Visualization (GOVT16-QSS17) Fall 2022
## Introduction To R
##
## Professor Robert A. Cooper
## Week 1

## First, let's open a new project. Next, let's install a package ('tidyverse').
## R has a lot of code already in its base programming. 

## That said, there is a lot of code out there to be installed as 'packages'. 
## You install said code from CRAN repository, or maybe Github if it's a 
## developer version. There are at least 3 ways to install a package.

install.packages("tidyverse") 

## Go to Tools -> Install Packages
## Go to Packages tab in plotting window, click Install, type package name.

## Once you have the package downloaded, you must activate/attach it. 
library(tidyverse)

## Next, let's practice creating an object. 
banana <- 15

## To show what's contained in the object, type it and enter or print() it. 
banana
print(banana)

## You can create multiple objects and interact them.
is.vector(banana)

apple <- 6
banana * apple

## Warning: You can easily overwrite objects, which is an easy mistake to make.
## Lesson: More often than not, it's a bad idea to overwrite objects.  
apple <- 8

## Objects
## Objects are containers with labels. Inside is some piece of information.
## Things in the R universe have classification, like plants v. animals. 
## Certain functions and forms work with certain classes and not others. 

## First Classification Distinction: Homogeneous or Heterogeneous?
## Some objects are all the same inside. Some can be made up of different types.

## Homogeneous: Vectors, Matrices, Arrays
## Heterogeneous: Data Frames, Lists

## The assignment operator. '<-' or '='

## ("Atomic") Vectors
## Vectors are 'flat' or one-dimensional. They have length, but not more than
## one dimension.

## Types: Double/Numeric, Integer, Factor, Character/String, Logical
## Rare Types: Complex, Raw

a <- 4 # This is actually a vector of length 1.

## There is a slight difference between typeof() and class().
typeof(a) # "double"
class(a) # "numeric"

b <- 3
d <- 5
e <- 7

## You can combine/create vectors with the c() function ('combine' or 'concatenate').

## Vectors stay 1D, even when nesting. 
numbers <- c(a, b, d)
class(numbers)

numbers2 <- c(c(a, b), c(d, e)) # Alternatively: c(a, b, d, e)
numbers; numbers2
numbers2

## Vectors can take names with the names() function.
## The 'names' of the vector are empty until taking the assignment.
names(numbers2) <- c("a", "b", "d", "e")
numbers2 # Now, when printing 'numbers2', the name labels are displayed as well. 
class(numbers2)

## Integer Vector
## The 'L' is the way to indicate integer/no decimal.
int1 <- 3L; int1

int2 <- c(5L, 29L); int2
class(int2)

int3 <- c(int1, int2); int3
typeof(int3); class(int3)

## Character/String/Text Vector
dart <- c("Dartmouth", "Rules"); dart
typeof(dart)
class(dart)

## Logical Vector (TRUE/FALSE)
## Understanding these is very important to data transformation. 
logic1 <- TRUE
logic2 <- c(FALSE, FALSE, TRUE, TRUE, FALSE)
logic3 <- c(logic1, logic2)
logic3
class(logic3)

## Logical Operators

## The result is a TRUE/FALSE vector.
## You can use rules with comparison/logical operators to your advantage.
numbers2 > 3
numbers2 != 3

##  Factors

## Factors are a combination of integers and character strings, in a sense. 
## They exist for categorical variables, and have integer values and labels. 
## Factors have predetermined acceptable levels. If you do not set the predefined
## levels, all unique values will become levels.

## 'letters; is a vector of the English alphabet. 
letterz <- sample(letters, 100, replace = TRUE)

letfact <- factor(letterz)
letfact # There is no need for quotations, because it is no longer a string variable.

## We can check the levels of the factor with 'levels' or with 'attributes'
levels(letfact)
attributes(letfact)

class(letfact) # You see and work with it as a 'factor' with a label.
typeof(letfact) # R stores it as an integer in the memory.

## If we instead define the set of acceptable levels, the variable changes. 
letfact2 <- factor(letterz, levels = c("a", "b", "c", "d"))
letfact2

## If we want an ordered factor, add the ordered = TRUE argument.
## When you print an ordered factor, you can see the levels and their order. 
letfact3 <- factor(letterz, levels = c("a", "b", "e", "d", "c"), ordered = TRUE)
letfact3

letfact3[5]
letfact3[6]

## Depending on the earlier sample function, one will be true, the other false. 
letfact3[6] > letfact3[5]
letfact3[5] > letfact3[6] 

summary(letfact3)
table(letfact3)

## Coercion

## When two objects of different classes are combined in a vector, one is 
## 'coerced' into the class of the other. The best way to determine the rules
## is through two-way competitions, rock-paper-scissors style.

vect1 <- c(numbers, int3); class(vect1) # numerics beat integers
vect1

vect2 <- c(numbers, dart); class(vect2) # characters/strings beat numerics 
vect2

vect3 <- c(int3, dart); class(vect3) # characters/strings beat integers
vect3

vect4 <- c(logic3, numbers); class(vect4) # numerics beat logicals
vect4

vect5 <- c(dart, logic3); class(vect5) # characters beat logicals
vect5

vect6 <- c(dart, letfact); class(vect6) # characters beat factors 
vect6

vect7 <- c(numbers, letfact); class(vect7) # numerics beat factors
vect7

vect8 <- c(int3, letfact); class(vect8) # integers beat factors
vect8

vect9 <- c(logic3, letfact); class(vect9) # integers beat logicals/factors
vect9

## Just so we are clear what just happened. 

logic3
letfact

## Lesson: 'characters' will coerce anything and 'integers' often linger beneath. 

## Matrices

mat1 <- matrix(1:9, nrow = 3, byrow = FALSE); mat1

## Matrices have two dimensions. Row and column.
## Matrices are defined by row x column and whether you fill by row or column. 

dim(mat1)

## They cannot take different types of data or classes of objects. 
## They can be different types of objects, as long as each element is the same.

mat2 <- matrix(letters, ncol = 2, byrow = TRUE)
mat3 <- matrix(letters, ncol = 2, byrow = FALSE)

## Data Frames

## Let's call a data set already in base R. 
data("mtcars") # Use the 'data' function to call native R data sets.

## R has many base data sets to practice with. I encourage you to use them.
## We can look at the data with 'head' or 'glimpse' or 'str'.
# Recall 'class' and 'typeof' can occasionally be different. 

class(mtcars)
typeof(mtcars)

head(mtcars)
glimpse(mtcars)
str(mtcars) # 'structure'

## Use the double colon to specify exactly what package a function is coming from.
## This is most helpful when two packages have functions with the same name.
## When this happens, one package function overtakes the other ('masking').

## Let's work with mtcars. 

head(mtcars)

## First, let's work through some base R sub-setting of a data frame. 
## Using hard brackets, we can subset by row or column, as we see fit. 

mtcars[3, ] # Row by column. Thus, row on the left of comma, column on the right. 

## Data frames can take row and column names as identifiers. 
head(mtcars)
dim(mtcars)

mtcars[4, ] # Extracts the fourth row.
mtcars[, 4] # Extracts the fourth column's values.
mtcars[3, 7] # The quarter-mile time for the Datsun 710.

mtcars[ ,"disp"]
mtcars["Datsun 710", ]
mtcars["Datsun", 3]

mtcars$disp[7] # mtcars[7, "disp"]

rownames(mtcars)
colnames(mtcars)

## Lists. 
## Lists have length, but do not have dimension. 
## Lists can take many different types and classes of objects. 

objects()

list1 <- list(mtcars, dart, apple, banana, vect5, mat1)
str(list1)

length(list1)
dim(list1) # Returns 'NULL' 

# What happens if we just print it?

list1
list1[2] # Points to the list element. 
list1[[2]] # Gets you inside the list element. 

list1[[2]][2] <- "is the best"
list1

dart[1] <- "dart" # Re-Coding

list1[[1]][3, 4]
