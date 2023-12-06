## Data Visualization (GOVT16-QSS17) Fall 2022
## Introduction To R
##
## Name: Carter Kruse
## Week 1

## R BASICS & OBJECTS

## This video shows objects in R as containers and how they work.
## This is a comment. R will not read it.

## The line below creates an object 'apple' and gives it the value 7.
apple <- 7

apple

## Numeric/Double
banana <- 8; banana

fruit <- apple * banana

print(fruit)

class(apple)

## c() is the 'concatenate' function
basket <- c(1, 5, 8)
basket

basket * apple

## Vectors - A vector is a set of data pieces or elements of the same type.
## If you try to combine different object classes into one...

a <- c(1, 5, 14, 8); a

## Interaction Of Classes

apple * a

## Types Of Objects

## R 'coerces' objects of different types.

## See below, even the previous object 'a' can be added to a vector.
## The data below are three numerics and one integer and are coerced to a numeric.

mix <- c(1, 3L, 8.65, a); class(mix)
mix

## This time, I add a string variable to the end of numeric data.
## R will coerce this string to one type of object, the most flexible.
## In this case, as a 'character' variable.

mix2 <- c(1.5, 44, 7.33, -8L, "cat"); mix2; class(mix2)

## Factors

## Factors are for categorical data. R applies integers to labels and accepts 
## certain "levels" only.
## Factors are one of the most important things to learn in R.

## A factor contains predefined values in a vector.
## I create a simple character/string vector of values below, a bunch of colors.

colors <- c("red", "orange", "yellow", "red", "orange", "yellow", "green", "orange")

class(colors) # Characters/Strings

## I create another object that is a copy of the 'colors' object, but now I make 
## it a factor.

colors_factor <- factor(colors, levels = c("red", "orange", "yellow", "green", "blue"))
class(colors_factor)
colors_factor

table(colors_factor) # Allowed levels included blue, but no blues are counted.

## Factors are categorical variables. Thus, they can be un-ordered or ordered.
## You can order your factor inside the factor() function with another simple argument.
## ordered = TRUE

## I make up a vector of values. Let's say they are from some small survey.
agree <- c("agree", "disagree", "disagree", "strongly agree", "don't know",
           "agree", "strongly disagree")

## I turn the survey variable into a factor and make sure the values are ordered.
agree_factor <- factor(agree, levels = c("strongly agree", "agree", "don't know",
                                         "disagree", "strongly disagree"),
                       ordered = TRUE)

table(agree_factor) # We can count factor observations with the table() function.

## We can see the levels of a factor variable at any time with the levels() function.
levels(agree_factor)

## If done correctly, then 'strongly agree' should have the lowest integer value
## underneath and 'strongly disagree' the highest.
## We can check by using functions like min() and max() to see if the ordered
## part works.

min(agree_factor)
max(agree_factor)

## That's enough about factors for now.

## MATRICES

## Matrices have two dimensions and each element within must be the same type.

## A matrix is just an arrangement of quantities in rows and columns, a 
## rectangular array, if you will. In R, a matrix is its own type of object.
## Matrices have an 'order', or dimensions. Always row by column.

## 1:25 is just the numbers from 1 to 25 in order.
matrix1 <- matrix(1:25, nrow = 5); matrix1

## Produces identical matrix to matrix1.
matrix2 <- matrix(1:25, ncol = 5); matrix2

## How do we subset matrices or choose specific elements from a matrix?
## With brackets and row by column.

matrix1[2, 3] # Grab the number from the second row, third column.

## What if I want to re-code that element? Let's say I recorded the matrix wrong.
## Instead of creating a new object, we can replace a particular element with
## the assignment operator.

matrix1[2, 3] <- 999
matrix1

## You can transform/re-code more than one element at a time, say an entire
## column/variable.

## Indicating that you are not specifying a specific row, but rather all of them.
matrix1[ , 4] <- NA
matrix1

## DATA FRAMES

## Data frames are how we interact with data most often. They are lists that are
## reorganized to look like matrices, in a nutshell.

## This is the type of object you will most likely deal with the majority of the
## time you are doing data analysis.
## Variants of this type of object include: data table, tibble, etc.
## Many, but not all, of the functional features of matrices and data frames
## overlap.

## The line below creates a data frame with the data.frame() function.
data1 <- data.frame(x = c(1:10),
                    y = c(21:30),
                    z = c(91:100))

## Print data1 and look at it.
data1

## We can subset a data frame just like we do with matrices, but we also have
## other subset operators available to us. We can use brackets (as in matrix[ , ]),
## or we can use the $ subset operator.

## Extract from 'data1' the element from row 6 and column 3. There are multiple
## ways to do this.

data1[6, 3] # Using pure matrix notation on a data frame will work just fine.

## Using the '$' subset operator, we can extract the third variable by its name, 'z'.
data1$z

## The variable 'z' is not an object by itself, it only exists within data1.
## z

## When you subset, you are then 'inside' the vector z, meaning there are no
## dimensions, just length.
## So, if you want to get to a particular element/observation this way, you don't
## use a comma in the brackets.

## data1$z[4, ] # This gives an error.

## This is correct, and gets you to the fourth observation inside the 'z' vector.
data1$z[4]

## Finally, let's say we want to re-code based on a value we knew was wrong.
## We can reference either (a) the sequence in the vector OR (b) the actual
## value we want to replace.

## All four of the following lines do the exact same thing.

data1

data1[7, 3] <- 4
data1$z[7] <- 999

## Using a logical to re-code any value that is equal to 97.
data1$z[data1$z == 999] <- 777 # Especially useful for changing multiple values.

data1$z[data1$x == 7] <- 888

## Lists

## Lists only have "length", but each element can be of any type. Very powerful 
## and flexible class.

## Lists are their own beasts. A list can contain objects of any dimensions
## within them in a sequence.
## Lists can have numbers, characters, vectors, data frames, matrices, spatial
## data sets, other lists, and more in them.

## Example - Using a bunch of objects we have created in this script and shoving
## them into a list.

list_1 <- list(apple, banana, a, mix, colors, agree_factor, matrix2, data1)
list_1

## You can put lists inside of other lists ('nested').
list_2 <- list(apple, matrix1, agree)
list_3 <- list(list_1, list_2); list_3

## The key to lists is the [[]] (double brackets) notation.
## Using one set of brackets 'points' you to an element in the list.
## Using two sets of brackets gets you inside that titem so that you can change
## the values or use them.

list_1[3] # This 'points' you to the third item, the vector 'a'.

## This gets you inside that vector, so that you can do things with the
## elements/values inside it.
list_1[[3]]

list_1[[3]][4] # vs list_1[3][4]

