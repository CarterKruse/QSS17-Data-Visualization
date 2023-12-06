## Data Visualization (GOVT16-QSS17) Fall 2022
## Intro to R, Part 1 & 2
##
## Name: Carter Kruse
## Date: September 19th, 2022

## Objects, Vectors, & Matrices
## 1) Objects
## Creating various objects and assigning values to them.
Harry <- 50; Harry
Hermione <- 47; Hermione
Ron <- 44; Ron
Ginny <- 40; Ginny

## Proof that 'Harry' and 'Ginny' together are not greater than 'Ron' and 'Hermione'
## using a logical operator.
Harry + Ginny > Ron + Hermione

## Creating an object and assigning a value to it.
Snape <- 49; Snape

## Creating an object that is the multiplicative inverse of 'Harry'.
Voldemort <- 1 / Harry; Voldemort

## Proof that 'Harry' and 'Voldemort' are inverses of each other.
Voldemort * Harry

## 2) Basic Math - No Creation Of Objects
## Order of operations is key.
7546 + 9918
3467 - 8493
37 * 47 + 30
(3 + 67) * (8 - 29)
656 + 23/53
74 ^ 2
999 %% 77 # The modulo operator is two of '%'.

## 3) (Atomic) Vectors
## Creating a vector of 'Harry' and 'Hermione', and a vector of 'Ginny' and 'Ron'.
wizards1 = c(Harry, Hermione); wizards1 
wizards2 = c(Ginny, Ron); wizards2

## Attaching the vectors tail to head.
wizards = c(wizards1, wizards2); wizards

## Adding labels/names to the vector, depending on the names of the wizards.
names(wizards) <- c("Harry", "Hermione", "Ginny", "Ron"); names(wizards)

## Removing 'Snape' and 'Voldemort' from R's memory.
remove(Snape, Voldemort)

## Checking to make sure the objects are gone. Proof: Error message.
## Using the 'print()' function when typing each object individually.
print(Snape)
print(Voldemort)

## Extracting the value for 'Ginny' from 'wizards' using brackets.
wizards["Ginny"] # Alternatively: wizards[3]

## Extracting the values for 'Hermione' and 'Ron' from 'wizards' using one command.
wizards[c("Hermione", "Ron")] # Alternatively: wizards[c(2, 4)]

## Extracting the values for everyone but 'Harry', using a colon in sub-setting.
wizards[c(2:4)] # Alternatively: wizards[c("Hermione", "Ginny", "Ron")]

## Using a logical operator to show whose scores are lower than 45.
wizards < 45

## 4) Matrices
## Creating a vector named 'scores'.
scores <- c(75, 49, 68, 83, 97, 91, 98, 89, 91); scores

## Turing 'scores' into a matrix, with 3 rows and 3 columns.
## The vector is fed into the matrix by row.
wiz_scores <- matrix(scores, nrow = 3, ncol = 3, byrow = TRUE); wiz_scores

## The numbers in the matrix represent the grades of Ron, Harry, and Hermione.
## Labeling the rows (with names) and the columns (with test1, test2, test3).
rownames(wiz_scores) <- c("Ron", "Harry", "Hermione"); rownames
colnames(wiz_scores) <- c("test1", "test2", "test3"); colnames

## Printing 'wiz_scores' to demonstrate the effect.
wiz_scores

## Creating a new matrix object by taking 'wiz_scores' and binding it to a new
## column that represents mean grades for each of the wizards.
wiz_scores2 <- cbind(wiz_scores, rowMeans(wiz_scores)); wiz_scores2

## Adding the column name 'avg' for the averages column.
colnames(wiz_scores2)[4] <- "avg"; colnames

## Printing 'wiz_scores2' to demonstrate the effect.
wiz_scores2

## Creating a matrix called 'otherwiz' for the scores of Neville and Ginny.
otherwiz <- matrix(c(81, 80, 78, 92, 87, 84), nrow = 2, ncol = 3, byrow = TRUE); otherwiz

## Setting up the matrix similar to the previous.
rownames(otherwiz) <- c("Neville", "Ginny"); rownames
colnames(otherwiz) <- c("test1", "test2", "test3"); colnames

## Printing 'otherwiz' to demonstrate the effect.
otherwiz

## Creating a column with the average scores for Neville and Ginny.
otherwiz <- cbind(otherwiz, rowMeans(otherwiz)); otherwiz

## Adding the column name 'avg' for the averages column.
colnames(otherwiz)[4] <- "avg"; colnames

## Printing 'otherwiz' to demonstrate the effect.
otherwiz

## Binding the scores for Neville and Ginny to the scores for the other wizards.
all_wiz_scores = rbind(wiz_scores2, otherwiz); all_wiz_scores

## Extracting Hermione's second and third test scores using hard brackets.
## Hard brackets are the matrix sub-setting operator.
all_wiz_scores["Hermione", c("test2", "test3")]

## Creating a matrix that subsets 'all_wiz_scores" by eliminating 'test2' from
## the matrix.
allwiz_sub <- subset(all_wiz_scores, select = -test2); allwiz_sub

## Eliminating the 'avg' column from the matrix to recalculate averages.
## This is just one way to recalculate the averages, the second will be
## demonstrated later in the script.
allwiz_sub <- subset(allwiz_sub, select = -avg); allwiz_sub

## Alternatively: allwiz_sub <- all_wiz_scores[ , c("test1", "test3")]; allwiz_sub

## Recalculating the averages for each wizard and setting the column name to 'newavg'.
allwiz_sub <- cbind(allwiz_sub, rowMeans(allwiz_sub)); allwiz_sub
colnames(allwiz_sub)[3] <- "newavg"; colnames

## Printing 'allwiz_sub' to demonstrate the effect.
allwiz_sub

## Changing Neville's 'test3' score by re-coding the single element in the matrix.
allwiz_sub['Neville', 'test3'] <- 98; allwiz_sub

## Recalculating the averages, by using sub-setting (brackets) and the subset() function.
allwiz_sub[ , 'newavg'] <- rowMeans(subset(allwiz_sub, select = c("test1", "test3")))

## Setting the column name to 'finalavg'.
colnames(allwiz_sub)[3] <- "finalavg"; colnames

## Printing 'allwiz_sub' to demonstrate the effect.
allwiz_sub

## Factors, Data Frames, & Lists
## 1) Categorical Variables: Characters & Factors
## Creating an object called 'truth'.
truth <- "Dartmouth is, like, way better than all the other schools. Like, it's not even close, you guys."; truth

## Checking the class of 'truth' and printing/reporting it.
class(truth) # Alternatively: print(class(truth))

## Creating a vector called 'colors'. The values are in quotations inside the vector.
colors <- c("red", "blue", "green", "red", "blue"); colors

## Creating a factor vector called 'factor_colors' from the 'colors' vector.
factor_colors <- factor(colors, levels = c("red", "blue", "green"), ordered = FALSE); factor_colors

## Analysis of the 'factor_colors' factor vector.
table(factor_colors)
levels(factor_colors)

## Creating a factor vector called 'factor2_colors' from the 'colors' vector.
## This time, the argument 'levels' is set to just 'red' and 'blue', which means
## the result is different from the previous factor vector.
factor2_colors <- factor(colors, levels = c("red", "blue")); factor2_colors

## Analysis of the 'factor2_colors' factor vector.
table(factor2_colors)
levels(factor2_colors)

## What does this tell you about factors?
   ## When using factor vectors, the categorical variables that are not specified
   ## by a certain 'level' are considered <NA> in the factor vector. This means
   ## that when printing the table for the factor vector, they are not included
   ## in the counts that are displayed.

## You are going to need to know about the properties of factors in this class
## and pretty much all your future research!

## Reporting the summaries of 'colors', 'factor_colors', and 'factor2_colors'.
summary(colors)
summary(factor_colors)
summary(factor2_colors)

## There are evident differences between the summaries of characters and factors.
## Moreover, there are differences between two factors of the same variable, but
## with different levels.

## Creating a vector called 'ideology' that takes various values.
ideology <- c("liberal", "conservative", "very liberal", "very conservative",
              "middle of the road", "slightly conservative", "slightly liberal",
              "liberal", "conservative", "middle of the road"); ideology

## Creating a factor vector from the 'ideology' vector. The levels are ordered
## appropriately, from 'very liberal' to 'very conservative'.
fact_ideo <- factor(ideology, levels = c("very liberal", "liberal", "slightly liberal",
                    "middle of the road", "slightly conservative", "conservative",
                    "very conservative"), ordered = TRUE); fact_ideo

## Analysis of the 'fact_ideo' factor vector.
table(fact_ideo)
levels(fact_ideo)

## Creating a character vector called 'respondents' that takes various values.
respondents <- c("Susie", "Abdul", "Maria", "Fred", "Wilma", "Barney", "Dino",
                 "Ajax", "Thor", "Betty"); respondents

## Analysis of the 'respondents' vector.
length(respondents)
class(respondents)

## 2) Data Frames
## Creating a vector called 'income' that takes various values.
## Remember not to add commas inside the numerical values, R will incorrectly read
## the values if you do.
income <- c(100000, 75000, 48000, 62000, 31000, 52500, 274000, 88000, 21000, 74000); income

## Creating a data frame from the 'respondents', 'ideology', and 'income' vectors.
data1 <- data.frame(respondents, ideology, income); data1

## Showing the head, tail, and structure of the data frame called 'data1'.
head(data1)
tail(data1)
str(data1)

## The names() command extracts the broad structure of the data frame.
names(data1)

## Ordering the observations in the data set by income in descending order, and
## making a new copy of the data calling it 'orderdat'.
orderdat <- data1[order(data1$income, decreasing = TRUE), ]; orderdat

## Printing 'data1' to demonstrate that the original data has not changed.
data1

## Extracting from 'data1' the respondent with the lowest income, sub-setting/isolating
## the entire row, and printing it.
data1[which.min(data1$income), ]

## Selecting the names of all the respondents starting with 'Fred' (index 4) and
## ending with 'Ajax' (index 8) from 'data1'. There are a number of ways to do this.
## Sub-setting the respondents in the order they sit in the data frame.
data1$respondents[4:8]

## Adding a new variable to 'data1' called 'log_income' that takes the natural
## log of the 'income' variable. This is used to identify the outliers in a sample.
data1$log_income <- log(data1$income)

## Alternatively: data1 <- within(data1, log_income <- log(income))
## Alternatively: data1$log_income <- with(data1, log(income))

## Printing 'data1' to demonstrate the effect.
data1

## Further Highlights
data1[1] # Extracts the first column from the data frame.
data1[1, ] # Extracts the first row from the data frame.

## 3) Lists
## Creating a list object called 'survey' from the 'ideology', 'respondents',
## and 'income' variables. The structure differs significantly from a data frame.
survey <- list(ideology, respondents, income); survey

## Creating an object called 'session' that takes the value 2 (just a single number).
session <- 2; session

## Creating an object called 'weeks' that is a 3x3 matrix of numbers 1 through 9,
## inputted by row.
weeks <- matrix(c(1:9), nrow = 3, ncol = 3, byrow = TRUE); weeks

## Creating a list called dv_list that takes 'truth', 'session', 'weeks', 'data1',
## and 'survey' (a list object).
dv_list <- list(truth, session, weeks, data1, survey); dv_list

## Renaming the objects to 'truth', 'sess1', 'wk', 'dat', and 'svy' inside the list.
names(dv_list) <- c("truth", "sess1", "wk", "dat", "svy"); names(dv_list)

## Printing 'dv_list' to demonstrate the effect.
dv_list

## Extracting the middle element from the 'weeks' matrix from 'dv_list'.
## The code to execute this starts with the list itself.
dv_list$wk[2, 2] # Alternatively: dv_list[["wk"]][2, 2]

## Changing the 'income' variable in 'data1' from 'dv_list', by dividing 'income' by 2.
## The code to execute this starts with the list itself.
dv_list$dat["income"] <- dv_list$dat["income"] / 2

## Alternatively: dv_list[['dat']]['income'] <- dv_list[['dat']]['income'] * 2

## Printing 'dv_list' to demonstrate the effect.
dv_list
