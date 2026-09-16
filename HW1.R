#Problem 1
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

#Part A
# Convert section to a factor with levels A, B
section <- factor(section, levels = c("A", "B"))

# Create the data frame
students <- data.frame(
  student_id,
  section,
  quiz1,
  quiz2,
  passed
)

# Create the matrix
scores <- cbind(quiz1, quiz2)
rownames(scores) <- student_id
colnames(scores) <- c("quiz1", "quiz2")

# Create the course record list
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)


#Part B
# S04's second quiz score
scores["S04", "quiz2"]

# First two rows, preserving matrix dimensions
scores[1:2, , drop = FALSE]

# Extract course
course_record["course"]
course_record[["course"]]
course_record$course

#Part B text answer: single bracket selects a part of an object and returns the object(s), double bracket returns a vector by extracting the element, and $ is the same as double bracket but instead takes in a name 


#Part C
students$average <- rowMeans(students[, c("quiz1", "quiz2")])

students$excellent <- students$average >= 90

section_A <- students[
  students$section == "A" & students$average >= 80,
  c("student_id", "section", "average")
]

student_averages <- setNames(students$average, students$student_id)

#Part C text answer: The answer is vectorized because rowMeans calculated using vectors

#problem 2
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

#Part A
measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

# Count missing values in each column
colSums(is.na(measurements))

# Complete cases
measurements_complete <- measurements[complete.cases(measurements), ]

# Sample IDs removed
measurements$sample_id[!complete.cases(measurements)]
#Sample IDs removed: "M02" "M03" "M06"

#Part A text answer: x == NA is not a valid missing-value test because NA represents a missing value, so the result of the comparison is also NA.

#Part B
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- ifelse(is.na(measurements$ph), NA, measurements$ph < 7)

filtered <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
  c("sample_id", "site", "temp_c", "temp_f", "ph")
]

filtered

mean(measurements$temp_c, na.rm = TRUE)
#20.35714
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)
#20.46667

#Part C
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

A * B
A %*% B

dim(A * B)
dim(A %*% B)
#Outputs: Both [1] 2 2


#Part C written part: A * B multiplies corresponding elements. A %*% B multiplies rows of A and columns of B.


#Problem 3
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

#Part A
grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)
#Outputs: NA, A, B, B, C

#Part B
grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id

grades
#Output:  P01 P02 P03 P04 P05 P06 P07 P08 
#         "A" "B"  NA "D" "C" "B" "F" "A" 

#Part B written part: i represents the position of the current score in the scores vector.

#Part C-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(
    total = length(x),
    missing = sum(is.na(x)),
    mean = round(mean(x, na.rm = na.rm), digits),
    sd = round(sd(x, na.rm = na.rm), digits),
    minimum = round(min(x, na.rm = na.rm), digits),
    maximum = round(max(x, na.rm = na.rm), digits)
  )
}

summarize_scores(scores)

summarize_scores(x = scores, na.rm = TRUE, digits = 2)
#Output:  total  missing    mean      sd    minimum  maximum 
#         8.00     1.00    79.43    13.72    59.00    95.00

#Part C-2
plot_scores <- function(x, ...) {
  plot(x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores")
