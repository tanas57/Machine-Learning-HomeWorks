name <- "John"
measure <- 5.7
fault <- TRUE

name_vector <- c("John", "Asli", "Can", "Berk", "Cansu")
num_vector <- c(3, -2, 4, -1, 5)
bool_vector <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
rand_num <- seq(from = 3, to = 100, length = 30)

name_vector[2]
name_vector[3]
num_vector[(num_vector > 3)]
name_vector[(bool_vector == TRUE)]
sum(num_vector)
pos_num <- num_vector[(num_vector > 0)]
pos_num

person_list <- list("John", 27, "Computer Engineer")
names(person_list) <- c("name", "age", "occupation")
person_list <- c(person_list, "salary" = 4000)
person_list$name
person_list$salary

weather_matrix <- matrix(seq(from = 5, to = 30, length = 15), byrow = TRUE, nrow = 5, ncol = 3) 
rownames(weather_matrix) <- c("day1", "day2", "day3", "day4", "day5")
colnames(weather_matrix) <- c("s1", "s2", "s3")

which.max(rowSums(weather_matrix))

subB <- c(weather_matrix[4,2], weather_matrix[4,3], weather_matrix[5,2], weather_matrix[5,3])
subB

cars <- data("mtcars")

head(mtcars)
length(mtcars)

smallc <- mtcars[mtcars$cyl<=6,]
mean(smallc$hp)

rownames(smallc[smallc$gear==5,])

inp_vec <- c(5, 2, 7, 6, 3, 19, 23, 78, 145, 3, 4, 6, 9, 12, 67)
for (variable in inp_vec) {
    if(variable %% 2 == 0) print("The number even")
    else print("The number odd")
}

name <- c("Ali","Cenk","Mete")
age <- c(26,32,29)
salary <- c(2700, 3200, 4900)
company <- data.frame(c(name), c(age), c(salary))

max = 0
for (row in 1:nrow(company)) {

    tempSalary = company[row, "c.salary."]
    if(tempSalary > max) max = tempSalary
}
max

company[company$c.salary.==max,1]


