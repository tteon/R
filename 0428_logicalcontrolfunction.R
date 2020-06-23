x <- pi
y <- 3
if (x > y) x


# if , else command

x <- pi
y <- 1:5
if (x < y) x else y
if (x > y) x else y

test <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
yes <- 1:5
no <- 0
ifelse(test, yes, no)

ifelse(x > y, x , y)

center <- function(x, type) {
  switch(type,
         mean=mean(x),
         median=median(x),
         trimmed=mean(x, trim=0.1))
}

# create the function 

x <- c(2,3,5,7,11,13,17,19,23,29)
center(x, 'mean')
center(x, 'median')
center(x, 'trimmed')

center <- function(x, type) {
  switch(type,
         mean=mean(x),
         median=median(x),
         trimmed=mean(x, trim=0.1),
         'Choose one of mean, median, and trimmed')
}
center(x, 'other')


# repeat function

i <- 5
repeat {if(i > 25) break
  else {
    print(i)
    i <- i + 5}
}
# for function

for (var in list) statement

for (i in seq(from=5, to=25, by=5)) print(i)

transLength <- function(x) {
  tlength <- round(x*0.9144, digits=1)
  result <- paste(tlength, 'm', sep='')
  return(result)
}
ls()

y <- c(100, 150, 200)
transLength(y)

trans2 <- transLength
trans2
trans2(y)


transLength <- function(x) {
  tlength <- round(x*0.9144, digits=1)
  result <- paste(tlength, 'm', sep='')
}

transLength(y)
print(transLength(y))

transLength <- function(x) {
  if(!is.numeric(x)) return('Not a Number')
  tlength <- round(x*0.9144, digits=1)
  paste(tlength, 'm', sep='')
}

transLength <- function(x) {
  if(!is.numeric(x)) return('Not a Number')
  tlength <- round(x*0.9144, digits = 1)
  paste(tlength, 'm', sep='')
}

ls()
rm(list=ls())

transLength('ABC')

f1 <- function(x, y) {x + y}
f2 <- function(x, y) x+y
f1(1, 3)
f2(1, 3)
y <- c(100, 200, 300)

transLength <- function(x, mult, unit) {
  tlength <- round(x*mult, digits=1)
  paste(tlength, unit, sep='')
}

transLength(y, mult=3, unit='ft')
transLength(y, mult=36, unit='in')

transLength(y)



transLength <- function(x, mult=0.9144, unit='m') {
  tlength <- round(x*mult, digits=1)
  paste(tlength, unit, sep='')
}

transLength(y)
transLength(y, mult=3, unit='ft')
transLength(y, 3, 'ft')

transLength <- function(x, mult=0.9144, unit='m', ...) {
  tlength <- round(x*mult, digits=...)
  paste(tlength, unit, sep='')
}

transLength(y, digits=2)

transLength(y)

# designate default variabels
transLength <- function(x, mult=0.9144, unit='m', digits=1) {
  tlength <- round(x*mult, digits=digits)
  paste(tlength, unit, sep='')
}


# initialize the function using 'FUN'
transLength <- function(x, mult=0.9144, unit='m', FUN=round , ...) {
  tlength <- FUN(x*mult, ...)
  paste(tlength, unit, sep='')
}

transLength(y, FUN=signif, digits=3)
transLength(y, FUN=floor)
transLength(y)

ls()

# local , global environment 
x <- 11:15
scopetest <- function(x) {
  cat('This is x: ', x, '\n')
  rm(x)
  cat('This is x after removing x', x, '\n')
}
scopetest(x=15:11)













