
x <- 'We have a dream'
x
nchar(x)
length(x)

y <- c('we', 'have', 'a', 'dream')
y
nchar(y)
length(y)
nchar(y[4])

letters
sort(letters, decreasing = T)

toupper()
tolower()

# strsplit

p1 <- 'You come at four in the afternoon, then at three I shall begin to be happy'
p2 <- 'One runs the risk of weeping a little, if one lets himself be tamed'
p3 <- 'What makes the desert beautiful is that somewhere it hides a well'
littleprice <- c(p1, p2, p3)
littleprice
strsplit(littleprice, ' ')
strsplit(littleprice, ' ')[[3]]
strsplit(littleprice, ' ')[[3]][5]

fox.said <- 'WHAT IS ESSENTIAL is invisible to the Eye'
fox.said.words <- strsplit(fox.said, ' ')[[1]]
fox.said.words
unique(fox.said.words)
unique(tolower(fox.said.words))

paste('Everybody', 'wants', 'to', 'fly')
paste(c('Everybody', 'wants', 'to', 'fly'))
# wrong case
fox.said.words
paste(fox.said.words)

paste('Everybody', 'wants', 'to', 'fly' , sep='-')
paste('Everybody', 'wants', 'to', 'fly' , sep=' ')
# non blank function
paste0('Everybody', 'wants', 'to', 'fly')

heroes <- c('Batman', 'Captain America', 'Hulk')
colors <- c('Balck', 'Blue', 'Green')
paste(heroes, colors)

paste('Type', 1:5)
paste(heroes, 'wants', 'to', 'fly')
# collapse argument 

paste(c('Everybody', 'wants', 'to', 'fly'))
paste(c('Everybody', 'wants', 'to', 'fly'),collapse='')

paste(heroes, 'wants', 'to', 'fly')
paste(heroes, 'wants', 'to', 'fly', collapse=', and')

paste(heroes, 'wants', 'to', 'fly', sep='-')
paste(heroes, 'wants', 'to', 'fly', sep='-', collapse=';')

paste(month.abb, 1:12)
paste(month.abb, 1:12, sep='_', collapse='-')

# outer function is outprint to matrix format 
outer(c(1,2,3), c(1,2,3))

asian.countries <- c('Korea', 'Japan', 'China')
info <- c('GDP', 'Population', 'Area')
out <- outer(asian.countries, info, FUN=paste , sep='-')
as.vector(out)

outer(asian.countries, asian.countries, FUN=paste, sep='-')
x <- outer(asian.countries, asian.countries, FUN=paste, sep='-')
x[!lower.tri(x)]

customer <- 'Jobs'
buysize <- 10
deliveryday <- 3
paste('Hello ' , customer, ', your order of ', buysize, ' product(s) will be dilivered within ' , deliveryday , ' ; day(s).', sep=' ')
sprintf('Hello %s  your order of %s product(s) will be dilivered within %s ; day(s).', customer, buysize, deliveryday)


customer <- c('Jobs' , 'Gates', 'Bezos')
buysize <- c(10 , 7 , 12)
deliveryday <- c(3 , 2, 7.5)
sprintf('Hello %s  your order of %s product(s) will be dilivered within %s ; day(s).', customer, buysize, deliveryday)
?sprintf

sprintf('Hello %s  your order of %s product(s) will be dilivered within %.1f ; day(s).', customer, buysize, deliveryday)

substr('Data Analytics', start=1, stop=4)
substr('Data Analytics', start=6, stop=14)
substring('Data Analytics', 6)

class <- c('Data Analytics', 'Data Mining', 'Data visualization')
substr(class, 1, 4)

# sense 
countries <- c('Korea, KR', 'Unites States, US', 'China, CN')
substr(countries, nchar(countries)-1 , nchar(countries))

head(islands)
landmasses <- names(islands)
landmasses

grep(pattern='New', x=landmasses)

index <- grep(pattern='New', x=landmasses)
landmasses[index]

# immediately print including value
grep(pattern='New', x=landmasses, value=T)

landmasses[grep(' ', landmasses)]
grep(' ', landmasses, value=T)

txt <- 'Data Analytics is useful. Data Analytics is also interesting.'
sub(pattern='Data', replacement='Business', x=txt)
gsub(pattern='Data', replacement='Business', x=txt)


x <- c('product.csv', 'customer.csv', 'supplier.csv')
sub(pattern='.csv', replacement='', x=x)

words <- c('at', 'bat', 'cat', 'chaenomeles', 'chase', 'chasse', 'cheap', 'check', 'cheese', 'chick', 'hat')
grep('che', words)
grep('che', words, value=T)
grep('at', words, value=T)

grep('[at]', words, value=T)
grep('[ch]', words, value=T)
grep('ch|at', words, value=T)
grep('ch(e|i)ck', words, value=T)

grep('chas?e', words, value=T)
grep('chas*e', words, value=T)
grep('chas+e', words, value=T)

grep('ch(a*|s*)se', words, value=T)
grep('^c', words, value=T)
grep('t$', words, value=T)

grep('^c.*t$', words, value=T)
grep('^[hc]?at', words, value=T)

words2 <- c('12 Dec', 'OK', 'http://', '<TITLE>Time?</TITLE>', '12345', 'Hi there')

# grep useful application
grep("[[:alnum:]]", words2, value=T)
grep("[[:alpha:]]", words2, value=T)
grep("[[:digit:]]", words2, value=T)
grep("[[:punct:]]", words2, value=T)
grep("[[:space:]]", words2, value=T)

grep('\\w+' , words2, value=T)
grep('\\d+' , words2, value=T)
grep('\\s+' , words2, value=T)

?regex

































