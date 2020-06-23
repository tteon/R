Sys.Date()
date()
Sys.time()

class(Sys.Date())

as.Date('2025-12-31')
as.Date('2025/12/31')

?strptime

d <- as.Date('2025-12-31')
d
format(d, format='%m/%d/%Y')

today <- Sys.Date()
today
format(today, format='%Y/%m/%d')
# print ~day
format(today, format='%Y/%m/%d %A')
format(today, format='%Y/%m/%d %a')

d <- as.Date('2025-12-31')
d
weekdays (d)

d
d + 7
d + 1:7

weekdays(d + 1:7)

start <- as.Date('2025-01-01')
end <- as.Date('2025-01-31')
seq(from=start, to=end, by=1)

seq(from=start, by=1, length.out=7)

seq(from=start, by="7 days", length.out=7)
seq(from=start, by="week", length.out=7)
seq(from=start, by="month", length.out=12)
seq(from=start, by="3 months", length.out=4)
seq(from=start, by="year", length.out=10)

seq(from=as.Date('2025-01-30'), by='month', length.out=6)

start <- as.Date('2025-01-01')
qrt <- seq(from=start, by="3 months", length.out=4)

months(qrt)
quarters(qrt)

# check your local time and setting options
Sys.getlocale()
Sys.setlocale('LC_TIME', 'C')
months(qrt)

Sys.setlocale()
Sys.getlocale('LC_TIME', 'Korean_Korea.949')

#
pct = as.POSIXct('2025/03/15, 15:03:04', format='%Y/%m/%d, %H:%M:%S')
pct
as.integer(pct)

plt = as.POSIXlt('2025/03/15, 15:03:04', format='%Y/%m/%d, %H:%M:%S')
plt
class(plt)

unclass(plt)

plt$mday
plt$mon
plt$year
plt$wday
plt$hour

dposix <- as.Date('2025-12-31')
dposix
as.POSIXlt(dposix)$wday
as.POSIXlt(dposix)$yday
as.POSIXlt(dposix)$year + 1900
as.POSIXlt(dposix)$mon + 1

strptime('2025-12-31', format='%Y-%m-%d')
class(strptime('2025-12-31', format='%Y-%m-%d'))
strptime('2025-12-31', format='%Y-%m-%d')$year + 1900

moon <- as.POSIXct('1969/07/20, 20:17:39', format='%Y/%m/%d, %H:%M:%S', tz='UTC')

moon
format(moon, 'The time of the Apollo moon landing was format=%Y/%m/%d, at %H:%M:%S')


# merge date objective
y <- 2020
m <- 12 
d <- 31
ISOdate(y, m, d)
class(ISOdate(y, m, d))
as.Date(ISOdate(y, m, d))

years <- c(2025, 2026, 2027, 2028)
months <- c(1, 4, 7, 10)
days <- c(12, 19, 25, 17)
ISOdate(years, months, days)

jdate <- as.Date('2025-12-31')
jdate
as.integer(jdate)
julian(jdate)

as.integer(as.Date('1970-01-01'))
as.integer(as.Date('1970-01-02'))
as.integer(as.Date('1969-12-31'))

as.Date(as.integer(jdate), origin='1970-01-01')

moon
class(moon)
moon + 60*60*2

moon + 60*60*24*7

as.Date(moon) + 7
start <- as.Date('1988-09-17')
end <- as.Date('2018-02-09')
start
end
end-start

today <- Sys.date
Dooly <- as.Date('1983-04-22')
difftime(today , Dooly , units = 'days')
difftime(today , Dooly , units = 'weeks')

moon
today <- Sys.Date
Sys.time() > moon
Sys.Date() > as.Date(moon)













































