library(quantmod)
from.dat <- as.Date("01/01/08",format= "%m/%d/%y")
to.dat <- as.Date("12/31/15",format= "%m/%d/%y")
getSymbols("GOOG",src="google", from=from.dat, to= to.dat)

head(GOOG)
tail(GOOG)

myGoog <- GOOG[,-5]
head(myGoog)

mGoog <- to.monthly(myGoog)
head(mGoog)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency = 12)
plot(ts1, ylab="Google")
head(ts1)

plot(decompose(ts1))

train <- window(ts1, start=1, end=5)
head(train, 20)
head(ts1, 20)
str(ts1)
class(ts1)
print(ts1)

library(forecast)

