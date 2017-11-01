# bizhours package
An R Package to calculate time difference between two date time vectors in terms of business hours

# Description
bizhours provides a single function bizhours(...) that calculates the difference between two vectors of POSIXct date-time values, that represents the date time from when to calculate the difference and the corresponding date-time values till when to calculate. 
bizhours uses package bizdays for it's calculation of business days (https://cran.r-project.org/web/packages/bizdays/bizdays.pdf) and for the calendars used.

# Pre-requisites
This package requires package bizdays.
Install bizdays using the following code:
```
install.packages('bizdays')
```

# Installing
Install bizhours package through:
```
devtools::install_github("sujeetp97/bizhours")
```