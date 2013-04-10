module ReservationUtils where

increaseMonth (12,year)    = (1, year+1)
increaseMonth (month,year) = (month+1, year)

decreaseMonth (1,year)     = (12, year-1)
decreaseMonth (month,year) = (month-1, year)

