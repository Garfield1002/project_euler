data WeekDay =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data DateTime =
 DateTime {
    year    :: Int,
    month   :: Int,
    day     :: Int,
    weekday :: WeekDay
} deriving (Eq, Show, Read)

weekdayNumber :: WeekDay -> Int
weekdayNumber wd = fromEnum wd + 1

isLeapYear :: Int -> Bool
isLeapYear y = (mod y 4 == 0) && (mod y 100 /= 0|| mod y 400 == 0)

nextWeekDay :: WeekDay -> WeekDay
nextWeekDay weekday = toEnum $ mod (fromEnum weekday + 1) 7

next :: DateTime -> DateTime
next (DateTime year month day weekday)
    | day == 31 && elem month [1, 3, 5, 7, 8, 10] = DateTime year (month + 1) 1 weekday
    | day == 30 && elem month [4, 6, 9, 11]       = DateTime year (month + 1) 1 weekday
    | day == 31 && month == 12                    = DateTime (year + 1) 1 1 weekday
    | day == 28 && month == 2 && not (isLeapYear year) = DateTime year 3 1 weekday
    | day == 29 && month == 2 && isLeapYear year  = DateTime year 3 1 weekday
    | otherwise = DateTime year month (day + 1) (nextWeekDay weekday)


today = helper (DateTime 1900 1 1 Monday)
    where helper (DateTime year month day weekday)
            | year == 1901 && month == 1 && day == 1    = DateTime year month day weekday
            | otherwise                                 = helper $ next (DateTime year month day weekday)

euler19 = helper' (DateTime 1901 1 1 Thursday) 0
    where helper' (DateTime year month day weekday) i
            | year == 2000 && month == 12 && day == 31 = i
            | weekday == Sunday && day == 1            = helper' (next (DateTime year month day weekday)) (i + 1)
            | otherwise                                = helper' (next (DateTime year month day weekday)) i