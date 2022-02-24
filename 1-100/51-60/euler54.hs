import Data.List ( groupBy, sortOn, sort, sortBy )
import Data.Function ( on )
import Control.Arrow ( Arrow((***)) )
import Data.Char (isDigit)
import Text.Read ( readMaybe )
import Maybes (catMaybes)
import Data.Ord (Down(Down))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)

data Suit = Diamond | Club | Spade | Heart deriving Eq

-- Cards values go from 2 to 14
data Card = Card Int Suit deriving Eq

valueFrom :: Char -> Maybe Int
valueFrom 'A' = Just 14
valueFrom 'K' = Just 13
valueFrom 'Q' = Just 12
valueFrom 'J' = Just 11
valueFrom 'T' = Just 10
valueFrom  c  = readMaybe [c]

suitFrom :: Char -> Maybe Suit
suitFrom 'S' = Just Spade
suitFrom 'H' = Just Heart
suitFrom 'C' = Just Club
suitFrom 'D' = Just Diamond
suitFrom  _  = Nothing

readCard :: String -> Maybe Card
readCard [v, s] = do
  v <- valueFrom v
  s <- suitFrom s
  pure $ Card v s
readCard _ = Nothing

suit :: Card -> Suit
suit (Card _ s) = s

value :: Card -> Int
value (Card v _) = v

data Combination =  High Int        |
                    Pair Int        |
                    Double Int Int  |  -- Highest pair first
                    Three Int       |
                    Straight Int    |  -- The lowest card
                    Flush [Int]     |
                    Four Int        |
                    StraightFlush Int  -- The lowest card
                    deriving(Eq, Ord)

dupe :: a -> (a, a)
dupe x = (x, x)

mapAdjacent :: (a1 -> a1 -> a2) -> [a1] -> [a2]
mapAdjacent f (h:ht:t) = f h ht : mapAdjacent f (ht:t)
mapAdjacent _ _ = []

isFlush :: [Card] -> Bool
isFlush = (5 ==) . maximum . map length . groupBy ((==) `on` suit)

isStraight :: [Card] -> Bool
isStraight = all (1 ==) . take 4 . mapAdjacent ((-) `on` value)

gradeHighs :: [(Int, Int)] -> [Combination]
gradeHighs [] = []
gradeHighs ((4, v):t) = Four v : gradeHighs t
gradeHighs ((3, v):t) = Three v : gradeHighs t
gradeHighs ((2, v):(2, v'):t)   = Double v v' : gradeHighs t
gradeHighs ((2, v):t) = Pair v : gradeHighs t
gradeHighs ((_, v):t) = High v : gradeHighs t

grade :: [Card] -> [Combination]
grade  [] = []
grade cards
    | isFlush cards = (:[]) $
        if straight
        then StraightFlush . value . head $ cards
        else Flush . map value $ cards
    | straight = (:[]) $ Straight . value . head $ cards
    | otherwise = gradeHighs oCards
    where
        straight = isStraight cards
        oCards = sortOn Down . map ((length *** (value . head)) . dupe) $ groupBy ((==) `on` value) cards


winner :: [Char] -> Bool
winner s =
    let cardStrings = words s in
    let [p1Strings, p2Strings]  = [take 5, drop 5] <*> [cardStrings] in
    let [p1MCards, p2MCards]    = map readCard <$> [p1Strings, p2Strings] in
    let [p1Cards, p2Cards]      = catMaybes <$> [p1MCards, p2MCards] in
    let [p1SCards, p2SCards]    = sortOn (Down . value) <$> [p1Cards, p2Cards] in
    let [p1Grade, p2Grade]      = grade <$> [p1SCards, p2SCards] in
    p1Grade > p2Grade

main :: IO ()
main = do
    hands <- map B.unpack . B.lines <$> B.readFile "euler54.txt"
    print . sum $ [1 | hand <- hands, winner hand]
