import Data.List

select :: Ord a => [a] -> [a]
select = take 2 . sort
