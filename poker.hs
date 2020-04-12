module Poker where
    import Data.List

    deal :: [Int] -> [[Char]]
    deal cards = do

        let hand1_indexed = [snd card | card <- (zip [0..] cards), even (fst card)]
        let hand2_indexed = [snd card | card <- (zip [0..] cards), odd(fst card)]

        let hand1 = map convert hand1_indexed
        let hand2 = map convert hand2_indexed

        winner_is (sort hand1) (sort hand2)

    convert :: Int -> [Char]
    convert card_int = do

        let suits = ["C", "D", "H", "S"]
        let cards = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"]

        let card_number = cards !! rem (card_int-1) (length cards)
        let card_suit = suits !! div (card_int-1) (length cards)

        card_number ++ card_suit

    evaluate :: [[Char]] -> Int
    evaluate hand = do
        has_pair hand + has_two_pairs hand + has_three hand + has_straight hand + has_flush hand + full_house hand + has_four hand + straight_flush hand + royal hand

    has_pair :: [[Char]] -> Int
    has_pair hand = do
        let new_hand = as_numbers hand
        if 2 `elem` (frequencies new_hand) then 1
        else 0
    has_two_pairs :: [[Char]] -> Int
    has_two_pairs hand = do
        let new_hand = as_numbers hand
        if 2 `elem` (frequencies (drop 1 (frequencies new_hand))) then 2
        else 0

    has_three :: [[Char]] -> Int
    has_three hand = do 
        let new_hand = as_numbers hand 
        if 3 `elem` (frequencies new_hand) then 3
        else 0

    has_straight :: [[Char]] -> Int
    has_straight hand = do
        let new_hand = as_numbers hand
        if (is_sequence (sort new_hand)) then 4
        else 0

    has_flush :: [[Char]] -> Int
    has_flush hand = do 
        if any (==5) (frequencies (map last hand)) then 5
        else 0

    full_house :: [[Char]] -> Int
    full_house hand = do
        if (has_pair hand > 0 && has_three hand > 0)  then 6
        else 0

    has_four :: [[Char]] -> Int
    has_four hand = do
        let new_hand = as_numbers hand
        if 4 `elem` (frequencies new_hand) then 11
        else 0

    straight_flush :: [[Char]] -> Int
    straight_flush hand = do
        if (has_straight hand > 0 && has_flush hand > 0) then 12
        else 0

    royal :: [[Char]] -> Int
    royal hand = do
        let royal_hands = [["1C", "10C", "11C", "12C", "13C"], ["1D", "10D", "11D", "12D", "13D"],
                           ["1H", "10H", "11H", "12H", "13H"], ["1S", "10S", "11S", "12S", "13S"]]

        if hand `elem` royal_hands then 13
        else 0

    winner_is :: [[Char]] -> [[Char]] -> [[Char]]
    winner_is hand1 hand2 = do
        let score1 = evaluate hand1
        let score2 = evaluate hand2

        let hand1_num = as_numbers hand1
        let hand2_num = as_numbers hand2

        let hand1_power = get_hand_power hand1_num
        let hand2_power = get_hand_power hand2_num

        if (score1 > score2) then hand1 
        else if (score2 > score1) then hand2 
        else if (tiebreak hand1_power hand2_power) == "hand1" then hand1
        else if (tiebreak hand1_power hand2_power) == "hand2" then hand2
        else (tiebreak_by_suit hand1 hand2)

    frequencies_helper :: Ord a => [a] -> [([a], Int)]
    frequencies_helper hand = map (\elem -> ([head elem], length elem)) . group . sort $ hand

    frequencies :: Ord a => [a] -> [Int]
    frequencies hand = [ snd elem | elem <- frequencies_helper hand]

    as_numbers :: [[Char]] -> [Int]
    as_numbers hand = [read (init card)::Int | card <- hand]

    is_sequence :: [Int] -> Bool
    is_sequence (_:[]) = True
    is_sequence (hd:tl) = do
        if (head tl - hd == 1) then (is_sequence tl)
        else if (hd == 1) then (card_sequence tl)
        else False

    card_sequence :: [Int] -> Bool
    card_sequence tail = do
        if (drop 3 tail == [13] && is_sequence tail) then True
        else False

    tiebreak :: [Int] -> [Int] -> [Char]
    tiebreak [] [] = "Not possible"
    tiebreak hand1_power hand2_power = do
        let max_value1 = maximum' hand1_power
        let max_value2 = maximum' hand2_power

        if (max_value1 > max_value2) then "hand1"
        else if (max_value2 > max_value1) then "hand2"
        else tiebreak (tail hand1_power) (tail hand2_power)

    get_hand_power :: [Int] -> [Int]
    get_hand_power hand = do
        let magnitude = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1] 
        let hand_power = [strength | (strength, card) <- (zip [0..] magnitude), card `elem` hand]

        if (hand !! 0 == 1 && is_sequence (tail hand)) then 0:(init hand_power)
        else hand_power
    
    tiebreak_by_suit :: [[Char]] -> [[Char]] -> [[Char]]
    tiebreak_by_suit hand1 hand2 = do

        if last hand1 > last hand2 then hand1
        else hand2
        
    maximum' :: Ord a => [a] -> a  
    maximum' [] = error "maximum of empty list"  
    maximum' [x] = x  
    maximum' (x:xs) = max x (maximum' xs) 
