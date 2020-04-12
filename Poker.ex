defmodule Poker do

  # Deal two hands alternatively.
  def deal(cards) do

    hand1_numbered = Enum.take_every(cards, 2) # Hand1 without converting (numbers from 1-52).
    hand2_numbered = Enum.drop_every(cards, 2) # Hand2 without converting (numbers from 1-52).

    # Converts and sorts both hands.
    hand1 = Enum.map(hand1_numbered, fn card -> convert(card) end) |> card_sort
    hand2 = Enum.map(hand2_numbered, fn card -> convert(card) end) |> card_sort

    # Return winner.
    winner_is(hand1, hand2)

  end

  # Maps integers from 1-52 into deck cards.
  def convert(card_int) do

    suits = ["C", "D", "H", "S"]
    cards = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]

    card_number = Enum.at(cards, rem(card_int-1, length cards))
    card_suit = Enum.at(suits, div(card_int-1, length cards))

    to_string(card_number) <> card_suit
  end

  # Evaluate a given hand.
  def evaluate(hand) do
    has_pair(hand) + has_two_pairs(hand) + has_three(hand) + has_straight(hand) + has_flush(hand) +
    full_house(hand) + has_four(hand) + straight_flush(hand) + royal(hand)

  end

  # Check if the hand has a pair.
  def has_pair(hand) do
    new_hand = as_numbers(hand)
    Enum.any?(frequencies(new_hand), fn count -> count == 2 end) &&
    1 || 0
  end

  # Check if the hand has twp pairs.
  def has_two_pairs(hand) do
    new_hand = as_numbers(hand)
    Enum.count(frequencies(new_hand), fn count -> count == 2 end) == 2 &&
    2 || 0
  end

  # Check if the hand has three of a kind.
  def has_three(hand) do
    new_hand = as_numbers(hand)
    Enum.any?(frequencies(new_hand), fn count -> count == 3 end) &&
    3 || 0
  end

  # Check if the hand has a straight.
  def has_straight(hand) do
    new_hand = as_numbers(hand)
    sequence?(new_hand) &&
    4 || 0
  end

  # Check if the hand has a flush.
  def has_flush(hand) do
    Enum.any?(frequencies(get_suits(hand)), fn count -> count == 5 end) &&
    5 || 0
  end

  # Check if the hand has a full house.
  def full_house(hand) do
    has_pair(hand) > 0 && has_three(hand) > 0 &&
    6 || 0
  end

  # Check if the hand has four of a kind.
  def has_four(hand) do
    new_hand = as_numbers(hand)
    Enum.any?(frequencies(new_hand), fn count -> count == 4 end) &&
    7 || 0
  end

  # Check if the hand has a straight flush.
  def straight_flush(hand) do
    has_straight(hand) > 0 && has_flush(hand) > 0 &&
    8 || 0
  end

  # Check if the hand has a royal straight flush.
  def royal(hand) do
    royal_hands = [["1C", "10C", "11C", "12C", "13C"], ["1D", "10D", "11D", "12D", "13D"],
    ["1H", "10H", "11H", "12H", "13H"], ["1S", "10S", "11S", "12S", "13S"]]

    Enum.member?(royal_hands, hand) &&
    9 || 0
  end

  # Sorts the hand in ascending order and by the suit power.
  def card_sort(hand) do
    Enum.sort(hand) |> Enum.sort_by(&byte_size(&1))
  end

  # Shows the frequencies of a given value on a hand.
  def frequencies(hand) do
    Map.values(Enum.frequencies(hand))
  end

  # Shows the hand without the suits.
  def as_numbers(hand) do
    suits = ["C", "D", "H", "S"]
    Enum.map(hand, fn number -> String.to_integer(List.first(String.split(number, suits))) end)
  end

  # Shows only the suits of each card.
  def get_suits(hand) do
    Enum.map(hand, fn suit -> String.last(suit) end)
  end

  # Helper method to check if the difference of two given elements is one.
  def diff(first, second) do
    second-first == 1
  end

  # Base case to the sequence? method
  def sequence?([_ | []]) do
    true
  end

  # Recursive implementation. Used to check if there is a sequence on a given hand.
  def sequence?([head | tail]) do
    head == 1 && card_sequence?(tail) || diff(head, hd tail) && sequence?(tail)
  end

  # Makes sure to check the 1-13 corner case. (i.e. Makes sure that the Ace and the King are considered consecutive.)
  def card_sequence?(tail) do
    Enum.drop(tail, 3) == [13] && sequence?(Enum.take(tail, 3))
  end

  # Returns the winning hand.
  def winner_is(hand1, hand2) do
    score1 = evaluate(hand1)
    score2 = evaluate(hand2)

    magnitude = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1] # List representing the power ranking of the cards.

    # Show both hands without the suits.
    hand1_num = as_numbers(hand1)
    hand2_num = as_numbers(hand2)

    # Creates a list with the index of each element of the hand on the magnitude list.
    # i.e. Each element of hand?_pos is the "strength" of the card.
    hand1_pos = Enum.map(hand1_num, fn card -> Enum.find_index(magnitude, fn pos -> pos == card end) end)
    hand2_pos = Enum.map(hand2_num, fn card -> Enum.find_index(magnitude, fn pos -> pos == card end) end)

    # Returns the hand with highest score or calls the tie breaker functions.
    (score1 > score2 && hand1) ||
    (score2 > score1 && hand2) ||
    tiebreak(hand1_pos, hand2_pos) == "hand1" && hand1 ||
    tiebreak(hand1_pos, hand2_pos) == "hand2" && hand2 ||
    tiebreak_by_suits(hand1, hand2)

  end

  # Base case for the tiebreak function.
  def tiebreak([], []) do
    false
  end

  # Makes sure to tie break by ranks.
  def tiebreak(hand1, hand2) do
    max_value1 = Enum.max(hand1)
    max_value2 = Enum.max(hand2)


    (max_value1 > max_value2 && "hand1") ||
    (max_value2 > max_value1 && "hand2") ||
    tiebreak(tl(hand1), tl(hand2))
  end

  # Makes sure to tie break by suits.
  def tiebreak_by_suits(hand1, hand2) do
    List.last(hand1) > List.last(hand2) && hand1 || hand2
  end

end
