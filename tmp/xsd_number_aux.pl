  [
    digitRemainderSeq/2, % +Integer:nonneg
                         % -Sequence:list(nonneg)
    digitSeq/2, % +Integer:nonneg
                % -Sequence:list(nonneg)
    digitSequenceValue/2, % +Digits:list(between(0,9))
                          % -Integer:nonneg
    fractionDigitsCanonicalFragmentMap//1, % +Fraction:rational
    fractionDigitSequenceValue/2, % +Digits:list(between(0,9))
                                  % -Fraction:rational
    lastSignificantDigit/2 % +Sequence:list(nonneg)
                           % -Last:nonneg
  ]
