#lang racket

(require (only-in parsack
                  [parser-compose pdo] ;; More concise, less indent
                  [parser-one pdo-one] ;; "
                  [parser-seq pdo-seq] ;; "
                  >>= >>
                  try <or> <?> choice $err
                  satisfy char string stringAnyCase
                  many many1
                  manyTill many1Till
                  manyUntil many1Until
                  sepBy
                  oneOf noneOf oneOfStrings
                  option optional
                  return
                  between
                  lookAhead
                  notFollowedBy
                  $space $newline $anyChar $letter $digit $hexDigit
                  $alphaNum $eof
                  getState setState withState
                  State State? Consumed Consumed! Empty Ok Error Msg
                  parse parse-result parsack-error parse-source
                  incr-pos))

(provide pdo
         pdo-one
         pdo-seq
         >>= >>
         try <or> <?> choice $err
         satisfy char string stringAnyCase
         many many1
         manyTill many1Till
         manyUntil many1Until
         sepBy
         oneOf noneOf oneOfStrings
         option optional
         return
         between
         lookAhead
         notFollowedBy
         $space $newline $anyChar $letter $digit $hexDigit
         $alphaNum $eof
         getState setState withState
         State State? Consumed Consumed! Empty Ok Error Msg
         parse parse-result parsack-error parse-source
         incr-pos)
