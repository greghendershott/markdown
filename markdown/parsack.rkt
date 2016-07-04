#lang racket/base

(require (for-syntax racket/base)
         racket/format
         racket/function
         racket/list
         racket/match
         (only-in parsack
                  [parser-compose pdo] ;; More concise, less indent
                  [parser-one pdo-one] ;; "
                  [parser-seq pdo-seq] ;; "
                  >>= >>
                  <or> try <?> choice $err err
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
                  Consumed Empty Ok Error
                  parse parse-result parsack-error parse-source))

(provide pdo
         pdo-one
         pdo-seq
         >>= >>
         <or> try <?> choice $err err
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
         Consumed Empty Ok Error
         parse parse-result parsack-error parse-source)
