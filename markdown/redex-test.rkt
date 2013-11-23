#lang racket

(module test racket
  (require rackunit
           redex/reduction-semantics
           sexp-diff
           racket/sandbox
           "parse.rkt")

  ;; grammar for html
  ;; using spec from (html 4.01 I think):
  ;;   http://www.cs.utah.edu/plt/snapshots/current/doc/html/index.html

  ;; non-terminals have "$" prefix, eg $attr
  (define-language HTML
    ($html (html $attrs $html-contents))
    ($html-contents ($head) ($body) ($head $body))
    ;; ($html-content $head $body)

    ($attr (attr $name $val))
    ;; limit number of attrs to help random generator
    ($attrs () ($attr) ($attr $attr) ($attr $attr $attr))
    ($name str)
    ($val str natural)
    
    (str (side-condition 
          string_1 
          (and (not (string=? (term string_1) ""))
               (andmap char-alphabetic? (string->list (term string_1))))))

    ($head (head $attrs $head-contents))
    ($head-contents ($head-content ...))
    ($head-content $base $isindex $alink $meta $object $script $style $title)
    ($base (base $attrs))
    ($isindex (isindex $attrs))
    ($alink (link $attrs))
    ($meta (meta $attrs))
    ($object (object $attrs $obj-contents))
    ($obj-contents ($obj-content ...))
    ($obj-content $param $g2)
    ($param (param $attrs))
    ($script (script $attrs $pcdatas))
    ($pcdatas ($pcdata ...))
    ($pcdata (pcdata str))
    ($style (style $attrs $pcdatas))
    ($title (title $attrs $pcdatas))

    ($body (body $attrs $body-contents))
    ($body-contents ($body-content ...))
    ($body-content $del $ins $g2)
    ($del (del $attrs $g2s))
    ($ins (ins $attrs $g2s))
    ;; g2
    ($g2s ($g2 ...))
    ($g2 $form $g3)
    ($form (form $attrs $g3s))
    ;; g3
    ($g3s ($g3 ...))
    ($g3 $fieldset $isindex $g4 $g11)
    ($fieldset (fieldset $attrs $fieldset-contents))
    ($fieldset-contents ($fieldset-content ...))
    ($fieldset-content $legend $g2)
    ($legend (legend $attrs $g5s))
    ;; g4
    ($g4s ($g4 ...))
    ($g4 $g8 $g10)
    ;; g5
    ($g5s ($g5 ...))
    ($g5 $label $g6)
    ($label (label $attrs $g6s))
    ;; g6
    ($g6s ($g6 ...))
    ($g6 $a $g7)
    ($a (a $attrs $a-contents))
    ($a-contents ($a-content ...))
    ($a-content $label $g7)
    ;; g7
    ($g7 $g8 $g12)
    ;; g8
    ($g8 $applet $basefont $big $font $img $object $small $sub $sup $g9)
    ($applet (applet $attrs $applet-contents))
    ($applet-contents ($applet-content ...))
    ($applet-content $param $g2)
    ($basefont (basefont $attrs))
    ($big (big $attrs $g5s))
    ($font (font $attrs $g5s))
    ($img (img $attrs))
    ($small (small $attrs $g5s))
    ($sub (sub $attrs $g5s))
    ($sup (sup $attrs $g5s))
    ;; g9
    ($g9 $abbr $acronym $b $bdo $br $cite $code $dfn $em $i $kbd $map $pcdata $q
         $s $samp $script $span $strike $strong $tt $u $var)
    ($abbr (abbr $attrs $g5s))
    ($acronym (acronym $attrs $g5s))
    ($b (b $attrs $g5s))
    ($bdo (bdo $attrs $g5s))
    ($br (br $attrs))
    ($cite (cite $attrs $g5s))
    ($code (code $attrs $g5s))
    ($dfn (dfn $attrs $g5s))
    ($em (em $attrs $g5s))
    ($i (i $attrs $g5s))
    ($kbd (kbd $attrs $g5s))
    ($map (map $attrs $map-contents))
    ($map-contents ($map-content ...))
    ($map-content $area $fieldset $form $isindex $g10)
    ($area (area $attrs))
    ($q (q $attrs $g5s))
    ($s (s $attrs $g5s))
    ($samp (samp $attrs $g5s))
    ($span (span $attrs $g5s))
    ($strike (strike $attrs $g5s))
    ($strong (strong $attrs $g5s))
    ($tt (tt $attrs $g5s))
    ($u (u $attrs $g5s))
    ($var (var $attrs $g5s))
    ;; g10
    ($g10 $address $blockquote $center $dir $div $dl $h1 $h2 $h3 $h4 $h5 $h6 $hr
          $menu $noframes $noscript $ol $p $pre $table $ul)
    ($address (address $attrs $addr-contents))
    ($addr-contents ($addr-content ...))
    ($addr-content $p $g5)
    ($blockquote (blockquote $attrs $g2s))
    ($center (center $attrs $g2s))
    ($dir (dir $attrs $lis))
    ($div (div $attrs $g2s))
    ($dl (dl $attrs $dl-contents))
    ($dl-contents ($dl-content ...))
    ($dl-content $dd $dt)
    ($dd (dd $attrs $g2s))
    ($dt (dt $attrs $g5s))
    ($lis ($li ...))
    ($li (li $attrs $g2s))
    ($h1 (h1 $attrs $g5s))
    ($h2 (h2 $attrs $g5s))
    ($h3 (h3 $attrs $g5s))
    ($h4 (h4 $attrs $g5s))
    ($h5 (h5 $attrs $g5s))
    ($h6 (h6 $attrs $g5s))
    ($hr (hr $attrs))
    ($menu (menu $attrs $lis))
    ($noframes (noframes $attrs $g2s))
    ($noscript (noscript $attrs $g2s))
    ($ol (ol $attrs $lis))
    ($p (p $attrs $g5s))
    ($pre (pre $attrs $pre-contents))
    ($pre-contents ($pre-content ...))
    ($pre-content $g9 $g11)
    ;;table
    ($table (table $attrs $table-contents))
    ($table-contents ($table-content ...))
    ($table-content $caption $col $colgroup $tbody $tfoot $thead)
    ($caption (caption $attrs $g5s))
    ($cols ($col ...))
    ($col (col $attrs))
    ($colgroup (colgroup $cols))
    ($thead (thead $attrs $trs))
    ($tfoot (tfoot $attrs $trs))
    ($tbody (tbody $attrs $trs))
    ($trs ($tr ...))
    ($tr (tr $attrs $tr-contents))
    ($tr-contents ($tr-content ...))
    ($tr-content $td $th)
    ($td (td $attrs $g2s))
    ($th (th $attrs $g2s))
    ;; end table
    ($ul (ul $attrs $lis))
    ;; g11
    ($g11 $a $label $g12)
    ;; g12
    ($g12 $button $iframe $input $select $textarea)
    ($button (button $attrs $g4s))
    ($iframe (iframe $attrs $g2s))
    ($input (input $attrs))
    ($select (select $attrs $select-contents))
    ($select-contents ($select-content ...))
    ($select-content $optgroup $option)
    ($optgroup (optgroup $attrs $options))
    ($options ($option ...))
    ($option (option $attrs $pcdatas))
    ($textarea (textarea $attrs $pcdatas))
    )

  ;; html->str
  (define-metafunction HTML
    html->str : any -> string
    [(html->str (pcdata str)) ,(format " ~a" (term str))]
    [(html->str (any $attrs)) ;; no content
     ,(let ([attrs-str (attrs->str (term $attrs))])
        (case (random 2)
          [(0) (format "<~a~a></~a>" (term any) attrs-str (term any))]
          [(1) (format "<~a~a />" (term any) attrs-str)]))]
    [(html->str (any_1 $attrs (any_2 ...)))
     ,(format "<~a~a>~a</~a>"
              (term any_1)
              (attrs->str (term $attrs))
              (string-join (map (λ (t) (term (html->str ,t))) (term (any_2 ...))) "")
              (term any_1))]
    [(html->str any) ,(~a (term any))])

  (define (attrs->str attrs)
    (if (null? attrs) ""
        (string-join #:before-first " "
                     (map (λ (a) (term (attr->str ,a))) attrs))))

  (define-metafunction HTML
    attr->str : $attr -> string
    [(attr->str (attr $name $val)) ,(format "~a='~a'" (term $name) (term $val))])

  ;; html->md
  (define-metafunction HTML
    html->md : any -> any
    [(html->md (pcdata str)) ,(format " ~a" (term str))]
    [(html->md (any $attrs)) (any ,(attrs->md (term $attrs)))]
    [(html->md (any_1 $attrs (any_2 ...)))
     (any_1 ,(attrs->md (term $attrs))
            ,@(let ([content (map (λ (t) (term (html->md ,t))) (term (any_2 ...)))])
                (if (and (not (null? content)) (andmap string? content))
                    (list (string-join content ""))
                    content)))]
    [(html->md any) any])

  (define (attrs->md attrs) (map (λ (a) (term (attr->md ,a))) attrs))

  (define-metafunction HTML
    attr->md : $attr -> (any string)
    [(attr->md (attr $name $val)) 
     ,(list (string->symbol (term $name))
            (if (number? (term $val))
                (number->string (term $val))
                (term $val)))])

  ;; html->md can create elements like (tag () "a" "b") but instead we
  ;; want all consecutive string elements appended: (tag () "ab")
  (define (fix-xexpr x) ; xexpr -> xexpr
    (match x
      [`(,tag ,as ,es ...)
       `(,tag ,as ,@(let loop ([es es])
                      (match es
                        [(list (? string? this) (? string? next) more ...)
                         (loop (cons (string-append this next) more))]
                        [(cons this more)
                         (cons (fix-xexpr this) (loop more))]
                        ['() '()])))]
      [x x]))
  (check-equal? (fix-xexpr '(x () "a" "b" (y () "a" "b") "a" "b"))
                '(x () "ab" (y () "ab") "ab"))

  (define n 0)
  (define (checker h)
    (define htmlstr (term (html->str ,h)))
    (define parsed
      (with-handlers
          ([exn:fail? (lambda (x)
                        (newline)
                        (displayln (exn-message x))
                        (displayln htmlstr)
                        #f)])
        (call-with-limits 5 #f (thunk (car (parse-markdown htmlstr))))))
    (cond
     [parsed
      (define expected (fix-xexpr (term (html->md ,h))))
      (when (zero? (modulo n 100))
        (printf "~a " n)
        (flush-output (current-output-port)))
      (set! n (add1 n))
      ;; check-equal? doesn't diff sexprs and we want to provide even more
      ;; info than the sexprs
      (define ok? (equal? parsed expected))
      (unless ok? (newline))
      (check-true ok?)
      (unless ok?
        (displayln "#:old is expected #:new is parsed")
        (pretty-print (sexp-diff expected parsed))
        ;; (displayln "Redex generated:")
        ;; (pretty-print h)
        (displayln "HTML string:")
        (displayln htmlstr))
      ok?]
     [else #f]))

  (define attempts 2000)
  (printf "Randomly generating and checking ~a HTML grammar examples:\n"
          attempts)
  (void (redex-check HTML $html (checker (term $html))
                     #:attempts attempts
                     #:attempt-size (λ _ 8)
                     #:print? #f))
  (newline))

;; (require 'test)
