(defparameter *consonants*
  (list
   "b" "c" "d" "f" "g" "h" "j" "k" "l" "m"
   "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"
   "bl" "br" "ch" "ck" "cl" "cr" "cs" "dd" "dl" "ds" "dw"
   "ff" "fl" "fr" "fs" "gh" "gl" "gn" "gr" "gs"
   "kl" "kn" "kr" "ks" "lf" "ll" "ln" "lt" "ls"
   "mn" "ms" "nd" "ng" "ns" "ph" "pl" "pr" "ps" "pt" "qu"
   "rf" "rh" "rk" "rl" "rm" "rn" "rr" "rs"
   "sc" "sh" "sk" "sm" "sn" "sp" "sr" "ss" "st" "sw"
   "th" "tr" "ts" "tt" "wh" "wl" "zh" "zz"
   "ddl" "ffl" "nch"
   "sch" "scr" "shl" "shr" "skr" "spl" "spr" "squ"
   "tch" "thr" "thw" "ttl")
  "Consonants and some combinations")

(defparameter *vowels*
  (list
    "a"  "e"  "i"  "o"  "u"  "y" 
   "aa" "ae" "ai" "ao" "au" "ay"
   "ea" "ee" "ei" "eo" "eu" "ey"
   "ia" "ie" "ii" "io" "iu" "iy"
   "oa" "oe" "oi" "oo" "ou" "oy"
   "ua" "ue" "ui" "uo" "uu" "uy")
  "this represents a language with a lot of dipthongs")

(defun pick-elt (seq
                 &optional
                 (pickf #'(lambda (seq) (elt seq (random (length seq))))))
  (funcall pickf seq))

(defun make-wordlike (word-length start
                      &key (consonants *consonants*) (vowels *vowels*)
                      &aux (next (if (eq start 'vowels) 'consonants 'vowels)))
  "make a more-or-less pronouncable word-like string"
  (with-output-to-string (word)
    (loop for n from 0 to (1- word-length)
       do (if (evenp n)
              (format word "~A"
                      (pick-elt (symbol-value start)))
              (format word "~A"
                      (pick-elt (symbol-value next) )) )) ))

;; FIXME: consonants unbound

;; CL-USER> (make-wordlike 6 'vowels) "uerkoiplist"
;; CL-USER> (make-wordlike 6 'consonants) "llydairloi"
;; CL-USER> (make-wordlike 6 'consonants) "scraurfiysche"
;; CL-USER> (make-wordlike 6 'consonants) "meisciwle"
;; CL-USER> (make-wordlike 5 'consonants) "lnoyndeosch"
;; CL-USER> (make-wordlike 5 'consonants) "schaushreypt"
;; CL-USER> (make-wordlike 4 'consonants) "wheetha"
;; CL-USER> (make-wordlike 4 'vowels) "iyghuuwh"
  
(defun make-password (&optional word)
  (format nil "~A~04D"
          (cond ((stringp word) word)
                (t "Password"))
          (random 10000) ))
