#lang racket
(require csc151)

;************************************************************************************
;location of movie file
(define tmdb-table
  (read-csv-file ""))

;location of verb file
(define verb-table
  (read-csv-file ""))
;************************************************************************************

;************************************************************************************
;Clean the Verb Dictionary

;;; Procedure
;;;   not-?
;;; Parameters
;;;   str, any single element
;;; Purpose
;;;   Checks if str is not "-" or ""
;;; Produces
;;;   result, a boolean value
;;; Preconditions
;;;   str is meant to be a string but can be any data type
;;; Postconditions
;;;   * result = #f when str is "-" or "" (an empty string)
;;;     else result = #t
(define not-?
  (lambda (str)
    (not
     (or
      (equal? str "")
      (equal? str "-")
      ))))

;;; Procedure
;;;   remover?
;;; Parameters
;;;   str, any single element
;;;   lst, a list
;;; Purpose
;;;   Checks if str is in lst
;;; Produces
;;;   result, a boolean value
;;; Preconditions
;;;   str is meant to be a string but can be any data type
;;; Postconditions
;;;   * result = #f when str is in lst
;;;     else result = #t
(define remover?
  (lambda (str lst)
    (let ([val (index-of str lst)])
      (if (< val 0) #t
          #f))))

;a list of verbs we felt should not be in our verb dictionary
;used with the remover? predicate
(define bad-verbs
  (list "in" "of" "house"))

;;; Procedure
;;;   dict-clean
;;; Parameters
;;;   dict, a list
;;; Purpose
;;;   * Cleans up our dictionary of verbs and
;;;     makes it into a vector of strings
;;;   * Sorts the strings alphabetically
;;;   * Adds extra strings we felt the dictionary was lacking
;;; Produces
;;;   result, a vector
;;; Preconditions
;;;   * dict is in the form of a list of list.
;;;     In our case coming from a .csv file
;;; Postconditions
;;;   * The strings in the vector are sorted alphabetically
;;;   * New words added to the vector
;;;   * No duplicate strings in the vector
(define dict-clean
  (lambda (dict)
    (list->vector
     (sort
      (map car (tally-all
                (append
                 (list  "were" "are" "is" "was" "been" "has")
                 (filter (conjoin not-? (section remover? <> bad-verbs))
                         (reduce append
                                      (cdr dict))))
                )) string-ci<?))))

;************************************************************************************

;************************************************************************************
;Clean the table of genres and plots

;;; Procedure
;;;   word-space?
;;; Parameters
;;;   char, a character
;;; Purpose
;;;   Checks if char is an alphabetic or whitespace character
;;; Produces
;;;   result, a boolean value
;;; Preconditions
;;;   [no additional]
;;; Postconditions
;;;   * result = #t when char is an alphabetic
;;;     character or a whitespace character
;;;     else result = #f
(define word-space?
  (lambda (char)
    (or (char-alphabetic? char)
        (char-whitespace? char))))

;;; Procedure
;;;   id-name?
;;; Parameters
;;;   str, any single element
;;; Purpose
;;;   Checks if str is not "id" or "name"
;;; Produces
;;;   result, a boolean value
;;; Preconditions
;;;   str is meant to be a string but can be any data type
;;; Postconditions
;;;   * result = #f when str is "id" or "name"
;;;     else result = #t
(define id-name?
  (lambda (str)
    (not
     (or (equal? "id" str)
         (equal? "name" str)
         ))))

;;; Procedure
;;;   clean-genre
;;; Parameters
;;;   str, a string
;;; Purpose
;;;   * Creates a list of only the names of the genres
;;;     as strings
;;; Produces
;;;   result, a list
;;; Preconditions
;;;   * str, a string with the names of genres
;;;     separated by whitespaces
;;;   * str, can contain the strings "id" and "name"
;;;     but they will be removed
;;; Postconditions
;;;   * result, a list of only strings
;;;   * the strings in result can be found in str
(define clean-genre
  (lambda (str)
    (filter id-name?
            (string-split (list->string
                           (filter word-space? (string->list str))
                           )))))

;;; Procedure
;;;   select-columns
;;; Parameters
;;;   table, a list of lists
;;;   column-indices, a list of natural numbers
;;; Purpose
;;;   create a new table with only the columns of table present in column-indices.  
;;; Produces
;;;   result, a new table
;;; Preconditions
;;;   all numbers in column-indices must be smaller than the length of the elements inside table.
;;; Postconditions
;;;   result will have the same length as table, but (length (car table)) will not be equal to (length ;;;   (car result))

(define select-columns
  (lambda (table column-indices)    
    (let kernel ([result (make-list (length table) null)]
                 [remaining column-indices]
                 [current-table table]
                 [position 0])      
      (cond [(null? remaining)
             (map reverse result)]
            [(null? table)
             "something went wrong"]           
            [(equal? (car remaining) position)
             (kernel (map cons (map car current-table) result)
                     (cdr remaining)
                     (map cdr current-table)
                     (+ 1 position))]            
            [(kernel result
                     remaining
                     (map cdr current-table)
                     (+ 1 position))]))))

;;; Procedure
;;;   empty-plot?
;;; Parameters
;;;   lst, a list
;;; Purpose
;;;   Checks if the plot column is not "" or " "
;;; Produces
;;;   result, a boolean value
;;; Preconditions
;;;   lst, a list of at least two elements
;;; Postconditions
;;;   * result = #f when cadr of lst is "" or " "
;;;     else result = #t
(define empty-plot?
  (lambda (plot)
    (not (or
          (equal? (cadr plot) "")
          (equal? (cadr plot) " ")))
    ))

;takes columns 2 and 8 and cleans up the genres in column 2

;;; Procedure
;;;   clean-table
;;; Parameters
;;;   table, list of lists
;;; Purpose
;;;   * Creates a list of lists containing
;;;     a list of genres and a plot
;;; Produces
;;;   result, a list
;;; Preconditions
;;;   * table is in the form of the tmdb-table
;;;     specifically a lists of lists with
;;;     at least 8 elements and the 2nd element
;;;     in the list is a single string containing movie genres and
;;;     the 8th element in the list is a movie plot as a string
;;; Postconditions
;;;   * result, a list of lists containing
;;;     a list of genres as strings and
;;;     a plot as a single string
(define clean-table
  (lambda (table)
    (let ([removed-titles (cdr table)])
      (filter empty-plot?
              (map append
                   (map list
                        (map clean-genre
                             (map car (select-columns removed-titles '(1)))))
                   (select-columns removed-titles '(7)))))))

;;; Procedure
;;;   genres
;;; Parameters
;;;   table, a list of lists
;;; Purpose
;;;   generates the list of genres appeared in the table
;;; Produces
;;;   result, a list
;;; Preconditions
;;;   the second column of the table should be genres
;;; Postconditions
;;;   the list will contain a list of strings, which are capitalized.
(define genres
  (lambda (table)
    (map car
         (tally-all
          (reduce append
                  (filter (negate null?)
                          (map (o clean-genre (section list-ref <> 1))
                               table)))))
    ))
;************************************************************************************

;************************************************************************************
;Verb Search

;Modified binary-search from Reading: Algorithms for searching lists and vectors
; https://www.cs.grinnell.edu/~hamidfah/courses/csc151fall2018/readings/searching.html
;Searches a vector of strings to see if a given string exist inside it

;;; Procedure:
;;;   binary-search-str
;;; Parameters:
;;;   vec, a vector to search
;;;   key, a key we're looking for
;;; Purpose:
;;;   Search vec for a string that is equal to key.
;;; Produces:
;;;   result, a boolean value
;;; Preconditions:
;;;   * The vector is sorted alphabetically
;;;   * If two values are equal, then each may precede the other.
;;;   * Similarly, if two values may each precede the other, then
;;;     the two values are equal.
;;; Postconditions:
;;;   * If vector contains no strings that is equal to key, result is #f
;;;   * If vec contains an element that is equal to key, result is #t
(define binary-search-str
  (lambda (vec key)
    (let search-portion ([lower-bound 0]
                         [upper-bound (- (vector-length vec) 1)])
      (if (> lower-bound upper-bound)
          #f
          (let* ([point-of-division (quotient (+ lower-bound upper-bound) 2)]
                 [separating-element (vector-ref vec point-of-division)]
                 [left? (string-ci<=? key separating-element)]
                 [right? (string-ci<=? separating-element key)])
            (cond
              [(and left? right?)
               #t]
              [left?
               (search-portion lower-bound (- point-of-division 1))]
              [else
               (search-portion (+ point-of-division 1) upper-bound)]))))))

;************************************************************************************

;************************************************************************************

;;; Procedure
;;;   random-3
;;; Paramaters
;;;   table, a list of lists
;;; Purpose
;;;   to return 3 different random numbers in the range [0, (- (length table) 1)]
;;; Produces
;;;   result, a list
;;; Preconditions
;;;   table must be of length >= 3
;;; Postconditions
;;;   result will only contain natural numbers

(define random-3
  (lambda (table genre)
    (let ([length-of-table (length table)])
      (let kernel ([rand1 (random length-of-table)]
                   [rand2 (random length-of-table)]
                   [rand3 (random length-of-table)])
        (cond
          [(<= (count-sentence (cadr (list-ref (filter-plots-by-genre table genre) rand1))) 1)
           (kernel (random length-of-table) rand2 rand3)]
          [(or (equal? rand3 rand2)
               (equal? rand1 rand3))
           (kernel rand1 rand2 (random length-of-table))]
          [(equal? rand1 rand2)
           (kernel  rand1 (random length-of-table) rand3)]
          [(list rand1 rand2 rand3)])))))
;************************************************************************************

;************************************************************************************
;generate a list of plots which has the tag of given genre

;;; Procedure
;;;   contain?
;;; Parameters
;;;   str, a string
;;;   lst, a list
;;; Purpose
;;;   * Checks if a list contains a given element
;;;   * It is case sensitive
;;; Produces
;;;   result, a boolean value
;;; Preconditions
;;;   lst, a list of any data type but meant to be strings
;;;   str, can be any data type but meant to be a string
;;; Postconditions
;;;   * result = #t if lst contains str
;;;     else result = #f
(define contain?
  (lambda (str lst)
    (if (>= (index-of str lst) 0)
        #t
        #f)
    ))

;;; Procedure
;;;   genre?
;;; Parameters
;;;   lst, a list
;;;   str, a string
;;; Purpose
;;;   * Checks if the first element lst contains a given element
;;;   * It is case sensitive
;;; Produces
;;;   result, a boolean value
;;; Preconditions
;;;   lst, the first element of the list must be a list
;;;   str, can be any data type but meant to be a string
;;; Postconditions
;;;   * result = #t if the first element of lst contains str
;;;     else result = #f
(define genre?
  (lambda (lst str)
    (contain? str (car lst))
    ))

;;; Procedure
;;;   filter-plots-by-genre
;;; Parameters
;;;   tbl, a list
;;;   str, a string
;;; Purpose
;;;   * filters for elements that contain a given genre
;;;   * It is case sensitive
;;; Produces
;;;   result, a list
;;; Preconditions
;;;   * lst, is a lists of lists where
;;;     the first element of the list must be a list
;;;   * str, can be any data type but meant to be a string
;;; Postconditions
;;;   * result, list of lists where the
;;;     the first element of the lists are
;;;     a list that contains str
(define filter-plots-by-genre
  (lambda (tbl str)
    (filter (section genre? <> str) tbl)
    ))
;************************************************************************************

;************************************************************************************
;Removing punctuation attacted to string

;punctuation we want to remove
(define punctuation
  '("," "." "’" "'" "-" "(" ")" "–" "!" ":" ";" "‘" "—" "\"" "·" "..." "&" "?" "“" "”" "/" "$" "-" "#" "®" "[" "_" "\u00AD" "|" "¡" "¦" "―" "%" "£" "]" "`" "+" "~"))

#|
;used to find all the punctuation in
  (map
   (o list->string list car)
   (tally-all
    (filter (conjoin (negate char-whitespace?)
                     (negate char-numeric?)
                     (negate char-alphabetic?))
            (string->list
             (reduce word->sentence
                     (reduce append (select-columns tmdb '(7))))))))
|#

;;; Procedure
;;;   clean-punc
;;; Paramaters
;;;   str, a string
;;;   punc, a string
;;; Purpose
;;;   to remove puc if it is at the end, or begining of str
;;; Produces
;;;   result, a string
;;; Precondtions
;;;   [no additional]
;;; Postconditions
;;;   result will be str without punc if punc is at the beginning or end of str.

(define clean-punc
  (lambda (str punc)
    (let kernel ([for-now str])
      (if (equal? (string-trim for-now punc)
                  for-now)
          for-now
          (kernel (string-trim for-now punc))
          ))))

;;; Procedure
;;;   clean-puncs
;;; Paramaters
;;;   str, a string
;;;   lst, a list of strings
;;; Purpose
;;;   to remove the elements of lst from str if they are at the end, or begining of str
;;; Produces
;;;   result, a string
;;; Precondtions
;;;   [no additional]
;;; Postconditions
;;;   result will be str without the elements of lst if the elements are at the beginning or end of str.

(define clean-puncs
  (lambda (str lst)
    (let kernel ([for-now str]
                 [remaining lst])
      (if (null? remaining)
          for-now
          (kernel (clean-punc for-now (car remaining)) (cdr remaining))
          ))))

;;; Procedure
;;;   clean-puncs
;;; Paramaters
;;;   str, a string
;;; Purpose
;;;   to remove the elements of punctuation from str if they are at the end, or begining of str
;;; Produces
;;;   result, a string
;;; Precondtions
;;;   [no additional]
;;; Postconditions
;;;   result will be str without the elements of punctuations if the elements are at the beginning or end of str.


(define clean-puncs-refined
  (lambda (str)
    (let kernel ([for-now str]
                 [remaining punctuation])
      (if (null? remaining)
          for-now
          (kernel (clean-punc for-now (car remaining)) (cdr remaining))
          ))))
;************************************************************************************

;************************************************************************************
;indentify the end of sentences and the second sentence given a paragraph

;;; Procedure
;;;   safe-cadr
;;; Parameters
;;;   lst, a list
;;; Purpose
;;;   * Performs cadr if the list has two or more elements
;;;   * Produces null if the list only has one element
;;; Produces
;;;   result, the second element in lst or null
;;; Preconditions
;;;   * lst must have at least one element
;;; Postconditions
;;;   * If lst has one element result = null
;;;     else result = (cadr lst)
(define safe-cadr
  (lambda (lst)
    (if (null? (cdr lst)) null
        (cadr lst))))

;;; Procedure
;;;   end-of-sentence
;;; Parameters
;;;   str, a string
;;; Purpose
;;;   to return everything preceding the end of the first sentence in str.
;;; Produces
;;;   result, a new string
;;; Preconditions
;;;    [no additional]
;;; Postconditions
;;;    if str has none of the sentence ending punctuation (#\. #\? #\! #\;), the procedure will return “no end of sentence”.

(define end-of-sentence
  (lambda (str)
    (let ([lst-str (string->list str)])
      (let kernel ([remaining lst-str]
                   [count 0])
        (cond [(null? remaining) "no end of sentence"]
              [(and
                (or (equal? (car remaining) #\.)
                    (equal? (car remaining) #\?)
                    (equal? (car remaining) #\!))
                (or (null? (safe-cadr remaining))
                    (equal? #\space (safe-cadr remaining))))
               (list->string(take lst-str (+ 1 count)))]
              [(kernel (cdr remaining) (+ 1 count))])))))

;;; Procedure:
;;;   contain-end?
;;; Parameters:
;;;   str, string
;;; Purpose:
;;;   determine if the word has an end punctuation such as . ? !
;;; Produces:
;;;   result, a boolean
;;; Preconditions
;;;   [no additional]
;;; Postconditions
;;;   result will be true if str is a sentence ending punctuation mark
;;;   otherwise, it will be false.

(define contain-end?
  (lambda (str)
    (let ([lst (string->list str)])
      (let kernel ([remaining lst])
        (cond
          [(null? remaining) #f]
          [(or (equal? (car remaining) #\.)
               (equal? (car remaining) #\?)
               (equal? (car remaining) #\!)) #t]
          [else (kernel (cdr remaining))]
          )))))

;;; Procedure:
;;;   count-sentence
;;; Parameters:
;;;   str, string
;;; Purpose:
;;;   count how many sentences we have
;;; Produces:
;;;;  result, an integer
;;; Preconditions
;;;   [no additional]
;;; Postconditions
;;;   result will be a positive integer

(define count-sentence
  (lambda (str)
    (let* ([lst (string-split str)])
      (let kernel ([remaining lst]
                   [count 0])
        (cond
          [(null? remaining) count]
          [(contain-end? (car remaining)) (kernel (cdr remaining) (increment count))]
          [else (kernel (cdr remaining) count)]
          )))))

;;; Procedure:
;;;   second-sentence
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   return the second sentence in the given string
;;; Produces:
;;;   result, a string
;;; Preconditions
;;;   str must be longer than one sentence.
;;; Postconditions
;;;   result will be the second sentence of str.

(define second-sentence
  (lambda (str)
    (let ([lst (string-split str)])
      (let kernel ([remaining lst]
                   [count 0])
        (cond
          [(= count 1) (reduce word->sentence remaining)]
          [(contain-end? (car remaining)) (kernel (cdr remaining) (increment count))]
          [else (kernel (cdr remaining) count)]
          )))))

; procedure to append two words together with a " " in between.
(define word->sentence
  (section string-append <> " " <>))
;************************************************************************************

;************************************************************************************
;Grabbing strings around the verb

;;; Procedure
;;;   the-verb
;;; Paramaters
;;;   str, a string
;;;   dict, a vector of verbs in string format
;;; Purpose
;;;   to return the verb of a sentence
;;; Produces
;;;   result, a string
;;; Preconditions
;;;   [no additional]
;;; Postconditions
;;;   result will be a verb, however if no verb is found, the procedure will return "no verb found".

(define the-verb
  (lambda (str dict)
    (let kernel ([remaining (string-split str)]
                 [count 0]
                 [current null])
      (cond [(null? (cdr remaining)) "killed"]           
            [(and (binary-search-str dict (clean-puncs-refined (car remaining)))
                  (not (binary-search-str dict (clean-puncs-refined (cadr remaining)))))
             (append current (take remaining 1))]            
            [(not (binary-search-str dict (clean-puncs-refined (car remaining))))
             (kernel (cdr remaining)
                     (+ 1 count)
                     current)]
            [(kernel (cdr remaining)
                     (+ 1 count)
                     (append current (take remaining 1)))]
            ))))

;;; Procedure
;;;   after-verb
;;; Paramaters
;;;   str, a string
;;;   dict, a vector of verbs in string format
;;; Purpose
;;;   to return everything that comes after a verb in str.
;;; Produces
;;;   result, a new string
;;; Preconditions
;;;   [no additional]
;;; Postconditions
;;;    result will be a string, however if no verb is found, the procedure will return "no verb found"

(define after-verb
  (lambda (str dict)
    (let* ([split-list (string-split str)]
           [verb-position (index-of (car (reverse (the-verb str dict))) split-list)])
      (drop split-list (+ 1 verb-position)))))

;;; Procedure
;;;   after-verb
;;; Paramaters
;;;   str, a string
;;;   dict, a vector of verbs in string format
;;; Purpose
;;;   to return everything that comes before a verb in str.
;;; Produces
;;;   result, a string
;;; Preconditions
;;;   [no additional]
;;; Postconditions
;;;    result will be a string, however if no verb is found, the procedure will return "Adam Sandler".

(define before-verb
  (lambda (str dict)
    (let ([split-list (string-split str)])
      (let kernel ([remaining split-list]
                   [count 0]
                   [was-verb #t])
        (cond [(null? remaining) (list "Adam Sandler")]
              [else
               (let ([is-verb
                      (or (binary-search-str (vector "after" "before" "during") (car remaining))
                          (binary-search-str dict (clean-puncs-refined (car remaining))))])
                 (cond
                   [(and
                     is-verb
                     (not was-verb))
                    (take split-list count)]
                   [is-verb
                    (kernel (cdr remaining) (+ 1 count) #t)]
                   [(kernel (cdr remaining)
                            (+ 1 count) #f)]
                   ))])))))
;************************************************************************************

;************************************************************************************
;Cleaned Up Verb Dictionary
(define full-verb-dict
  (dict-clean verb-table))

;Cleaned Up Table
(define cleaned-table
  (clean-table tmdb-table))

;A list of all the genres in the tmdb data file
(define genre-list
  '("Action"
    "Adventure"
    "Fantasy"
    "Science"
    "Fiction"
    "Crime"
    "Drama"
    "Thriller"
    "Animation"
    "Family"
    "Western"
    "Comedy"
    "Romance"
    "Horror"
    "Mystery"
    "History"
    "War"
    "Music"
    "Documentary"
    "Foreign"
    "TV"
    "Movie"))
;************************************************************************************

;************************************************************************************
;;; Procedure:
;;;   final
;;; Parameters:
;;;   table, a list
;;;   genre, a string
;;;   verb-dict, a vector
;;; Purpose:
;;;   to generate a random new plot out of the given plots of a certain genre
;;; Produces:
;;;;  result, a string
;;; Preconditions:
;;;   * table, a list containing
;;;     a list of genres as strings, in the first column and
;;;     a plot as a string, in the second column
;;;   * genre, string that is equal to
;;;     at least one genre in table (case sensitive)
;;;   * verb-dict, a vector of strings sorted alphabetically
;;; Postconditions:
;;;   result will be at least 2 sentences long.
(define final
  (lambda (table genre verb-dict)
    (let*
        ([filtered-by-genre (filter-plots-by-genre table genre)]
         [random-nums (random-3 filtered-by-genre genre)]
         [plot1 (cadr (list-ref filtered-by-genre (car random-nums)))]
         [plot2 (cadr (list-ref filtered-by-genre (cadr random-nums)))]
         [plot3 (cadr (list-ref filtered-by-genre (caddr random-nums)))]
         [predicate (before-verb plot1 verb-dict)]
         [verb (the-verb plot2 verb-dict)]
         [subject (after-verb plot3 verb-dict)]
         [predicate2 (before-verb (second-sentence plot1) verb-dict)]
         [verb2 (the-verb (cadr (list-ref filtered-by-genre (random (length filtered-by-genre)))) verb-dict)]
         [subject2 (after-verb (cadr (list-ref filtered-by-genre (random (length filtered-by-genre)))) verb-dict)])
      ;(display (list 'the-plots (list plot1) (list plot2) (list plot3))) (newline)
      ;(display (list 'let* predicate verb subject predicate2 verb2 subject2))
      (string-append
       (reduce word->sentence
               (append predicate verb))
       " "
       (end-of-sentence
        (reduce word->sentence subject))
       " "
       (reduce word->sentence
               (append predicate2 verb2))
       " "
       (end-of-sentence
        (reduce word->sentence subject2)))
      )))
;(final cleaned-table "Comedy" full-verb-dict)
;(final cleaned-table "Action" full-verb-dict)

