#| Assignment 1 - Racket Query Language (due February 11, noon)

***Write the names and CDF accounts for each of your group members below.***
Jasmin Lantos, g4lantos
Haris Shoaib, g3shoaib
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         atom-replace replace where order-by SELECT)


; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  (first table))

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  (rest table))

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  (length (rest table)))


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (corresponding-value attr string tuple)
  (second 
   (first
    (filter (lambda (x) (equal? (first x) string)) 
            (map (lambda (x y) (list x y)) attr tuple)))))

#|Returns true if x is in list of attributes
  - x
  - attrs: a list of attributes

>(in-attrs 8 '(1 3 5 6 8 9 7))
#t
>(in-attrs 8 '(1 3 5 6 9 7))
#f
|#
(define (in-attrs x attrs)
  (if (false? (member x attrs)) #f #t))

#|
A function 'replace-attrs' that takes:
  - x 
  - attrs: a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.

>((replace-attrs "food" '("name" "food" "age")) '("david" "icecream" "8"))
"icecream"
>((replace-attrs "hairstyle" '("name" "food" "age")) '("david" "icecream" "8"))
"hairstyle"
|#
(define (replace-attrs x attrs)
  (lambda (tuple) (if (in-attrs x attrs) (corresponding-value attrs x tuple) x))) 
         
#|@105
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.

>(define (e? tuple) (if (equal? tuple '(1 2 3)) #t #f))
>(satisfies e? '(("food" "name" "age") (1 2 3) (#t #f "hey") (1 2 3) ("potato" "jasmin" 22)))
'(("food" "name" "age") (1 2 3) (1 2 3))
|#
(define (satisfies f table)
  (cons (attributes table) (filter (lambda (x) (f x)) (tuples table))))


; ----SELECT HELPERS----

#|Returns element from lst at index i
  - lst: a list
  - i: an index
  - ilist: a list of indices with same length as lst
|#
(define (index lst i ilist)
  (foldl (lambda (x y acc) (if (equal? x i) y acc)) "ERROR" ilist lst))

#|Returns a function which takes a list and returns the item in the list at index 'i', or "ERROR" if 'i' is out of bounds
  - i: an index
|#
(define (list-ref-one i)
  (lambda (lst) (index lst i (range(length lst)))))

#|Returns a function which takes a list and returns the items in the list at the indices specified by 'is' in the order 'is' is given
  - is: a list of indices

>((list-ref-many (get-indices-list '("food" "age") '("food" "age" "name"))) '("yogurt" 11 "david"))
'("yogurt" 11)
|#
(define (list-ref-many is)
  (lambda (lst) (map (lambda (x) ((list-ref-one x) lst)) is)))

#|Returns the index of string in attrs
  - attrs: a list of attributes
  - string: a string representing a single attribute
|#
(define (get-attr-index attrs string) 
  (foldl
   (lambda (y acc) (if (equal? (second y) string) (cons (first y) acc) acc))
   '()
   (map (lambda (x y) (list x y)) (range (length attrs)) attrs)))

#|Returns a list of indices of strings in attrs
  - strings: a list of strings representing attributes in attrs
  - attrs: a list of attributes

>(get-indices-list '("food" "age") '("food" "age" "name"))
'(0 1)
|#
(define (get-indices-list strings attrs)
  (foldr
   (lambda (x acc) (append (get-attr-index attrs x) acc))
   '()
   strings))


; ----SELECT----

#|Returns only the columns in table specified by strings
  - strings: a list of strings representing attributes in the table
  - table: a table

>(select '("food" "age") '(("food" "age" "name") ("yogurt" 11 "david") ("apple" 3 "jasmin") ("banana" 4 "haris")))
'(("food" "age")
  ("yogurt" 11)
  ("apple" 3)
  ("banana" 4))
|#
(define (select strings table)
  (foldl
   (lambda (t acc) (append acc (list ((list-ref-many (get-indices-list strings (attributes table))) t))))
   '()
   table))


; ----FROM HELPERS----

#|Takes an unlimited amount of arguments and returns them in a list
|#
(define unlimited (lambda x x))

#|Combine lst1 with every element of table2
  - lst1: a list
  - table2: a table
|#
(define (cartesian-helper lst1 table2) (map (lambda (next) (append lst1 next)) table2))

#|Return the cartesian product of two tables
  - table1: a table
  - table2: another table

>(cartesian-product '(("age" "name" "date") (11 "Jasmin" 92) (12 "Sally" 30)) '(("food" "name") ("apples" "granny smith")))
'(("age" "name" "date" "food" "name")
  ("age" "name" "date" "apples" "granny smith")
  (11 "Jasmin" 92 "food" "name")
  (11 "Jasmin" 92 "apples" "granny smith")
  (12 "Sally" 30 "food" "name")
  (12 "Sally" 30 "apples" "granny smith"))
|#
(define (cartesian-product table1 table2)
  (foldl (lambda (next sofar) (append sofar (cartesian-helper next table2))) '() table1))

#|Returns a list that contains the attributes lists from each table in tables
  - tables: a list of tables

>(get-tables-attrs '(
    (("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)) 
    
    (("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343"))
    ))
'(("Name" "Age" "LikesChocolate")
  ("Name" "Course"))
|#
(define (get-tables-attrs tables)
  (foldl (lambda (table acc) (append acc (list (attributes table)))) '() tables))

#|Returns a list of all the attributes besides for the the attibutes in the list at index
  - lstattrstables: a list of all the attributes lists from each table
  - index: an index

>(filter-flatten '(("name" "age") ("name" "year") ("age" "food")) 1)
'("name" "age" "age" "food")
|#
(define (filter-flatten lstattrstables index)
  (foldl
   (lambda (x y acc) (if (equal? index y) acc (append acc x)))
   '()
   lstattrstables (range (length lstattrstables))))

#|Prefixes an attribute of the current table with the given table name if any other tables in the query have the same attribute
  - currenttable: the current table
  - lstattrstables: a list of the attributes lists of each table
  - currenttablename: the name given to the current table in the query
  - currenttableindex: the index of the current table's attributes list in lstattrstables

>(replace-duplicates '(("name" "age") ("jasmin" 2)) '(("name" "age") ("name" "year") ("age" "food")) "P" 0)
'(("P.name" "P.age") ("jasmin" 2))
>(replace-duplicates '(("name" "year") ("jasmin" 2)) '(("name" "age") ("name" "year") ("age" "food")) "P" 1)
'(("P.name" "year") ("jasmin" 2))
|#
(define (replace-duplicates currenttable lstattrstables currenttablename currenttableindex)
  (append 
   (list
    (foldl
     (lambda (x acc)
       (if (member x (filter-flatten lstattrstables currenttableindex))
           (append acc (list (string-append currenttablename "." x))) (append acc (list x))))
     '()
     (attributes currenttable)))
   (tuples currenttable)))

#|Renames the attributes of each table when there are name collisions
  - names: a list of all the table names in the order given in the query
  - lsttables: a list of all the tables given in the query

>(define tables (unlimited 
                '(("Name" "Food") ("Jasmin" "Banana")) 
                '(("Number")(1) (2) (3)) 
                '(("Pet" "Name") ("Dog" "Doggie") ("Cat" "Fluffy"))))
>(rename '("P" "Q" "R") tables)
'((("P.Name" "Food") ("Jasmin" "Banana"))
  (("Number") (1) (2) (3))
  (("Pet" "R.Name") ("Dog" "Doggie") ("Cat" "Fluffy")))
|#
(define (rename names lsttables)
  (foldl
   (lambda (x y acc)
     (append acc (list (replace-duplicates x (get-tables-attrs lsttables) ((list-ref-one y) names) y))))
   '()
   lsttables (range (length lsttables))))


; ----FROM----

#|Returns the cartesian product of all the tuples of tables along with a combined attribute list
 -tables: list of all tables

>(define tables (unlimited 
                '(("Name" "Food") ("Jasmin" "Banana")) 
                '(("Number")(1) (2) (3)) 
                '(("Pet" "Name") ("Dog" "Doggie") ("Cat" "Fluffy"))))
>(from tables)
'(("Name" "Food" "Number" "Pet" "Name")
  ("Jasmin" "Banana" 1 "Dog" "Doggie")
  ("Jasmin" "Banana" 1 "Cat" "Fluffy")
  ("Jasmin" "Banana" 2 "Dog" "Doggie")
  ("Jasmin" "Banana" 2 "Cat" "Fluffy")
  ("Jasmin" "Banana" 3 "Dog" "Doggie")
  ("Jasmin" "Banana" 3 "Cat" "Fluffy"))
|#
(define (from tables)
  (cons
   (foldl (lambda (x acc) (append acc (attributes x))) '() tables)
   (foldl (lambda (x acc) (cartesian-product acc (tuples x))) '(()) tables)))


; Starter for Part 4;

#|Macro that returns a binary function that takes a tuple and a list of attributes and returns a boolean value

>(atom-replace (equal? "Age" 1))
#<procedure:...database (4).rkt:328:0>
>((atom-replace (equal? "Age" 1)) '("Name" "Age") '("Jasmin" 1))
#t
>((atom-replace (Or "Guess" (Or #f "Answer"))) '("Guess" "Answer") '(#f #t))
#t
|#
(define-syntax atom-replace
  (syntax-rules ()
    [(atom-replace(<operator> a ...))
     (lambda (attrs tuple) (<operator> ((replace-attrs a attrs) tuple) ...))]
    [(atom-replace <attribute>)
     (lambda (attrs tuple) ((replace-attrs <attribute> attrs) tuple))]))

#|Macro that returns a unary function that takes a tuple and returns a boolean value

>(replace (equal? "Age" 1) '(("Name" "Age" "Food") ("Jasmin" 1 "Potato") ("Haris" 2 "Banana") ("David" 1 "Cake") ("Paul" 100 "Chocolate")
                                                  ("Jen" 3 "Cookies") ("Diane" 1 "Pie")))
#<procedure:...database (4).rkt:347:0>
>((replace (equal? "Age" 1) '(("Name" "Age" "Food") ("Jasmin" 1 "Potato") ("Haris" 2 "Banana") ("David" 1 "Cake") ("Paul" 100 "Chocolate")
                                                  ("Jen" 3 "Cookies") ("Diane" 1 "Pie"))) '("Haris" 2 "Banana"))
#f
>((replace (And (equal? "Name" "Haris") (And (equal? "Age" 1) (equal? "Food" "Cookies"))) '(("Name" "Age" "Food") ("Jasmin" 1 "Potato")
                                                  ("Haris" 2 "Banana") ("David" 1 "Cake") ("Paul" 100 "Chocolate")
                                                  ("Jen" 3 "Cookies") ("Diane" 1 "Pie"))) '("Haris" 2 "Banana"))
#f
|#
(define-syntax replace
  (syntax-rules ()
    [(replace (<operator> a ...) table)
     (lambda (tuple) (<operator> ((replace a table) tuple) ...))]
    [(replace atom table)
     (lambda (tuple) ((atom-replace atom)(attributes table) tuple))]))

#|Macro that returns only the tuples in table that satisfy predicate

>(where (Or (equal? "Age" 1) (And "LikesChocolate" (equal? "Name" "Haris"))) '(("Name" "Age" "Food" "LikesChocolate") ("Jasmin" 1 "Potato" #t) ("Haris" 2 "Banana" #t) 
                                                   ("Haris" 3 "Skittles" #f) ("David" 1 "Cake" #t) ("Paul" 100 "Chocolate" #f)
                                                   ("Jen" 3 "Cookies" #t) ("Diane" 1 "Pie" #f)))
'(("Name" "Age" "Food" "LikesChocolate")
  ("Jasmin" 1 "Potato" #t)
  ("Haris" 2 "Banana" #t)
  ("David" 1 "Cake" #t)
  ("Diane" 1 "Pie" #f))
|#
(define-syntax where
  (syntax-rules ()
    [(replace pred table)
     (satisfies (replace pred table) table)]))

#|Maro that returns the tuples in table ordered by ord in non-increasing order

>(order-by (string-length "Name") '(("Height" "Name") (4 "Jasmin") (5 "Haris") (1 "Jen") (50 "David") (500 "Karen")))
'(("Height" "Name")
  (4 "Jasmin")
  (5 "Haris")
  (50 "David")
  (500 "Karen")
  (1 "Jen"))
>(order-by (+ (string-length "Name") "Height") '(("Height" "Name" "Age") (6 "Jasmin" 18) (5 "Jennifer" 20) (5 "Haris" 20) (50 "David" 1)))
'(("Height" "Name" "Age")
  (50 "David" 1)
  (5 "Jennifer" 20)
  (6 "Jasmin" 18)
  (5 "Haris" 20))
|#
(define-syntax order-by
  (syntax-rules ()
    [(order-by ord table)
     (cons (attributes table) (sort (tuples table) #:key (replace ord table) >))]))


#|Macro that returns the results of a query on a table
  See database_tests.rkt for test cases
|#
(define-syntax SELECT
  (syntax-rules (FROM)
    [(SELECT (attrs ...) FROM table)
     (select (attrs ...) table)]
    [(SELECT (attrs ...) FROM [table name] ...)
     (SELECT (attrs ...) FROM (from (rename (list name ...) (unlimited table ...))))]

    [(SELECT atom FROM table)
     (if (equal? * atom) (select (attributes table) table) "invalid")]
    [(SELECT atom FROM [table name] ...)
    (if (equal? * atom) (from (rename (list name ...) (unlimited table ...))) "invalid")]
    
    [(SELECT (attrs ...) FROM table WHERE pred)
     (SELECT (attrs ...) FROM (where pred table))]
    [(SELECT (attrs ...) FROM [table name] ... WHERE pred)
     (SELECT (attrs ...) FROM (where pred (from (rename (list name ...) (unlimited table ...)))))]
    
    [(SELECT atom FROM table WHERE pred)
     (SELECT atom FROM (where pred table))]
    [(SELECT atom FROM [table name] ... WHERE pred)
     (SELECT atom FROM (where pred (from (rename (list name ...) (unlimited table ...)))))]
    
    [(SELECT (attrs ...) FROM table ORDER BY ord)
     (SELECT (attrs ...) FROM (order-by ord table))]
    [(SELECT (attrs ...) FROM [table name] ... ORDER BY ord)
     (SELECT (attrs ...) FROM (order-by ord (from (rename (list name ...) (unlimited table ...)))))]
    
    [(SELECT atom FROM table ORDER BY ord)
     (SELECT atom FROM (order-by ord table))]
    [(SELECT atom FROM [table name] ... ORDER BY ord)
     (SELECT atom FROM  (order-by ord (from (rename (list name ...) (unlimited table ...)))))]
    
    [(SELECT (attrs ...) FROM table WHERE pred ORDER BY ord)
     (SELECT (attrs ...) FROM (order-by ord table) WHERE pred)]
    [(SELECT (attrs ...) FROM [table name] ... WHERE pred ORDER BY ord)
     (SELECT (attrs ...) FROM (order-by ord (where pred (from (rename (list name ...) (unlimited table ...))))))]
    
    [(SELECT atom FROM table WHERE pred ORDER BY ord)
     (SELECT atom FROM (order-by ord (where pred table)))]
    [(SELECT atom FROM [table name] ... WHERE pred ORDER BY ord)
     (SELECT atom FROM (order-by ord (where pred (from (rename (list name ...) (unlimited table ...))))))]  
    ))