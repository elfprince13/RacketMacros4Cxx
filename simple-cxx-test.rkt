#lang Cxx
; Notes: Syntax-local-bind-syntaxes should be able to dynamically introduce macros :)
; Still can't figure out how to
; (a) compare if two bits of syntax have the same marks
; (b) list currently known identifiers
; (c) access the current intdef-ctx at all!
; Idea: keep our own identifier dictionary when walking for CppWriter
; and use Source Location information to check which identifiers are original and which are introduced
; ---- better yet: syntax-original?
; CppWriter probably wants to use pretty print

(let* ((tmp 1) (j 2)) 
  (swap tmp j) 
  (print tmp)
  (dummy)
  (print j))

(let* ((tmp 1) (j tmp)) 
  (swap tmp j) 
  (print tmp) 
  (print j))

(let* ((i 1) (j 1) (tmp3 (swap i j)))
  (swap i j)
  (let* ((tmp 1)) 
    (print tmp)
    (print tmp3))
  (swap i j))