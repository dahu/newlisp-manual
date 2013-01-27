#!/usr/bin/newlisp
# version 0.1
# Barry Arthur, January 2013
#
# convert a .preasciidoc file to .asciidoc format:
# * generates cross reference IDs for all functions
# * renders functions in asciidoc syntax

(define (generate-ids ids)
  (dolist (id (parse ids ","))
    (println (string "anchor:" id "[]"))))

(define (admonishment adm)
  (let ((adms '(("!" "destructive") ("utf8" "utf8_capable"))))
    (string " link:#" ((ref (list adm '?) adms match 1) 1) "[" adm "]")))

(define (generate-func-format funcs admonishments)
  (print "\n==== [big red]#")
  (print (join (parse funcs ",") ", "))
  (if (!= admonishments "")
   (print (join (map admonishment (parse admonishments ",")) "")))
  (println "#"))

(define (generate-functions funcs admonishments)
      (generate-ids funcs)
      (generate-func-format funcs admonishments))

(set 'prefile (open ((main-args) 2) "read"))
(while (read-line prefile)
  (if (regex {^\s*function::(.*?)\[(.*?)\]} (current-line))
    (generate-functions $1 (or $2 ""))
    (println (current-line))))

(exit)
