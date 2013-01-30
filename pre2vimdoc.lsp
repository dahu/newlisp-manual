#!/usr/bin/newlisp
# version 0.1
# Barry Arthur, January 2013
#
# convert a .preasciidoc file to vim doc format:
# * generates cross reference IDs for all functions
# * renders functions and code snippets in vimdoc syntax

(load "newlisp-function-list.lsp")
(load "pre2vimdoc_snippets.lsp")

(define (inline-replacements id) (replace {\*} id "star" 0))

(define (generate-ids ids)
  (apply string (map (fn (id) (string "*newlisp-function-" (inline-replacements id) "*" "\n" "*nl-" (inline-replacements id) "*" "\n")) (parse ids ","))))

(define (admonishment adm)
  (let ((adms '(("!" "destructive") ("utf8" "utf8_capable"))))
    (string "\n|nl-" ((ref (list adm '?) adms match 1) 1) "|")))

(define (generate-func-format funcs admonishments)
  (string "\nFUNCTION: "
    (join (parse funcs ",") ", ")
    "~"
    (if (!= admonishments "")
      (string "\n" (join (map admonishment (parse admonishments ",")) ""))
      "")))

(define (generate-functions funcs admonishments)
 (string (generate-ids funcs) (generate-func-format funcs admonishments)))

(define (markup-syntax-lines line)
  (replace {^\*syntax:\*} line "{syntax:}" 4))

(define (markup-underscore-refs line)
  (if (regex {__PRE2VIMDOC_SNIPPET_} line)
    line
    (replace {_(.+?)_} line (string "{" $1 "}") 0)))

(define (markup-backtick-refs line)
  (replace {`(.+?)`} line (if (find $1 newlisp-functions) (string "|nl-" (inline-replacements $1) "|") (string "{" $1 "}")) 0))

(define (markup-link-refs line)
  (replace {link:#.+?\[(.+?)\]} line (string "|nl-" (inline-replacements $1) "|") 0))

(define (markup line)
  (markup-syntax-lines
   (markup-underscore-refs
    (markup-backtick-refs
     (markup-link-refs line)))))

(define (extract-snippets text)
  (replace {\[source,.*?]\n----+\n(.*?)\n----+\n} text (Snippets:add $1) 4))

(define (insert-snippets text)
  (replace {__PRE2VIMDOC_SNIPPET_\d+} text (Snippets:get) 0))

; ;; remove all code snippets until after reflowing text with /bin/fmt
(setf pretext (extract-snippets (read-file ((main-args) 2))))

;; add vim-doc markup for function refs and literals
;; and reflow text to 68 cols wide
(set 'tmp-file (open "/tmp/pre2vimdoc.txt" "write"))
(write tmp-file (join (map markup (parse pretext "\n" 0)) "\n"))
(close tmp-file)
(setf posttext (join (exec "fmt -68 /tmp/pre2vimdoc.txt") "\n"))

;; generate function anchors (IDs) and
;; reinsert snippets now formatted for vim-doc
(println (insert-snippets (join
  (map (fn (ln)
    (if (regex {^\s*function::(.*?)\[(.*?)\]} ln)
      (generate-functions $1 (or $2 ""))
      ln))
    (parse posttext "\n" 0))
  "\n")))

(exit)
