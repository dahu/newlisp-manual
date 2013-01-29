(context 'Snippets)

(define (Snippets:add snippet)
  (push snippet Snippets:snips)
  (string "__PRE2VIMDOC_SNIPPET_" (length Snippets:snips) "\n"))

(define (Snippets:render snip)
  (replace "→" snip ";→" 0)
  (replace "^" snip "  " 2)
  (string ">\n" snip "\n<"))

(define (Snippets:get)
  (if (> (length Snippets:snips) 1)
    (Snippets:render (pop Snippets:snips -1))
    (Snippets:render (pop Snippets:snips))))

(context MAIN)

;(setf x "\nthis\n\n[source,newlisp]\n----\n(println \"Hi\")\n(exit)\n----\n\nhere\nand\n\n[source,newlisp]\n----\n(more)\n----\n\nthere\n")

; (replace {\[source,.*?]\n----+\n(.*?)\n----\n} (copy x) (Snippets:add $1) 4)
; (replace {__PRE2VIMDOC_SNIPPET_\d+} (copy y) (Snippets:get) 0)
