;; Git merge-conflict resolver.
;;
;; Operates directly on the currently focused document containing standard git
;; conflict markers:
;;
;;   <<<<<<< HEAD          (ours)
;;   ...ours lines...
;;   ||||||| base          (optional, diff3 style)
;;   ...base lines...
;;   =======
;;   ...theirs lines...
;;   >>>>>>> other-branch  (theirs)
;;
;; Provides navigation between conflicts, overlay highlighting of the ours/theirs
;; regions, and resolution actions (accept ours / theirs / both / none). Every
;; provided function is exposed as a `:typable-command`, e.g. `:conflict-next`.

(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require-builtin helix/core/text)
(require-builtin steel/process)

(provide conflict-highlight
         conflict-clear
         conflict-next
         conflict-prev
         conflict-accept-ours
         conflict-accept-theirs
         conflict-accept-both
         conflict-accept-none
         conflict-list
         conflict-files
         conflict-diff
         conflict-diff-close)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Theme scopes used to highlight the two sides of a conflict. `find_highlight`
;; falls back hierarchically (diff.plus -> diff), and a scope that resolves to
;; nothing is simply not drawn, so these are safe on any theme.
(define OURS-SCOPE "diff.plus")
(define THEIRS-SCOPE "diff.delta")

(define NS-OURS "git-conflict-ours")
(define NS-THEIRS "git-conflict-theirs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (str-trim s)
  (trim-end (trim-start s)))

(define (find-first pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) (car lst)]
    [else (find-first pred (cdr lst))]))

(define (list-last lst)
  (if (null? (cdr lst)) (car lst) (list-last (cdr lst))))

;; The rope backing the currently focused document.
(define (current-doc-rope)
  (let* ([focus (editor-focus)]
         [focus-doc-id (editor->doc-id focus)])
    (editor->text focus-doc-id)))

;; Current cursor line (0-based) in the given rope.
(define (cursor-line rope)
  (rope-char->line rope (hx.cx->pos)))

;; The text of line `i`, including its trailing newline if present.
(define (line-str rope i)
  (rope->string (rope->line rope i)))

;; The label following a `<<<<<<<` / `>>>>>>>` marker (the 7 marker chars + space).
(define (marker-label line)
  (str-trim (substring line 7 (string-length line))))

;; True when `line` opens with exactly seven `ch` characters followed by either
;; end-of-line or a space — i.e. a real git conflict marker, and not an 8+ run
;; or a heading underline that merely starts with the character.
(define (marker? line ch)
  (and (>= (string-length line) 7)
       (equal? (substring line 0 7) (make-string 7 ch))
       (let ([rest (substring line 7 (string-length line))])
         (or (equal? (str-trim rest) "")
             (char=? (string-ref rest 0) #\space)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A single parsed conflict block.
;;   start-line   : line index of the `<<<<<<<` marker
;;   base-line    : line index of the `|||||||` marker, or #false (2-way conflict)
;;   sep-line     : line index of the `=======` marker
;;   end-line     : line index of the `>>>>>>>` marker
;;   start-char   : char offset of the first char of the block
;;   last-char    : inclusive char offset of the last char of the block
;;   ours-label   : label from the `<<<<<<<` marker
;;   theirs-label : label from the `>>>>>>>` marker
(struct Conflict
        (start-line base-line sep-line end-line start-char last-char ours-label theirs-label))

(define (finalize-conflict rope partial end-line marker-line)
  (define start-line (hash-ref partial 'start))
  (define n (rope-len-lines rope))
  (define after (+ end-line 1))
  (define next-start
    (if (< after n) (rope-line->char rope after) (rope-len-chars rope)))
  (Conflict start-line
            (hash-ref partial 'base)
            (hash-ref partial 'sep)
            end-line
            (rope-line->char rope start-line)
            (- next-start 1)
            (hash-ref partial 'ours-label)
            (marker-label marker-line)))

;; Parse every well-formed conflict block in the rope, in document order.
(define (parse-conflicts rope)
  (define n (rope-len-lines rope))
  (let loop ([i 0] [partial #false] [acc '()])
    (if (>= i n)
        (reverse acc)
        (let ([line (line-str rope i)])
          (cond
            [(marker? line #\<)
             (loop (+ i 1)
                   (hash 'start i 'base #false 'sep #false 'ours-label (marker-label line))
                   acc)]
            [(and partial (marker? line #\|))
             (loop (+ i 1) (hash-insert partial 'base i) acc)]
            [(and partial (not (hash-ref partial 'sep)) (marker? line #\=))
             (loop (+ i 1) (hash-insert partial 'sep i) acc)]
            [(and partial (hash-ref partial 'sep) (marker? line #\>))
             (loop (+ i 1) #false (cons (finalize-conflict rope partial i line) acc))]
            [else (loop (+ i 1) partial acc)])))))

;; End of the "ours" content (exclusive) is the base marker in diff3 mode,
;; otherwise the separator.
(define (ours-content-end c)
  (or (Conflict-base-line c) (Conflict-sep-line c)))

;; The literal text of the ours / theirs sides (each line keeps its newline).
(define (ours-text rope c)
  (rope->string (rope->slice rope
                             (rope-line->char rope (+ (Conflict-start-line c) 1))
                             (rope-line->char rope (ours-content-end c)))))

(define (theirs-text rope c)
  (rope->string (rope->slice rope
                             (rope-line->char rope (+ (Conflict-sep-line c) 1))
                             (rope-line->char rope (Conflict-end-line c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Highlighting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (start . end) char pair for the ours side highlight (marker line through the
;; line before the separator).
(define (ours-range rope c)
  (cons (Conflict-start-char c)
        (rope-line->char rope (Conflict-sep-line c))))

;; (start . end) char pair for the theirs side highlight (separator line through
;; the `>>>>>>>` line).
(define (theirs-range rope c)
  (cons (rope-line->char rope (Conflict-sep-line c))
        (+ (Conflict-last-char c) 1)))

;; usize doc-ids of buffers with conflict highlighting active. A buffer stays
;; tracked until `conflict-clear`, so the document-changed hook keeps
;; re-highlighting it (e.g. after an undo restores a resolved conflict — script
;; highlights are not part of the undo history and would otherwise be lost).
(define *conflict-active-docs* (box '()))

(define (current-doc-uid)
  (doc-id->usize (editor->doc-id (editor-focus))))

(define (mark-conflict-active!)
  (define uid (current-doc-uid))
  (unless (member uid (unbox *conflict-active-docs*))
    (set-box! *conflict-active-docs* (cons uid (unbox *conflict-active-docs*)))))

(define (unmark-conflict-active!)
  (define uid (current-doc-uid))
  (set-box! *conflict-active-docs*
            (filter (lambda (x) (not (equal? x uid))) (unbox *conflict-active-docs*))))

;; Recompute and apply overlay highlights for every conflict in the buffer.
;; Clears the highlights entirely when no conflicts remain.
(define (refresh-conflict-highlights)
  (mark-conflict-active!)
  (define rope (current-doc-rope))
  (define conflicts (parse-conflicts rope))
  (if (null? conflicts)
      (begin
        (clear-document-highlights! NS-OURS)
        (clear-document-highlights! NS-THEIRS))
      (begin
        (set-document-highlights! NS-OURS
                                  (map (lambda (c) (ours-range rope c)) conflicts)
                                  OURS-SCOPE)
        (set-document-highlights! NS-THEIRS
                                  (map (lambda (c) (theirs-range rope c)) conflicts)
                                  THEIRS-SCOPE))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move the primary selection (a zero-width cursor) to `ch`.
(define (goto-char! ch)
  (helix.static.set-current-selection-object!
   (helix.static.range->selection (helix.static.range ch ch))))

;; The conflict the cursor is inside, or the first one starting at/after the
;; cursor line. #false when the cursor is past the last conflict.
(define (conflict-at-cursor rope conflicts)
  (define line (cursor-line rope))
  (or (find-first (lambda (c)
                    (and (>= line (Conflict-start-line c))
                         (<= line (Conflict-end-line c))))
                  conflicts)
      (find-first (lambda (c) (>= (Conflict-start-line c) line)) conflicts)))

;;@doc
;; Jump to the next conflict below the cursor (wraps to the first).
(define (conflict-next)
  (define rope (current-doc-rope))
  (define conflicts (parse-conflicts rope))
  (unless (null? conflicts)
    (define line (cursor-line rope))
    (define target
      (or (find-first (lambda (c) (> (Conflict-start-line c) line)) conflicts)
          (car conflicts)))
    (goto-char! (Conflict-start-char target)))
  (refresh-conflict-highlights))

;;@doc
;; Jump to the previous conflict above the cursor (wraps to the last).
(define (conflict-prev)
  (define rope (current-doc-rope))
  (define conflicts (parse-conflicts rope))
  (unless (null? conflicts)
    (define line (cursor-line rope))
    (define target
      (or (find-first (lambda (c) (< (Conflict-start-line c) line))
                      (reverse conflicts))
          (list-last conflicts)))
    (goto-char! (Conflict-start-char target)))
  (refresh-conflict-highlights))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Resolution ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Select the half-open char span [start, end) and replace it with `str`.
;; Helix ranges are half-open (`range->to` is exclusive), so `end` must be one
;; past the last char to replace — otherwise the final char (here the newline
;; after `>>>>>>>`) is left behind, leaving a stray blank line.
(define (replace-char-range! start end str)
  (helix.static.set-current-selection-object!
   (helix.static.range->selection (helix.static.range start end)))
  (helix.static.replace-selection-with str))

;; Resolve the conflict under the cursor by replacing the whole block with
;; `resolved` (a function of the rope + conflict producing the replacement text).
(define (resolve-with resolved)
  (define rope (current-doc-rope))
  (define conflicts (parse-conflicts rope))
  (define target (conflict-at-cursor rope conflicts))
  (when target
    (goto-char! (Conflict-start-char target))
    (replace-char-range! (Conflict-start-char target)
                         (+ (Conflict-last-char target) 1)
                         (resolved rope target))
    (refresh-conflict-highlights)))

;;@doc
;; Resolve the current conflict keeping only our (HEAD) side.
(define (conflict-accept-ours)
  (resolve-with ours-text))

;;@doc
;; Resolve the current conflict keeping only their (incoming) side.
(define (conflict-accept-theirs)
  (resolve-with theirs-text))

;;@doc
;; Resolve the current conflict keeping both sides (ours then theirs).
(define (conflict-accept-both)
  (resolve-with (lambda (rope c) (string-append (ours-text rope c) (theirs-text rope c)))))

;;@doc
;; Resolve the current conflict by discarding both sides (delete the block).
(define (conflict-accept-none)
  (resolve-with (lambda (rope c) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Highlight commands ;;;;;;;;;;;;;;;;;;;;;;;;;

;;@doc
;; Highlight every conflict in the current buffer.
(define (conflict-highlight)
  (refresh-conflict-highlights))

;;@doc
;; Remove conflict highlighting from the current buffer and stop tracking it.
(define (conflict-clear)
  (unmark-conflict-active!)
  (clear-document-highlights! NS-OURS)
  (clear-document-highlights! NS-THEIRS))

;; Re-apply conflict highlights after any edit to a tracked buffer. Undo/redo
;; restore document text but not script highlights, so without this an undo that
;; brings a resolved conflict back would leave it unhighlighted.
(define (conflict-doc-changed-hook doc-id old-text)
  (when (member (current-doc-uid) (unbox *conflict-active-docs*))
    (refresh-conflict-highlights)))

(register-hook 'document-changed conflict-doc-changed-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pickers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (conflict->label c)
  (string-append "L"
                 (number->string (+ 1 (Conflict-start-line c)))
                 "  "
                 (Conflict-ours-label c)
                 " <> "
                 (Conflict-theirs-label c)))

;; Callback for the conflict-list/conflict-files pickers: enter diff view for
;; whatever conflict is now under the cursor. Delayed so it runs after the
;; picker component has finished closing.
(define (enter-diff-view-after-select)
  (enqueue-thread-local-callback-with-delay 10 conflict-diff))

;;@doc
;; Open a native picker over the conflicts in the current buffer. The preview
;; shows the file scrolled to the conflict; selecting one jumps to it and
;; opens the ours/working/theirs diff view (unless already in diff view).
(define (conflict-list)
  (define rope (current-doc-rope))
  (define conflicts (parse-conflicts rope))
  (define path (current-file-path))
  (when (and path (not (null? conflicts)))
    (refresh-conflict-highlights)
    (push-component!
     (#%location-picker
      (map conflict->label conflicts)
      (map (lambda (c) path) conflicts)
      (map Conflict-start-line conflicts)
      (map Conflict-end-line conflicts)
      enter-diff-view-after-select))))

;; Capture stdout of a git command (run in `dir`, or the editor cwd when #false).
;; Returns "" on failure instead of raising.
(define (git-capture dir args)
  (define cmd (command "git" args))
  (when dir (set-current-dir! cmd dir))
  (set-piped-stdout! cmd)
  (with-handler (lambda (_) "")
                (Ok->value (wait->stdout (Ok->value (spawn-process cmd))))))

;; Capture stdout of a git invocation as a string (editor cwd).
(define (git-output args)
  (git-capture #false args))

;;@doc
;; Open a picker over every file in the repo with unresolved conflicts, previewing
;; the whole file; selecting one opens it and enters the ours/working/theirs diff
;; view (unless already in diff view).
(define (conflict-files)
  (define files
    (filter (lambda (s) (not (equal? s "")))
            (map str-trim
                 (split-many (git-output (list "diff" "--name-only" "--diff-filter=U")) "\n"))))
  (unless (null? files)
    (push-component!
     ;; #%exp-picker treats items as file paths, previews the whole file, and
     ;; opens the selection itself; the callback (no args) runs post-open.
     (#%exp-picker files enter-diff-view-after-select))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 3-way split view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Namespace for the diff-view line highlights (reuses OURS-SCOPE / THEIRS-SCOPE).
(define NS-DIFF "git-conflict-diff")

;; Paths of the side buffers opened by the last `conflict-diff`, so
;; `conflict-diff-close` can tear them down. A non-empty box is also the
;; "currently in diff view" signal used by `conflict-diff` to no-op on repeat.
(define *conflict-diff-files* (box '()))

(define (in-diff-view?)
  (not (null? (unbox *conflict-diff-files*))))

;; Absolute path of the currently focused file, or #false for a scratch buffer.
(define (current-file-path)
  (define p (editor-document->path (editor->doc-id (editor-focus))))
  (and p (to-string p)))

(define (path-parts p) (split-many p "/"))
(define (path-basename p) (list-last (path-parts p)))
(define (join-slash parts)
  (cond
    [(null? parts) ""]
    [(null? (cdr parts)) (car parts)]
    [else (string-append (car parts) "/" (join-slash (cdr parts)))]))
(define (path-parent p)
  (define but-last (reverse (cdr (reverse (path-parts p)))))
  (if (null? but-last) "." (join-slash but-last)))

;; Content of a merge stage (1=base, 2=ours, 3=theirs) for the given file.
;; `:N:./name` resolves the path relative to the file's own directory.
(define (git-stage dir basename stage)
  (git-capture dir (list "show" (string-append ":" (number->string stage) ":./" basename))))

(define (ensure-dir d)
  (unless (path-exists? d) (create-directory! d)))

;; Write `content` to /tmp/hx-conflict/<side>/<basename> (extension preserved so
;; the buffer gets the right language) and return the path.
(define (write-side-file side basename content)
  (define root "/tmp/hx-conflict")
  (define dir (string-append root "/" side))
  (ensure-dir root)
  (ensure-dir dir)
  (define file (string-append dir "/" basename))
  ;; open-output-file errors if the file exists (it won't truncate), so a repeat
  ;; diff of the same file would fail with "io: file exists" — remove it first.
  (when (path-exists? file) (delete-file! file))
  (define port (open-output-file file))
  (write-string content port)
  (close-output-port port)
  file)

;; 1-based line numbers on the "+" side of a `git diff -U0` hunk header
;; (e.g. "@@ -1,0 +2,3 @@" -> (2 3 4)).
(define (hunk-plus-lines header)
  (define plus (find-first (lambda (t) (starts-with? t "+")) (split-many header " ")))
  (if (not plus)
      '()
      (let* ([spec (substring plus 1 (string-length plus))]
             [parts (split-many spec ",")]
             [start (string->number (car parts))]
             [count (if (null? (cdr parts)) 1 (string->number (cadr parts)))])
        (if (or (not start) (not count) (= count 0))
            '()
            (map (lambda (k) (+ start k)) (range 0 count))))))

;; Lines in `other-file` that differ from `base-file`, via git's -U0 diff.
(define (changed-lines base-file other-file)
  (define out (git-capture #false (list "diff" "--no-index" "-U0" "--" base-file other-file)))
  (flatten (map hunk-plus-lines
                (filter (lambda (l) (starts-with? l "@@")) (split-many out "\n")))))

;; Highlight the given 1-based line numbers on the CURRENT document.
(define (highlight-lines linenos scope)
  (define rope (current-doc-rope))
  (define n (rope-len-lines rope))
  (set-document-highlights!
   NS-DIFF
   (map (lambda (ln)
          (define i (- ln 1))
          (cons (rope-line->char rope i)
                (if (< (+ i 1) n) (rope-line->char rope (+ i 1)) (rope-len-chars rope))))
        (filter (lambda (ln) (and (>= ln 1) (<= ln n))) linenos))
   scope))

;;@doc
;; Open a 3-way split for the conflicted file under the cursor:
;; ours (HEAD) | working file | theirs (incoming), with lines that differ from
;; the merge base highlighted in the ours/theirs panes. The working (center)
;; pane keeps focus, so the :conflict-accept-* commands still apply there.
;; No-ops if already in diff view — use :conflict-diff-close first to switch files.
(define (conflict-diff)
  (define path (current-file-path))
  (cond
    [(in-diff-view?) void]
    [(not path) "conflict-diff: current buffer has no file"]
    [else
     (define dir (path-parent path))
     (define name (path-basename path))
     (define ours (git-stage dir name 2))
     (define theirs (git-stage dir name 3))
     (define base (git-stage dir name 1))
     (if (and (equal? ours "") (equal? theirs ""))
         "conflict-diff: no merge-conflict stages for this file"
         (let ([ours-file (write-side-file "ours" name ours)]
               [theirs-file (write-side-file "theirs" name theirs)]
               [base-file (write-side-file "base" name base)])
           (define ours-changed (changed-lines base-file ours-file))
           (define theirs-changed (changed-lines base-file theirs-file))
           (set-box! *conflict-diff-files* (list ours-file theirs-file))
           ;; Build ours | working | theirs, ending with focus on working.
           (helix.vsplit-new)
           (helix.open ours-file)
           (highlight-lines ours-changed OURS-SCOPE)
           (helix.static.swap_view_left)
           (helix.static.jump_view_right)
           (helix.vsplit-new)
           (helix.open theirs-file)
           (highlight-lines theirs-changed THEIRS-SCOPE)
           (helix.static.jump_view_left)
           ;; Highlight the conflict regions in the (now focused) working buffer.
           (refresh-conflict-highlights)
           void))]))

;;@doc
;; Close the ours/theirs side buffers opened by `conflict-diff`, leaving the
;; working file.
(define (conflict-diff-close)
  (for-each (lambda (f)
              (helix.open f)
              (helix.buffer-close!))
            (unbox *conflict-diff-files*))
  (set-box! *conflict-diff-files* '()))
