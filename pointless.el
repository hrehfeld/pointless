;; -*- lexical-binding: t; -*-


(defface pointless-target `((t . (:foreground "white" :background "#a00"))) "Generic face for a pointless jump target.")
(defface pointless-target-1 `((t . (:inherit pointless-target))) "First face for pointless jump targets.")
(defface pointless-target-2 `((t . (:background "dark green" :inherit pointless-target))) "Second face for pointless jump targets.")
(defface pointless-target-3 `((t . (:background "dark blue" :inherit pointless-target))) "Second face for pointless jump targets.")
(defface pointless-target-4 `((t . (:background "dark pink" :inherit pointless-target))) "Second face for pointless jump targets.")
(defface pointless-face-further `((t . (:background "#aaa" :inherit pointless-target))) "Generic face for a pointless jump target when no better face is available.")


(defmacro pointless-save-window-start-and-mark-and-excursion (&rest body)
  (let ((window-start (cl-gensym 'pointless-window-start))
        (window (cl-gensym 'pointless-window))
        (point (cl-gensym 'pointless-point))
        (result (cl-gensym 'pointless-result)))
    `(let ((,window-start (window-start))
           (,window (selected-window))
           (,point (point))
           (,result (save-mark-and-excursion ,@body)))
       (set-window-start ,window ,window-start)
       (cl-assert (= (point) ,point) t)
       ;;(goto-char ,point)
       ,result
       )))


(defun pointless--make-overlay (start char &optional face)
  "Make a single char overlay and return it."

  )

(defun pointless-target-show (position length str)
  ;;(cl-assert (eq (get-text-property position 'display) nil) "Position already has a display text property!")
  ;;(cl-assert (eq (get-text-property position 'face) nil) "Position already has a face text property!")
  (let* ((overlay (make-overlay position (+ position length)  nil t nil))
         )
    (overlay-put overlay
                 (if (eq position (point-max)) 'after-string 'display)
                 str)
    (overlay-put overlay 'read-only t)
    (list overlay)
    ))

(defun pointless-target-hide (overlays)
  (cl-assert (listp overlays))
  (mapc #'delete-overlay overlays))

(defvar pointless-keys '(("asdfghjkl;'" . ?h) ("qwertyuiop" . ?y) ("zxcvbnm,." . ?b) ("1234567890" . ?6)) "A list of strings of keys that are used as jump keys.

pointless will use these list to build keys relative to point, each list element (a list of keys) will be assigned to one category of jumps.")
;;(setq pointless-keys '(("asdfghjkl;'" . ?h) ("qwertyuiop" . ?y) ("zxcvbnm,." . ?b) ("1234567890" . ?6)))

(defvar pointless-faces '(pointless-target-1 pointless-target-2
                                             pointless-target-3
                                             pointless-target-4)
  "A list of faces for each pointless keyset")

(defun pointless--funcall-arg-maybe (fun arg)
  "Call `fun' with index if it takes more than zero args."
  (let ((arity (func-arity fun)))
    (apply fun
           (when (or (eq arity 'many) (and (consp arity) (>= (cdr arity) 1)))
             (list arg)))))

(defun pointless-call-with-keyset (key-set fun)
  (let* ((keys (string-to-list (car key-set)))
           (after-point-key (cdr key-set)))
      (funcall fun keys after-point-key)))


(cl-defun pointless--collect-targets-iteratively (create-targets-fun
                                                  &key max-num-candidates start-position-fn include-start-position
                                                  (region-begin (window-start))
                                                  (region-end (window-end)))
  (pointless-save-window-start-and-mark-and-excursion
   (when start-position-fn
     (funcall start-position-fn))
   (let ((start-position (point)))
     (cl-flet ((within-region ()
                              (and (<= region-begin (point))
                                   (<= (point-min) (point))
                                   (< (point) region-end)
                                   (< (point) (point-max))
                                   )))
       (let ((candidates (cl-loop for istep from 0
                                  while (and (within-region)
                                             (or (eq max-num-candidates nil)
                                                 (< istep max-num-candidates))
                                             )
                                  do (pointless--funcall-arg-maybe create-targets-fun istep)
                                  ;; filter out candidates that are outside region
                                  ;; e.g. with a regex search, condition might be true before the movement
                                  if (within-region)
                                  collect (point)
                                  )))
         (if include-start-position
             (cons start-position candidates)
           candidates))))))


(defconst pointless-base-overlay-priority 50)

(defun pointless--compose-overwriting-overlay (overlayed-str buffer-str)
  (let ((buffer-str-width (string-width buffer-str))
        (overlayed-str-width (string-width overlayed-str)))
    (cond ((s-starts-with-p "\n" buffer-str)
           (concat overlayed-str "\n"))
          ;; add padding for wide-width character
          ((> buffer-str-width overlayed-str-width)
           (funcall #'concat overlayed-str (s-repeat (- buffer-str-width overlayed-str-width) " ")))
          (t
           overlayed-str))))

(defun pointless-sort-candidates-close-to-point (candidates)
  (let ((point (point)))
    (cl-sort candidates (lambda (a b)
                          (< (abs (- point a))
                             (abs (- point b)))))))


(defun pointless--show-keys (keys-faces position next-position &optional compose-fn)
  (let* ((num-keys (length keys-faces))
         (ilast-key (1- num-keys))
         (str (apply #'concat (mapcar (-compose #'char-to-string #'car) keys-faces)))
         (str (cl-loop for iface from 0
                       for face in (mapcar #'cdr keys-faces)
                       do (progn
                            (cl-assert (symbolp face) face)
                            (put-text-property iface (1+ iface) 'face face str))
                       finally return str))
         (overlay-length num-keys)
         (num-non-eol-chars (save-excursion
                              (goto-char position)
                              (looking-at (format "[^\n]\\{0,%i\\}" overlay-length))
                              (- (match-end 0) (match-beginning 0))))
         (overlay-length (max 1 (min num-non-eol-chars overlay-length)))
         (overlay-length (if next-position
                             (min (- next-position position) overlay-length)
                           overlay-length))
         (old-str (if (< position (point-max))
                      (buffer-substring position (+ position overlay-length))
                    ""))
         (str (funcall (or compose-fn #'concat) str old-str)))
      (pointless-target-show position overlay-length str)))

(defun pointless--tree-position-p (node) (number-or-marker-p node))

(defun pointless--get-position-prefix-keys (key-face-nodes-node &optional prefix-keys)
  (pcase-let*
      ((`(,key ,face ,tree-or-position) key-face-nodes-node))
    (let ((prefix-keys (cons (cons key face) prefix-keys)))
      ;;(message "%S %S --- %S" key face tree-or-position)
      (if (pointless--tree-position-p tree-or-position)
          ;; leaf nodes show and return positions & prefix-keys (as well as faces)
          (list (list tree-or-position (seq-reverse prefix-keys)))
        ;; inner nodes just collect prefixes and results
        (seq-mapcat (lambda (el) (pointless--get-position-prefix-keys el prefix-keys))
                    tree-or-position)
        )))
  )

(defun pointless--create-overlays (keys-faces-positions-nodes &optional compose-fn)
  "Create overlays from the `KEYS-FACES-POSITIONS-NODES' and return an
list like `((POSITION KEY-SEQUENCE (OVERLAY ...) (OVERLAY ...) ...) ...)'"
  ;;(message "pointless--create-overlays %S" keys-faces-positions-nodes)
  (let* ((positions-prefix-keys (seq-mapcat #'pointless--get-position-prefix-keys keys-faces-positions-nodes))
         (positions-prefix-keys (cl-sort positions-prefix-keys #'< :key #'car))
         )
    (apply #'seq-concatenate 'list
             (cl-loop
              for iposition from 0
              for (position prefix-keys) in positions-prefix-keys
              ;; only place overlay on a single char, so we only take buffer-substring of length 1
              collect
              (let* ((next-position (car (nth (1+ iposition) positions-prefix-keys))))
                (pointless--show-keys prefix-keys position next-position compose-fn))))))

;; (pointless--create-overlays
;;  (pointless-make-jump-keys-unidirectional (string-to-list "123") (mapcar (lambda (i) (+ (window-start) (* 10 i)))
;;                                                                          (seq-take '(0 1 2 3 4 5 6 7 8 9) 1))))

(defvar pointless-on-jump-hook nil "A list of functions that are
called right after the user presses a key that jumps somewhere.

Each function is called with KEY and POSITION as its arguments.")

(defvar pointless-make-jump-keys-unidirectional-partition-function
  #'pointless-partition-values-top-down
    "The default function that is used by
`pointless-make-jump-keys-unidirectional' to partition values into
number-of-keys subsets at each level of the tree.")


(defvar pointless-compose-overlay-default-function #'pointless--compose-overwriting-overlay "Default function to compose overlays with the overlayed buffer content.")
(defvar pointless-compose-overlay-function-alist nil "Define functions to compose overlays with the overlayed buffer content in an alist per command.

Should be a list of `cons' cells `(COMMAND . COMPOSE-OVERLAY-FUNCTION)'.")

(defvar pointless-partition-candidates-default-function #'pointless-partition-values-top-down "Default function to partition candidates")
(defvar pointless-partition-candidates-function-alist nil "Define functions to partition candidates in an alist per command.

Should be a list of `cons' cells `(COMMAND . PARTITION-CANDIDATES-FUNCTION)'.")

(defun pointless-partition-candidates-function-default (command-name &optional partition-fn)
  "Convenience function to retrieve"
  (or partition-fn
      (alist-get command-name pointless-partition-candidates-function-alist)
      pointless-partition-candidates-default-function))


(defvar pointless-sort-candidates-default-function nil "Default function to sort candidates")
(defvar pointless-sort-candidates-function-alist nil "Define functions to sort candidates in an alist per command.

Should be a list of `cons' cells like `(COMMAND . SORT-CANDIDATES-FUNCTION)'.")


(defvar pointless-keysets-alist nil "Define keysets in an alist per command.

Should either be a list of `cons' cells `(LIST-OR-STRING-OF-KEYS . MIDDLE-KEY)' or a number to use a keyset in `pointless-keys'")


(defun pointless-sort-candidates-function-default (command-name &optional partition-fn)
  (or partition-fn
      (alist-get command-name pointless-sort-candidates-function-alist)
      pointless-sort-candidates-default-function))

;;(pointless-partition-candidates-function-default 'pointless-jump-word-beginning)

(defun pointless-keyset-default (command-name &optional keyset)
  (let ((keyset (or keyset
                    (alist-get command-name pointless-keysets-alist)
                    ;; just take the first keyset
                    (car (pointless-keys)))))
    ;; if keyset is a number, interpret as index into pointless-keys
    (if (numberp keyset)
        (nth keyset (pointless-keys))
      keyset)))



(defun pointless--do-jump-no-user-options (keys-faces-positions-nodes compose-fn)
  "`COMMAND-NAME' is the name of the calling command."
  (unless keys-faces-positions-nodes
    (user-error "No valid jump targets."))
  (cl-assert (seqp keys-faces-positions-nodes) t)

  (setq inhibit-quit t)
  (let (
        ;; save overlays globally so we can easily remove them on quit
        keys-overlays)
    (cl-flet ((check-overlays (keys-overlays)
                              (mapc (lambda (keys-overlays)
                                      (cl-assert (number-or-marker-p (car keys-overlays)))
                                      (cl-assert (cl-every #'number-or-marker-p (cadr keys-overlays)))
                                      (cl-assert (cl-every #'listp (caddr keys-overlays)))
                                      (mapc (lambda (overlays) (cl-assert (cl-every #'overlayp overlays) overlays))
                                            (caddr keys-overlays)))
                                    keys-overlays)))
      (with-local-quit
        (cl-labels
            ((read-level (ilevel prefix-keys nodes)
                         ;;(message "ilevel: %S,	prefix-keys: %S,	tree: %S" ilevel prefix-keys nodes)
                         (setq keys-overlays (pointless--create-overlays nodes compose-fn))
                         ;;(message "%S" keys-overlays)
                         ;;(check-overlays keys-overlays)

                         (let* ((keys (mapcar #'car nodes))
                                (key (read-char-choice-with-read-key (format "Jump to target: (%s) [%i, %i, %i, %i]"
                                                                             (s-join "" (--map (char-to-string it) keys))
                                                                             (point) (window-start) (window-end)
                                                                             (overlay-start (car (last keys-overlays))))
                                                                     keys))
                                (ikey (seq-position keys key))
                                ;;(overlays (seq-elt overlays ikey))
                                ;;(pos (overlay-start (car overlays)))
                                (prefix-keys (cons key prefix-keys)))
                           (pointless-target-hide keys-overlays)

                           (let ((chosen-item (caddr (nth ikey nodes))))
                             (cl-assert chosen-item)
                             ;;(message "pointless-do-jump before next")
                             (if (pointless--tree-position-p chosen-item)
                                 (progn
                                   (push-mark)
                                   (goto-char chosen-item))
                               (read-level (1+ ilevel) prefix-keys chosen-item)))
                           )))
          (read-level 0 nil keys-faces-positions-nodes))
        )
      )
    (pointless-target-hide keys-overlays))
  (setq inhibit-quit nil))


(defun pointless-do-jump (command-name keys-faces-positions-nodes &optional compose-fn)
  "`COMMAND-NAME' is the name of the calling command."
  (let ((compose-fn (or compose-fn (assq command-name pointless-compose-overlay-function-alist) pointless-compose-overlay-default-function))

        )
    (pointless--do-jump-no-user-options keys-faces-positions-nodes compose-fn)
    )
  )


(defun pointless-jump-chars-words-lines ()
  (interactive)
  (pointless-do-jump pointless-keys
                     (pointless-make-targets-function-backward-forward #'backward-char #'forward-char)
                     (pointless-make-targets-function-backward-forward #'backward-word #'forward-word)
                     (pointless-make-targets-function-backward-forward (lambda ()
                                                                              (previous-line)
                                                                              (beginning-of-line))
                                                                            (lambda ()
                                                                              (next-line)
                                                                              (beginning-of-line)))
                     (pointless-make-targets-function-backward-forward (lambda ()
                                                                              (previous-line)
                                                                              (end-of-line))
                                                                            (lambda ()
                                                                              (next-line)
                                                                              (end-of-line)))
                     )
  )

(defun pointless-keys ()
  "Retrieve the keysets from `pointless-keys', but map each keyset given as a string to a list of chars first."
  (mapcar (lambda (e)
            (let ((keys (car e))
                  (center-char (cdr e)))
              (list (string-to-list keys) center-char)))
          pointless-keys)
  )

(defun pointless-get-keys-unidirectional ()
  (car (pointless-keys)))

(defun pointless-partition-values-top-down (values num-keys)
  "Use highest number of keys `NUM-KEYS' necessary and return subsets of `VALUES' that are filled as much as possible in the beginning."
  (seq-partition values (ceiling (length values) num-keys)))

(defun pointless-partition-values-quick-first (values num-keys &optional num-quick-keys)
  ""
  ;; (seq-partition values (1+ (/ (length values) 2)))
  (let* ((num-quick-keys (or num-quick-keys (floor num-keys 2)))
         (res
         (append (mapcar #'list (seq-take values (1- num-keys)))
                 (list (nthcdr (1- num-keys ) values)))))
    (message "quick-first: %S" res)
    res)
  )
;;(setq pointless-make-jump-keys-unidirectional-partition-function #'pointless-partition-values-quick-first)

(defun pointless-make-jump-keys-unidirectional (keys positions partition-fn)
  (cl-assert (functionp partition-fn) t)
  (let* ((keys (if (stringp keys) (string-to-list keys) keys))
         (num-keys (length keys))
         (keys-faces-positions-nodes
          (cl-labels
              ((descend (ilevel values)
                        (let* ((num-values (length values))
                               (face (or (nth ilevel pointless-faces) 'pointless-face-further))
                               (faces (-repeat num-values face)))
                          ;;(message "level: %S,	num-keys: %S,	num-values: %S,	keys: %S" ilevel num-keys num-values keys)
                          (let ((res (if (<= num-values num-keys)
                                         values
                                       (let ((position-partitions (funcall partition-fn values num-keys)))
                                         (seq-map (lambda (positions)
                                                    (if (length> positions 1)
                                                        (descend (1+ ilevel) positions)
                                                      (let ((position (car positions)))
                                                        (cl-assert (number-or-marker-p position) t)
                                                        position)))
                                                  position-partitions)))))
                            (-zip-lists keys faces res)))))
            (descend 0 positions))))
    (cl-labels ((count-leafs (keys-faces-positions-nodes)
                         (let ((values (caddr keys-faces-positions-nodes)))
                           (if (pointless--tree-position-p values)
                               1
                             (cl-reduce #'+ (mapcar #'count-leafs values))))
                         ))
      ;;(cl-check-type keys-faces-positions-nodes (list-of number))
      ;; (message "%S" keys-faces-positions-nodes)
      ;; (message "%S" positions)
      (cl-assert (length= positions (cl-reduce #'+ (mapcar #'count-leafs keys-faces-positions-nodes))) nil "List lengths: %i %i" (count-leafs keys-faces-positions-nodes) (length positions)))
    keys-faces-positions-nodes)
  )

;; (pointless-make-jump-keys-unidirectional (string-to-list "123") (mapcar (lambda (i) (+ (window-start) (* 10 i)))
;;                                                                         '(0 1 2 3 4 5 6 7 8 9)))


(defvar pointless-moveto-mark-max-candidates 20
  "Maximum number of candidates when using `pointless-moveto-mark'.")

(defun pointless-moveto-mark ()
  (interactive)
  (let* ((positions (seq-uniq (--filter (ignore-errors
                                          (and (>= it (window-start)) (< it (window-end))))
                                        mark-ring)))
         (keys (car (pointless-get-keys-unidirectional)))
         (faces (-repeat (length keys) 'pointless-target)))
    (pointless-do-jump (pointless-make-jump-keys-unidirectional keys (seq-take positions pointless-moveto-mark-max-candidates))))
)

(defun pointless-make-targets-backward-forward (keyset face backward-f forward-f)
  (cl-destructuring-bind (keys after-point-key) keyset
    (let* ((num-steps-backward (seq-position keys after-point-key))
           (num-steps-forward (- (length keys) num-steps-backward)))
      (cl-assert (length= keys (+ num-steps-backward num-steps-forward)))
      (let ((backward-positions
             (pointless--collect-targets-iteratively backward-f :max-num-candidates num-steps-backward))
            (forward-positions
             (pointless--collect-targets-iteratively forward-f :max-num-candidates num-steps-forward)))
        (let* ((key-start-offset (- num-steps-backward (length backward-positions)))
               (positions (append (nreverse backward-positions) forward-positions))
               (faces (-repeat (length positions) face))
               (keys (seq-drop keys key-start-offset)))
          (pointless-make-jump-keys-unidirectional keys positions)
          )
        )
      )))

(defun pointless-moveto-end-of-line ()
  (interactive)
  (pointless-do-jump
   (pointless-make-targets-backward-forward
    (car (pointless-keys))
    'pointless-target
    ;; move up a line
    (lambda (istep) (end-of-line 0))
    (lambda (istep)
      (if (= istep 0)
          ;; just go to end of line
          (end-of-line)
        ;; move down a line
        (end-of-line 2))))
  ))




(defmacro pointless-defmove-unidirectional (name &rest positions-form)
  (let ((positions (gensym)))
    `(defun ,name ()
       (interactive)
       (let* ((,positions (progn ,@positions-form)))
         (pointless-do-jump
          ',name
           (-zip
            ;;keys
            (car (pointless-get-keys-unidirectional))
            ;;faces
            (-repeat (length ,positions) 'pointless-target)
            ,positions)))
       ))
  )

(defun pointless--normalize-keyword-arguments-with-rest (keywords args)
  (let ((args (seq-copy args))
        keyword-args
        rest-args)
    (cl-loop while args
             do (let ((arg (pop args)))
                  (if (keywordp arg)
                      (progn
                        (cl-assert (member arg keywords) t)
                        (let ((value (pop args)))
                          (push arg keyword-args)
                          (push value keyword-args)))
                    (push arg rest-args))))
    (mapcar #'nreverse (list rest-args keyword-args))
    )
  )


;; &key aren't working with &rest for some reason
(cl-defmacro pointless-defjump-unidirectional (name &rest args)
  "Define a jump command conveniently.

`NAME' is the name of the command to be defined.

`ARGS' consists of KEYWORDS and `&rest'.
`&rest' is a function body that returns a list of candidate positions.

`:MAX-NUM-CANDIDATES' is the maximum number of candidates. This
is applied *after* sorting.

`:KEYSET' is the set of keys to be used.

`:SORT-FN' is a function that is called with the list of
candidates as the single argument and returns the list sorted.

`:PARTITION-FN' is a candidate partition function handed to
`pointless-make-jump-keys-unidirectional'.
"
  (cl-destructuring-bind (candidates-forms keyword-args)
      (pointless--normalize-keyword-arguments-with-rest '(:sort-fn :partition-fn :max-num-candidates :keyset) args)
    ;;(message "%S" keyword-args)
    (let ((positions (gensym))
          (max-num-candidates (gensym))
          (sort-fn (gensym 'sort-fn))
          (keyset (gensym)))
      `(defun ,name ()
         (interactive)
         (let* ((,sort-fn (pointless-sort-candidates-function-default (quote ,name) ,(plist-get keyword-args :sort-fn)))
                (,max-num-candidates ,(plist-get keyword-args :max-num-candidates))
                (,keyset (pointless-keyset-default ',name ,(plist-get keyword-args :keyset)))
                (,positions (pointless-save-window-start-and-mark-and-excursion ,@candidates-forms))
                (,positions (if ,sort-fn (funcall ,sort-fn ,positions) ,positions))
                (,positions (if ,max-num-candidates (seq-take ,positions ,max-num-candidates) ,positions))
                )
           (pointless-do-jump
            ',name
            (pointless-make-jump-keys-unidirectional
             ;;keys
             (car ,keyset)
             ;;positions
             ,positions
             ;;partition-fn
             (pointless-partition-candidates-function-default ',name ,(plist-get keyword-args :partition-fn)))))
         )))
  )

(let ((print-length 9999)
      (print-level 99)
      (eval-expression-print-level 99))
  (prin1 (macroexpand-1 '(pointless-defjump-unidirectional pointless-jump-beginning-of-line-text
                                  (progn (cons (point)
                                               (pointless--collect-targets-iteratively
                                                (apply-partially #'beginning-of-line-text 2)
                                                :include-start-position t
                                                :start-position-fn (lambda ()
                                                                     (goto-char (window-start))
                                                                     (beginning-of-line-text)
                                                                     ))))))))



(defun pointless-source-mark () (seq-uniq (--filter (and (/= it (point))
                                                         (>= it (window-start)) (< it (window-end)))
                                                    (cons (mark) mark-ring))))

(pointless-defmove-unidirectional pointless-moveto-mark (pointless-source-mark))
(pointless-defjump-unidirectional pointless-jump-mark
                                  (pointless-source-mark)
                                  :max-num-candidates pointless-moveto-mark-max-candidates
                                  :partition-fn #'pointless-partition-values-top-down
                                  )

(pointless-defjump-unidirectional pointless-jump-beginning-of-line
                                  (pointless--collect-targets-iteratively
                                   (lambda (istep)
                                     (goto-char (window-start))
                                     (beginning-of-line (1+ istep)))
                                   :start-position-fn (apply-partially #'goto-char (window-start))
                                   ))



(pointless-defjump-unidirectional pointless-jump-beginning-of-line-text
                                  (pointless--collect-targets-iteratively
                                   (lambda () (beginning-of-line-text 2))
                                   :start-position-fn (lambda ()
                                                        (goto-char (window-start))
                                                        (beginning-of-line-text))
                                   :include-start-position t
                                   ))

(pointless-defjump-unidirectional pointless-jump-end-of-line
                                  (pointless--collect-targets-iteratively
                                   (lambda () (end-of-line 2))
                                   :include-start-position t
                                   :start-position-fn (lambda ()(goto-char (window-start))
                                                        (end-of-line))
                                         ))

(defun pointless-source-word-beginning ()
  (nreverse (pointless--collect-targets-iteratively
             (lambda (istep)
               (backward-word)
               )
             :start-position-fn (apply-partially #'goto-char (1- (window-end)))
             :include-start-position t)))

(defun pointless-helper-buffer-char-as-string (pos)
  (buffer-substring-no-properties pos (1+ pos)))

(defun pointless-helper-buffer-looking-at-string (str pos)
  (string-equal str (buffer-substring-no-properties pos (+ pos (length str)))))

(defun pointless-helper-read-char-as-string (prompt &rest format-args)
  (char-to-string (read-char (apply #'format prompt format-args) t))
  )

(defun pointless-helper-re-search-forward (re)
  "Like `re-search-forward', but if the search fails, go to end of buffer to stop the search."
  (unless (re-search-forward re nil t)
    (end-of-buffer)))

(pointless-defjump-unidirectional pointless-jump-word-beginning (pointless-source- word-beginning))

(pointless-defjump-unidirectional pointless-jump-word-beginning-1
                                  (let ((char (pointless-helper-read-char-as-string "Word beginning character: ")))
                                    (seq-filter (apply-partially #'pointless-helper-buffer-looking-at-string char)
                                                (pointless-source-word-beginning))
                                    ))

(pointless-defjump-unidirectional pointless-jump-char-1
                                  (let ((char (pointless-helper-read-char-as-string "Goto character: ")))
                                    (mapcar #'1-
                                              (pointless--collect-targets-iteratively
                                               (lambda (istep) (unless (re-search-forward (regexp-quote char) nil t)
                                                                 (end-of-buffer))
                                                 ;;(message "%S %i %S %i" (point) (point-max) (< (point) (point-max)) istep)
                                                 )
                                               :start-position-fn (apply-partially #'goto-char (window-start))))
                                    ))

(pointless-defjump-unidirectional pointless-jump-char-line-1
                                  (let ((char (pointless-helper-read-char-as-string "Goto character in line: ")))
                                    (mapcar #'1-
                                              (pointless--collect-targets-iteratively
                                               (lambda (istep) (pointless-helper-re-search-forward (regexp-quote char)))
                                               :start-position-fn (apply-partially #'goto-char (line-beginning-position))
                                               :region-begin (line-beginning-position)
                                               :region-end (line-end-position)))
                                    ))


(pointless-defjump-unidirectional pointless-jump-org-headline
                                  (pointless--collect-targets-iteratively
                                   (lambda (istep) (org-next-visible-heading 1))
                                   :start-position-fn (apply-partially #'goto-char (window-start))))

(pointless-defjump-unidirectional pointless-jump-org-headline-tags
                                  (mapcar #'1-
                                            (pointless--collect-targets-iteratively
                                             (lambda (istep) (pointless-helper-re-search-forward org-tag-line-re))
                                             :start-position-fn (apply-partially #'goto-char (window-start)))))

(pointless-defjump-unidirectional pointless-jump-keyword
                                  (let ((face 'font-lock-keyword-face))
                                    (pointless--collect-targets-iteratively
                                     (lambda (istep) (unless (text-property-search-forward 'face face #'eq t)
                                                       (end-of-buffer)))
                                     :start-position-fn (apply-partially #'goto-char (window-start)))))




;;(remove-overlays)

(provide 'pointless)
