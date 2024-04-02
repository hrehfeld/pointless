;;; pointless.el --- An avy clone -*- lexical-binding: t; eval: (aggressive-indent-mode); -*-
;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;; Version: 0.1
;; URL: https://github.com/hrehfeld/pointless/
;; This file is not part of GNU Emacs.
;;; Commentary:
;; TODO
;; Package-Requires: ((emacs "25.1") dash cl-lib seq)

(require 'cl-lib)
(require 'seq)
(require 'dash)

;;; Code:

;; FIXME: use -interleave-all once merged
(defun pointless-interleave-with-rest (&rest lists)
  "Return a new list of the first item in each list, then the second etc."
  (declare (pure t) (side-effect-free t))
  (when lists
    (let (result)
      (while (--any? (consp it) lists)
        (while (-none? 'null lists)
          (--each lists (!cons (car it) result))
          (setq lists (-map 'cdr lists)))
        (setq lists (-filter #'consp lists)))
      (nreverse result))))

(defface pointless-target `((t . (:foreground "white" :background "#892E8B"))) "Generic face for a pointless jump target.")
(defface pointless-target-1 `((t . (:inherit pointless-target))) "First face for pointless jump targets.")
(defface pointless-target-2 `((t . (:background "#DE3E7A" :inherit pointless-target))) "Second face for pointless jump targets.")
(defface pointless-target-3 `((t . (:background "#FF8964" :inherit pointless-target))) "Second face for pointless jump targets.")
(defface pointless-target-4 `((t . (:background "#F5B83B" :inherit pointless-target))) "Second face for pointless jump targets.")
(defface pointless-face-further `((t . (:background "#C0E08B" :inherit pointless-target))) "Generic face for a pointless jump target when no better face is available.")


(defmacro pointless-save-window-start-and-mark-and-excursion (&rest body)
  "Save everything interesting to pointless, execute `BODY', restore.

Return result of `BODY'."
  (let ((window-start (cl-gensym 'pointless-window-start))
        (window (cl-gensym 'pointless-window))
        (point (cl-gensym 'pointless-point))
        (result (cl-gensym 'pointless-result)))
    `(let ((,window-start (window-start))
           (,window (selected-window))
           (,point (point))
           (,result (save-mark-and-excursion ,@body)))
       (set-window-start ,window ,window-start)
       ;; (cl-assert (= (point) ,point) t)
       (goto-char ,point)
       ,result)))


(defun pointless-target-show (position length str)
  ;;(cl-assert (eq (get-text-property position 'display) nil) "Position already has a display text property!")
  ;;(cl-assert (eq (get-text-property position 'face) nil) "Position already has a face text property!")
  (let* ((overlay (make-overlay position (+ position length)  nil t nil)))
    (overlay-put overlay
                 (if (eq position (point-max)) 'after-string 'display)
                 str)
    (overlay-put overlay 'read-only t)
    (list overlay)))

(defun pointless-target-hide (overlays)
  (cl-assert (listp overlays))
  (mapc #'delete-overlay overlays))

(defvar pointless-last-search-input nil "The result of the last pre-tree-traverse input.")
(defvar pointless-resume-command nil "Which command to call when resuming.")
(defvar pointless-repeat-command nil "Which command to call when repeating.")
(defvar pointless-last-command-args nil
  "Arguments to either `pointless-repeat-command' or `pointless-resume-command'.")
(defvar pointless-last-select-keys nil "Last key combination that selected a leaf from a tree.")
(defvar pointless-last-action-fn nil "Last used action function. See `pointless-action-functions'.")
(defvar pointless-last-selected-index nil "Index of last element that was selected with `pointless-select'.")

(defvar pointless-keys '(("asdfghjkl;'" . ?h) ("qwertyuiop" . ?y) ("zxcvbnm,." . ?b) ("1234567890" . ?6)) "A list of strings of keys that are used as jump keys.

pointless will use these list to build keys relative to point, each list element (a list of keys) will be assigned to one category of jumps.")
;;(setq pointless-keys '(("asdfghjkl;'" . ?h) ("qwertyuiop" . ?y) ("zxcvbnm,." . ?b) ("1234567890" . ?6)))

;; TODO: add other rows as lists of characters
(defvar pointless-jump-keysets '((?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)))
(defvar pointless-action-keyset '(?w ?e ?i ?o))

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
     (cl-flet ((within-region (pos)
                              (and (<= region-begin pos)
                                   (<= (point-min) pos)
                                   (< pos region-end)
                                   (<= pos (point-max))
                                   )))
       (let ((candidates (cl-loop with last-point = (1- (point))
                                  for istep from 0
                                  do (progn
                                       (setq last-point (point))
                                       (pointless--funcall-arg-maybe create-targets-fun istep)
                                       ;; (message "collect: %i %i %i %i" istep (point) last-point region-begin)
                                       )
                                  while (and (within-region (point))
                                             (or (eq max-num-candidates nil)
                                                 (< istep (- max-num-candidates (if include-start-position 1 0))))
                                             ;; allow the first step to sit at region-begin
                                             (or (= istep 0) (/= (point) last-point)))
                                  ;; filter out candidates that are outside region
                                  ;; e.g. with a regex search, condition might be true before the movement
                                  ;;if (and (within-region (point)))
                                  collect (progn (point))
                                  )))
         (cl-check-type candidates (list-of number))
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
    (cl-stable-sort candidates (lambda (&rest signed-distances)
                                 (let ((distances (mapcar #'abs signed-distances)))
                                   (cond ((apply #'< distances) t)
                                         ((apply #'= distances) (apply #'< signed-distances)))))
                    :key (lambda (pos) (- pos point)))))

(defun pointless-sort-candidates-before-after-point (candidates)
  (let ((point (point)))
    (apply #'pointless-interleave-with-rest
           (mapcar (lambda (fn)
                     (--> (--filter (funcall fn it point) candidates)
                          (cl-stable-sort it #'< :key (lambda (pos) (abs (- pos point))))))
                   '(< >=)))))


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
  (cl-destructuring-bind (key face tree-or-position) key-face-nodes-node
    (let ((prefix-keys (cons (cons key face) prefix-keys)))
      ;;(message "%S %S --- %S" key face tree-or-position)
      (if (pointless--tree-position-p tree-or-position)
          ;; leaf nodes show and return positions & prefix-keys (as well as faces)
          (list (list tree-or-position (seq-reverse prefix-keys)))
        ;; inner nodes just collect prefixes and results
        (seq-mapcat (lambda (el) (pointless--get-position-prefix-keys el prefix-keys))
                    tree-or-position)
        ))))

(defun pointless--create-overlays (keys-faces-positions-nodes &optional compose-fn)
  "Create overlays from the `KEYS-FACES-POSITIONS-NODES' and return an
list like `((POSITION KEY-SEQUENCE (OVERLAY ...) (OVERLAY ...) ...) ...)'"
  (cl-assert (equal keys-faces-positions-nodes (seq-uniq keys-faces-positions-nodes)) t)
  ;; no unique keys
  (let ((keys (mapcar #'car keys-faces-positions-nodes)))
    (cl-assert (equal keys (seq-uniq keys))))
  ;;(message "pointless--create-overlays %S" keys-faces-positions-nodes)
  (let* ((positions-prefix-keys (seq-mapcat #'pointless--get-position-prefix-keys keys-faces-positions-nodes))
         (positions-prefix-keys (cl-stable-sort positions-prefix-keys #'<
                                                :key (lambda (position--prefix-keys)
                                                       (let ((position (car position--prefix-keys)))
                                                         (if (markerp position)
                                                             (marker-position position)
                                                           position)))))
         )
    (apply #'seq-concatenate 'list
           (let ((tail (cdr positions-prefix-keys))
                 next-position)
             (cl-loop
              for (position prefix-keys) in positions-prefix-keys
              ;; only place overlay on a single char, so we only take buffer-substring of length 1
              collect
              (let* ((next-position (when tail
                                      (caar tail))))
                (when tail
                  (cl-check-type next-position number-or-marker (format "%S %S" next-position (car tail)))
                  ;; keep tail in sync with iterator
                  (setq tail (cdr tail)))
                (cl-check-type position number-or-marker)
                (cl-check-type next-position (or null number-or-marker))
                (when next-position
                  (cl-assert (< position next-position) nil
                             (format-message "Position %S is not before next position %S. Context: %S" position next-position (car tail))))
                ;; (message "pointless--create-overlays %S %S" position next-position)
                (pointless--show-keys prefix-keys position next-position compose-fn)))))))

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


(defun pointless-helper-default (command-name command-value-alist default-value &optional explicit-arg type)
  (let ((value (or (alist-get command-name command-value-alist)
                   explicit-arg
                   default-value)))
    (when type
      (eval `(cl-check-type ',value ,type) t))
    value))

(defun pointless-sort-candidates-function-default (command-name &optional partition-fn)
  (pointless-helper-default command-name pointless-sort-candidates-function-alist pointless-sort-candidates-default-function partition-fn))

;;(pointless-partition-candidates-function-default 'pointless-jump-word-beginning)

(defun pointless-keyset-default (command-name keysets &optional explicit-keyset)
  (cl-check-type keysets (list-of (list-of number)))
  (cl-check-type explicit-keyset (or null (list-of number)))
  (cl-check-type command-name symbol)
  (let ((keyset (pointless-helper-default command-name
                                          pointless-keysets-alist
                                          ;; just take the first keyset
                                          (car keysets)
                                          explicit-keyset
                                          '(list-of number))))
    (cl-check-type keyset (list-of number))
    ;; if keyset is a number, interpret as index into pointless-keys
    (if (numberp keyset)
        (nth keyset pointless-jump-keysets)
      keyset)))


(defun pointless--do-jump--check-overlays (keys-overlays)
  (mapc (lambda (keys-overlays)
          (cl-assert (number-or-marker-p (car keys-overlays)))
          (cl-assert (cl-every #'number-or-marker-p (cadr keys-overlays)))
          (cl-assert (cl-every #'listp (caddr keys-overlays)))
          (mapc (lambda (overlays) (cl-assert (cl-every #'overlayp overlays) overlays))
                (caddr keys-overlays)))
        keys-overlays))


(defun pointless--select (keys-faces-positions-nodes keys-actions compose-fn action-fn)
  "`COMMAND-NAME' is the name of the calling command."
  (unless keys-faces-positions-nodes
    (user-error "No valid jump targets."))
  (cl-assert (seqp keys-faces-positions-nodes) t)

  (setq inhibit-quit t)
  (let (
        ;; save overlays globally so we can easily remove them on quit
        keys-overlays)
    (prog1
        (with-local-quit
          (cl-labels
              ((read-level (ilevel prefix-keys nodes)
                           "Read the `ilevel'-th level of the jump tree.

`prefix-keys' are the previosly pressed keys if any.
`keys-treenodes'."
                           ;;(message "ilevel: %S,	prefix-keys: %S,	tree: %S" ilevel prefix-keys nodes)
                           (setq keys-overlays (pointless--create-overlays nodes compose-fn))
                           ;;(message "%S" keys-overlays)
                           ;;(check-overlays keys-overlays)

                           ;; loop while key was selecting an action instead of walking the tree
                           (let (position-read)
                             (while (not position-read)
                               (let* ((keys (mapcar #'car nodes))
                                      (action-keys (mapcar #'car keys-actions))
                                      (key (read-char-choice-with-read-key (format "%s: (%s) or action [%s]"
                                                                                   (let ((verb (alist-get action-fn pointless-action-functions)))
                                                                                     (if verb
                                                                                         (s-capitalize verb)
                                                                                       (format "[error: check pointless-action-functions for %S]" action-fn)))
                                                                                   (s-join "" (--map (char-to-string it) keys))
                                                                                   (s-join "" (--map (char-to-string it) action-keys))
                                                                                   )
                                                                           (seq-concatenate 'list keys action-keys)))
                                      )
                                 ;;(message "%S %S" key action-keys)
                                 (if (member key action-keys)
                                     (progn (cl-assert (not (member key keys)) nil "Overlapping keys: action-keys: %S and position-keys: %S" action-keys keys)
                                            (let ((ikey (seq-position action-keys key)))
                                              (setq action-fn (cadr (nth ikey keys-actions)))
                                              (message "setting action-fn to %s" action-fn)))
                                   ;; not an action key, walk tree
                                   (let* ((ikey (seq-position keys key))
                                          (prefix-keys (cons key prefix-keys)))
                                     (pointless-target-hide keys-overlays)
                                     (setq position-read
                                           (let ((chosen-item (caddr (nth ikey nodes))))
                                             (cl-assert chosen-item)
                                             ;; if at leaf node
                                             (if (pointless--tree-position-p chosen-item)
                                                 (progn
                                                   (setq pointless-last-select-keys prefix-keys
                                                         pointless-last-action-fn action-fn)
                                                   (let ((res (funcall action-fn chosen-item)))
                                                     ;; (message "pointless--select %S %S" (or res chosen-item) res)
                                                     (or res chosen-item)))
                                               (let ((res (read-level (1+ ilevel) prefix-keys chosen-item)))
                                                 ;; (message "pointless--select %S" res)
                                                 res))))))))
                             position-read)))
            (read-level 0 nil keys-faces-positions-nodes)))
      (pointless-target-hide keys-overlays)
      (setq inhibit-quit nil))))

(defvar pointless-this-command nil "For recursive pointless
command calls via actions, save the current jump command.

See `pointless-push-mark'.")

(defun pointless-push-mark ()
  "Save the mark if it is active and we're not inside a recursive pointless jump. See `pointless-this-command'."
  (unless (or mark-active ;; (eq this-command pointless-this-command)
              )
    (push-mark nil nil)))

(defun pointless-action-jump (position)
  "Unless the mark is active, save the mark and goto-char `position'."
  (pointless-push-mark)
  (goto-char position))

(defun pointless-action-jump-behind (position)
  "Unless the mark is active, save the mark and goto-char `position' + 1."
  (pointless-push-mark)
  (goto-char (1+ position)))

(defun pointless-action-yank (position)
  "`yank' at `position'."
  (save-excursion
    (goto-char position)
    (call-interactively #'yank)))

(defun pointless-action-recenter-top-bottom (position)
  "Recenter window using `recenter-top-bottom' around `position'."
  (save-excursion
    (goto-char position)
    (recenter-top-bottom)))

(defun pointless-action-pointless-jump-sexp (position)
  "From `position', jump again using `pointless-jump-sexp'."
  (pointless-push-mark)
  (goto-char position)
  (call-interactively #'pointless-jump-sexp t))

(defvar pointless-action-default-function #'pointless-action-jump
  "The action that is called when not overridden by jump specific defaults.
See `pointless-action-jump' and `pointless--select'.")
(defvar pointless-action-function-alist nil "Define default
action functions in an alist per command.")

(defvar pointless-action-functions
  '((pointless-action-jump . "jump to")
    (pointless-action-jump-behind . "jump behind")
    (pointless-action-recenter-top-bottom . "recenter around")
    (pointless-action-pointless-jump-sexp . "jump sexp from")
    (pointless-action-yank . "yank at"))
  "Alist of actions that may be called after candidate selection.

Each element is a cons cell `(action . verb)', where action is a
function symbol and verb is a string put into the candidate
selection prompt.

Each function takes the position as its only argument. See
`pointless-action-jump'.")

(defvar pointless-action-function-alist nil "Define default action functions in an alist per command.")



(defun pointless-select (command-name keys-faces-positions-nodes &optional compose-fn action-fn keys-actions)
  "`COMMAND-NAME' is the name of the calling command."
  (let ((compose-fn
         (or compose-fn
             (assq command-name pointless-compose-overlay-function-alist)
             pointless-compose-overlay-default-function))
        (action-fn
         (or action-fn
             (assq command-name pointless-action-function-alist)
             pointless-action-default-function))
        (keys-actions
         (or keys-actions
             (assq command-name pointless-action-function-alist)
             (-zip-lists pointless-action-keyset (mapcar #'car pointless-action-functions)))))
    (setq pointless-this-command command-name)
    (pointless--select keys-faces-positions-nodes keys-actions compose-fn action-fn)))

(defun pointless-resume ()
  (interactive)
  (pointless--call-resume-command))

(defun pointless--call-resume-command ()
  (cl-check-type pointless-resume-command function)
  (cl-check-type pointless-last-command-args list)
  (apply pointless-resume-command (append pointless-last-command-args)))

(defun pointless--call-repeat-command ()
  (apply pointless-repeat-command pointless-last-command-args))

(defun pointless-repeat ()
  (interactive)
  (pointless--call-repeat-command))


(defun pointless-jump-chars-words-lines ()
  (interactive)
  (pointless-select pointless-keys
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
  (seq-reverse
   (mapcar #'seq-reverse
           (seq-partition (seq-reverse values) (ceiling (length values) num-keys)))))

(defun pointless-partition-values-quick-first (values num-keys &optional quick-key-ratio tree-keys-first?)
  ""
  ;;(message "values: %S" values)
  ;; (seq-partition values (1+ (/ (length values) 2)))
  (let* ((num-values (length values))
         (quick-key-ratio (or quick-key-ratio 0.75))
         (min-num-quick-keys (min (floor num-keys (/ 1 quick-key-ratio)) (1- num-keys)))
         (max-num-tree-keys (- num-keys min-num-quick-keys))
         (quick-values (nreverse (seq-take values min-num-quick-keys)))
         (tree-values (seq-reverse (nthcdr min-num-quick-keys values))))
    (cl-assert (> num-keys min-num-quick-keys 0) t)
    ;; (message "tree-values: %i %S" (length tree-values) tree-values)
    ;; (message "quick-values: %i %S" (length quick-values) quick-values)
    ;; (message "num-keys: %i %i %i" num-keys min-num-quick-keys max-num-tree-keys)
    (cl-assert (= num-values (+ (length quick-values) (length tree-values))) t)
    (let* ((tree-values (cl-loop for depth from 1
                                 with partitions = (seq-partition tree-values num-keys)
                                 while (length> partitions max-num-tree-keys)
                                 do (let ((tree-size (+ num-keys (* (1- depth)
                                                                    min-num-quick-keys))))
                                      (setq partitions (seq-partition tree-values
                                                                      tree-size))
                                      (setq depth (1+ depth)))
                                 finally return (nreverse (mapcar #'nreverse partitions))))
           )
      (cl-assert tree-values t)
      ;; extra error checking
      ;; (message "tree-values: %i %S" (length tree-values) tree-values)
      ;; (message "quick-values: %i %S" (length quick-values) quick-values)
      (cl-labels ((num-values () (+ (length quick-values) (-reduce #'+ (mapcar #'length tree-values)))))
        (cl-assert (= (length values) (num-values)) t)
        (cl-loop while (< (+ (length quick-values) (length tree-values)) (1- num-keys))
                 do (progn
                      (push (car (car tree-values)) quick-values)
                      (pop (car tree-values))
                      (cl-assert (= (length values) (num-values)) t)
                      )
                 unless (car tree-values)
                 do (pop tree-values)))
      ;; extra error checking end
      (cl-assert tree-values t)
      (let ((quick-values (mapcar #'list (nreverse quick-values))))
        ;; (message "tree-values: %i %S" (length tree-values) tree-values)
        ;; (message "quick-values: %i %S" (length quick-values) quick-values)
        (cl-destructuring-bind (front-values back-values)
            ;; assign vars
            (let ((vs (list quick-values tree-values)))
              (if tree-keys-first? (nreverse vs) vs))
          (let* ((result (append front-values back-values)))
            ;; (message "result: %i %S" (length result) result)
            (cl-assert result t)
            (cl-check-type result (list-of (list-of number-or-marker)))
            (cl-assert (<= (length result) num-keys) t)
            (cl-assert (= (length values) (-reduce #'+ (mapcar #'length result))) t)
            result))))))

(defun pointless-partition-values-quick-last (values num-keys &optional num-quick-keys)
  (pointless-partition-values-quick-first values num-keys num-quick-keys t))

(defun pointless-make-jump-keys-unidirectional (keys positions partition-fn)
  (cl-assert (functionp partition-fn) t)
  (cl-check-type keys (satisfies listp))
  (cl-assert (length> keys 1) t)
  (cl-check-type positions (list-of number-or-marker))
  (cl-assert (equal positions (seq-uniq positions)) t)
  (let* ((keys (if (stringp keys) (string-to-list keys) keys))
         (num-keys (length keys))
         (keys-faces-positions-nodes
          (cl-labels
              ((descend (ilevel values)
                        (let* ((num-values (length values))
                               (face (or (nth ilevel pointless-faces) 'pointless-face-further))
                               (faces (-repeat num-values face)))
                          ;;(message "level: %S,	num-keys: %S,	num-values: %S,	keys: %S" ilevel num-keys num-values keys)
                          ;;(message "	positions: %S" values)
                          (let ((res (if (<= num-values num-keys)
                                         values
                                       (let ((position-partitions (funcall partition-fn values num-keys)))
                                         (cl-check-type position-partitions (list-of (list-of number-or-marker)))
                                         (cl-assert (<= (length position-partitions) num-keys) t)
                                         (seq-map (lambda (positions)
                                                    (if (length> positions 1)
                                                        (progn (mapc (lambda (p)
                                                                       (cl-assert (number-or-marker-p p) t "%S" positions))
                                                                     positions)
                                                               (descend (1+ ilevel) positions))
                                                      (let ((position (car positions)))
                                                        (cl-check-type position number-or-marker)
                                                        position)))
                                                  position-partitions)))))
                            (-zip-lists keys faces res)))))

            (descend 0 positions))))
    (cl-check-type keys-faces-positions-nodes list)
    (cl-labels ((count-leafs (key-face-positions)
                             ;; (message "count-leafs %S" key-face-positions)
                             (cl-assert (length= key-face-positions 3) t)
                             (cl-destructuring-bind (key face values) key-face-positions
                               (cl-check-type values (or list pointless--tree-position))
                               (unless (pointless--tree-position-p values)
                                 (mapc (lambda (val)
                                         (cl-check-type val list)
                                         (cl-check-type (car val) number)
                                         (cl-check-type (cadr val) symbol)
                                         (cl-check-type (caddr val) (or list pointless--tree-position)))
                                       values))
                               (if (pointless--tree-position-p values)
                                   1
                                 (cl-reduce #'+ (mapcar #'count-leafs values))))
                             ))
      ;;(cl-check-type key-face-positions (list-of number))
      ;; (message "%S" key-face-positions)
      ;; (message "%S" positions)
      (let ((num-leafs (count-leafs (list ?a 'some-face keys-faces-positions-nodes))))
        (cl-assert (length= positions num-leafs) nil "List lengths: %i %i" (length positions) num-leafs)))
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
    (pointless-select (pointless-make-jump-keys-unidirectional keys (seq-take positions pointless-moveto-mark-max-candidates))))
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
  (pointless-select
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
         (pointless-select
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

(defun pointless-filter-position-due-to-text-properties (position)
  (let ((invisible? (get-char-property position 'invisble)))
    ;;(message "pointless-filter-position-due-to-text-properties: %i %S" position invisible?)
    (eq nil invisible?)))


(defun pointless--jump-clean-positions (positions sort-fn max-num-candidates)
  (let* ((positions (seq-uniq positions (lambda (a b) (cl-labels ((get-pos (pos) (if (markerp pos) (marker-position pos) pos)))
                                                        (= (get-pos a) (get-pos b))))))
         (positions (-filter #'pointless-filter-position-due-to-text-properties positions))
         (positions (-filter (apply-partially #'/= (point)) positions))
         (positions (if sort-fn (funcall sort-fn positions) positions))
         (positions (if max-num-candidates (seq-take positions max-num-candidates) positions)))
    positions))

(defun pointless--jump-get-positions (candidates-fn search-input sort-fn max-num-candidates)
  (let* ((positions (funcall candidates-fn search-input))
         (positions (pointless--jump-clean-positions positions sort-fn max-num-candidates)))
    positions))

(defun pointless--jump-repeat (name keyset candidates-fn sort-fn partition-fn max-num-candidates)
  "Same signature as `pointless--jump-select'."
  (cl-assert (not (eq nil pointless-last-selected-index)) t)
  (let* ((positions (pointless--jump-get-positions candidates-fn pointless-last-search-input sort-fn max-num-candidates)))
    ;; just return the same offset in positions instead of walking the tree
    (funcall pointless-last-action-fn (nth pointless-last-selected-index positions))))


(defun pointless--jump-select (name keyset candidates-fn sort-fn partition-fn max-num-candidates)
  "Same signature as `pointless--jump-repeat'."
  (let* ((positions (pointless--jump-get-positions candidates-fn pointless-last-search-input sort-fn max-num-candidates))
         (keys-faces-positions-nodes (pointless-make-jump-keys-unidirectional keyset positions partition-fn)))
    ;;(message "pointless-defjump-do-jump: %S %S" candidates-fn positions)
    (let ((position (pointless-select name keys-faces-positions-nodes)))
      (setq pointless-last-selected-index (-elem-index position positions))
      ;; (message "pointless-last-chosen-index: %S %S %S" pointless-last-selected-index position positions)
      position)))


(defun pointless--jump (name keyset search-input-fn candidates-fn sort-fn partition-fn max-num-candidates)
  ;; first, try to detect active multiple cursors
  (let ((keyset (pointless-keyset-default name pointless-jump-keysets keyset))
        (sort-fn (pointless-sort-candidates-function-default name sort-fn))
        (partition-fn (pointless-partition-candidates-function-default name partition-fn)))
    (message "mc: %S %S" this-command pointless-this-command)
    (if (and (boundp 'mc--executing-command-for-fake-cursor) mc--executing-command-for-fake-cursor)
        (progn
          (message "mc:pointless-resume")
          (pointless-repeat))
      (message "mc: full: %S" name)
      (setq pointless-repeat-command 'pointless--jump-repeat
            pointless-last-command-args (list name keyset candidates-fn sort-fn partition-fn max-num-candidates)
            pointless-resume-command 'pointless--jump-select
            pointless-last-search-input (when search-input-fn (funcall search-input-fn)))
      (apply #'pointless--jump-select pointless-last-command-args))))


;; &key aren't working with &rest for some reason
(cl-defmacro pointless-defjump-unidirectional (name &rest args)
  "Define a jump command conveniently.

`NAME' is the name of the command to be defined.

`ARGS' consists of KEYWORDS and `&rest'.
`&rest' is a function body that returns a list of candidate positions.

`:search-input' is a form that queries the user for input and
returns that input.

`:MAX-NUM-CANDIDATES' is the maximum number of candidates. This
is applied *after* sorting.

`:KEYSET' is the set of keys to be used.

`:SORT-FN' is a function that is called with the list of
candidates as the single argument and returns the list sorted.

`:PARTITION-FN' is a candidate partition function handed to
`pointless-make-jump-keys-unidirectional'.
"
  (declare (indent defun))
  (cl-destructuring-bind (candidates-forms keyword-args)
      (pointless--normalize-keyword-arguments-with-rest '(:sort-fn :partition-fn :max-num-candidates :keyset :search-input) args)
    ;;(message "%S" keyword-args)
    (let ((max-num-candidates (gensym 'max-num-candidates))
          (sort-fn (gensym 'sort-fn))
          (partition-fn (gensym 'partition-fn))
          (keyset (gensym 'keyset)))
      `(defun ,name ()
         (interactive)
         (let* ((,sort-fn ',(plist-get keyword-args :sort-fn))
                (,partition-fn ',(plist-get keyword-args :partition-fn))
                (,max-num-candidates ,(or (plist-get keyword-args :max-num-candidates) 999)) ;;use upper limit of 999 candidates if none given
                (,keyset ,(plist-get keyword-args :keyset))
                )
           (pointless--jump
            ',name
            ,keyset
            (lambda () ,(plist-get keyword-args :search-input))
            (lambda (search-input) (pointless-save-window-start-and-mark-and-excursion
                                    ,@candidates-forms))
            ,sort-fn
            ,partition-fn
            ,max-num-candidates)
           )))))

;; (let ((print-length 9999)
;;       (print-level 99)
;;       (eval-expression-print-level 99))
;;   (prin1 (macroexpand-1 '(pointless-defjump-unidirectional pointless-jump-beginning-of-line-text
;;                                   (progn (cons (point)
;;                                                (pointless--collect-targets-iteratively
;;                                                 (apply-partially #'beginning-of-line-text 2)
;;                                                 :include-start-position t
;;                                                 :start-position-fn (lambda ()
;;                                                                      (goto-char (window-start))
;;                                                                      (beginning-of-line-text)
;;                                                                      ))))))))



(defun pointless-source-mark ()
  (seq-uniq (--filter (and (/= it (point))
                           (>= it (window-start)) (< it (window-end)))
                      ;; make sure each mark has a buffer
                      (let ((marks (seq-filter #'marker-buffer mark-ring))
                            (mark (mark t)))
                        ;; add the current mark as it's not in mark-ring
                        (if (not (eq mark nil))
                            (cons mark marks)
                          marks)))))

(pointless-defmove-unidirectional pointless-moveto-mark (pointless-source-mark))
(pointless-defjump-unidirectional pointless-jump-mark
  (pointless-source-mark)
  :max-num-candidates pointless-moveto-mark-max-candidates)

(defun pointless-activate-mark ()
  "Select a mark and activate the region to it."
  (interactive)
  (save-excursion
    (pointless-jump-mark)
    (push-mark nil t t))
  (exchange-point-and-mark)
  ;;(activate-mark t)
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
   (lambda ()
     (next-line)
     (end-of-line))
   :include-start-position t
   :start-position-fn (lambda ()
                        (goto-char (window-start))
                        (end-of-line))
   )
  :sort-fn pointless-sort-candidates-before-after-point)

(defun pointless-source-word-beginning ()
  (nreverse (pointless--collect-targets-iteratively
             (lambda (istep)
               (backward-word)
               )
             :start-position-fn (apply-partially #'goto-char (1- (window-end)))
             :include-start-position t)))

(defun pointless-source-symbol-beginning ()
  (nreverse (pointless--collect-targets-iteratively
             (lambda (istep)
               (forward-symbol -1))
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

(cl-defmacro pointless-toggle-mode (mode pre-arg  &rest body)
  (let ((old-state (symbol-value mode)))
    (unless (equal old-state pre-arg)
      (funcall mode pre-arg))
    `(prog1
         (progn ,@body)
       (funcall (quote ,mode) ,old-state))
    ))

(pointless-defjump-unidirectional pointless-jump-subword-beginning
  (pointless-toggle-mode subword-mode 1 (pointless-source-word-beginning)))

(pointless-defjump-unidirectional pointless-jump-word-beginning
  (pointless-toggle-mode subword-mode -1 (pointless-source-word-beginning)))

(pointless-defjump-unidirectional pointless-jump-word-beginning-1
  (let ((char (pointless-helper-read-char-as-string "Word beginning character: ")))
    (seq-filter (apply-partially #'pointless-helper-buffer-looking-at-string char)
                (pointless-source-word-beginning))
    ))

(pointless-defjump-unidirectional pointless-jump-symbol-beginning
  (pointless-source-symbol-beginning))

(defun pointless-helper-read-char-timer (timeout prompt &rest format-args)
  "Read a character, then read more characters in TIMEOUT seconds after that.

PROMPT will be used as the prompt after format-args are applied to it using `format'."
  (let ((prompt (apply #'format prompt format-args))
        cancelled)
    (let ((first-char (read-char prompt t))
          (idle-timer (run-with-timer timeout nil (lambda () (setq cancelled t)))))
      (let ((chars (cons first-char
                         (cl-loop with char = nil
                                  while (not cancelled)
                                  do (setq char (read-char prompt t 0.05))
                                  if char
                                  collect char))))
        (when chars
          (s-join "" (mapcar #'char-to-string chars)))))))

(defvar pointless-jump-char-timeout 0.75 "Timeout before pointless-jump-char-timeout stops reading input")

(pointless-defjump-unidirectional
  pointless-jump-char-timeout
  (goto-char (window-start))
  (cl-loop
   while (re-search-forward (regexp-quote search-input) (window-end) t)
   ;; do
   ;; (message "timeout: (progn (goto-char %S) (re-search-forward (regexp-quote \"%s\") (window-end) t))
   ;; %i %i %i %i" i  search-input (match-beginning 0) (point) (window-start) (window-end)
   ;; )
   collect
   (match-beginning 0)
   ;; (let ((pos (match-beginning 0)))
   ;;   (goto-char (1+ pos))
   ;;   pos)
   )
  :search-input (pointless-helper-read-char-timer pointless-jump-char-timeout "Goto characters before timeout: ")
  :sort-fn pointless-sort-candidates-before-after-point)

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

(pointless-defjump-unidirectional pointless-jump-sexp
  (append
   (pointless--collect-targets-iteratively
    (lambda (istep)
      (with-demoted-errors
          (sp-forward-sexp))))
   (pointless--collect-targets-iteratively
    (lambda (istep)
      (with-demoted-errors
          (sp-backward-sexp))))))


;;(remove-overlays)

(provide 'pointless)
;;; pointless.el ends here
