;;; treesit-fold-level.el --- Level-based folding for treesit-fold  -*- lexical-binding: t; -*-

;; This adds level-based folding functionality to treesit-fold

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'treesit)
(require 'treesit-fold)
(require 'treesit-fold-util)  ; Make sure we have utility functions

(defun treesit-fold--node-level (node)
  "Calculate the level (depth) of NODE in the syntax tree.
Root node is at level 0."
  (let ((level 0)
        (current node))
    (while (setq current (treesit-node-parent current))
      (cl-incf level))
    level))

(defun treesit-fold--foldable-nodes-at-level (level)
  "Return all foldable nodes at specific LEVEL in the syntax tree."
  (let* ((root (treesit-buffer-root-node))
         (mode-ranges (alist-get major-mode treesit-fold-range-alist))
         (foldable-nodes '()))
    (when mode-ranges
      (setq foldable-nodes 
            (treesit-fold--collect-nodes-at-level root level mode-ranges)))
    foldable-nodes))

(defun treesit-fold--collect-nodes-at-level (node target-level mode-ranges)
  "Recursively collect foldable nodes at TARGET-LEVEL.
NODE is the current node being examined.
MODE-RANGES contains the foldable node types for current mode."
  (let ((current-level (treesit-fold--node-level node))
        (collected '()))
    (cond
     ;; If we're at the target level, check if this node is foldable
     ((= current-level target-level)
      (let* ((node-type (treesit-node-type node))
             (type-sym (intern node-type))
             (fold-fn (alist-get type-sym mode-ranges)))
        (when fold-fn
          ;; Check if the node actually has a valid fold range
          (let ((range (treesit-fold--get-fold-range node)))
            (when (and range 
                       (not (treesit-fold--range-on-same-line range)))
              (push node collected))))))
     ;; If we haven't reached the target level yet, recurse into children
     ((< current-level target-level)
      (dolist (child (treesit-node-children node))
        (setq collected 
              (append collected
                      (treesit-fold--collect-nodes-at-level 
                       child target-level mode-ranges))))))
    collected))

(defun treesit-fold--max-foldable-level ()
  "Return the maximum level that contains foldable nodes."
  (let* ((root (treesit-buffer-root-node))
         (mode-ranges (alist-get major-mode treesit-fold-range-alist))
         (max-level 0))
    (when mode-ranges
      (treesit-fold--find-max-level root mode-ranges 0 max-level))))

(defun treesit-fold--find-max-level (node mode-ranges current-level max-level)
  "Recursively find the maximum level with foldable nodes."
  (when (and (alist-get (intern (treesit-node-type node)) mode-ranges)
             (not (treesit-fold--node-range-on-same-line node)))
    (setq max-level (max max-level current-level)))
  (dolist (child (treesit-node-children node))
    (setq max-level (treesit-fold--find-max-level 
                    child mode-ranges (1+ current-level) max-level)))
  max-level)

;;;###autoload
(defun treesit-fold-close-level (level)
  "Fold all foldable nodes at LEVEL in the syntax tree."
  (interactive "nFold at level: ")
  (treesit-fold--ensure-ts
    (let ((nodes (treesit-fold--foldable-nodes-at-level level))
          (count 0))
      (dolist (node nodes)
        (when (treesit-fold-close node)
          (cl-incf count)))
      (message "Folded %d nodes at level %d" count level)
      count)))

;;;###autoload
(defun treesit-fold-open-level (level)
  "Open all folded nodes at LEVEL in the syntax tree."
  (interactive "nOpen at level: ")
  (treesit-fold--ensure-ts
    (let ((nodes (treesit-fold--foldable-nodes-at-level level))
          (count 0))
      (dolist (node nodes)
        (when-let ((ov (treesit-fold-overlay-at node)))
          (delete-overlay ov)
          (cl-incf count)))
      (message "Opened %d nodes at level %d" count level)
      count)))

;;;###autoload
(defun treesit-fold-toggle-level (level)
  "Toggle all foldable nodes at LEVEL in the syntax tree."
  (interactive "nToggle at level: ")
  (treesit-fold--ensure-ts
    (let ((nodes (treesit-fold--foldable-nodes-at-level level))
          (folded 0)
          (opened 0))
      (dolist (node nodes)
        (if-let ((ov (treesit-fold-overlay-at node)))
            (progn
              (delete-overlay ov)
              (cl-incf opened))
          (when (treesit-fold-close node)
            (cl-incf folded))))
      (message "Level %d: Folded %d, Opened %d nodes" level folded opened))))

;;;###autoload
(defun treesit-fold-close-all-to-level (max-level)
  "Fold all foldable nodes from level 0 to MAX-LEVEL."
  (interactive "nFold all nodes up to level: ")
  (treesit-fold--ensure-ts
    (let ((total 0))
      (dotimes (level (1+ max-level))
        (cl-incf total (or (treesit-fold-close-level level) 0)))
      (message "Folded %d total nodes up to level %d" total max-level))))

;;;###autoload
(defun treesit-fold-open-to-level (max-level)
  "Open all nodes up to MAX-LEVEL, keeping deeper levels folded."
  (interactive "nOpen nodes up to level: ")
  (treesit-fold--ensure-ts
    ;; First, fold everything
    (treesit-fold-close-all)
    ;; Then open up to the specified level
    (let ((total 0))
      (dotimes (level (1+ max-level))
        (cl-incf total (or (treesit-fold-open-level level) 0)))
      (message "Opened %d nodes up to level %d" total max-level))))

;;;###autoload
(defun treesit-fold-cycle-levels ()
  "Cycle through folding levels: all open -> level 1 -> level 2 -> ... -> all closed -> all open."
  (interactive)
  (treesit-fold--ensure-ts
    (let* ((max-level (treesit-fold--max-foldable-level))
           (current-state (treesit-fold--current-fold-state))
           (next-level))
      (cond
       ;; If everything is open, start by folding level 1
       ((eq current-state 'all-open)
        (treesit-fold-close-all-to-level 0)
        (message "Folded to level 0"))
       ;; If everything is closed, open everything
       ((eq current-state 'all-closed)
        (treesit-fold-open-all)
        (message "Opened all levels"))
       ;; Otherwise, advance to the next level
       (t
        (let ((current-max-folded (treesit-fold--highest-folded-level)))
          (if (>= current-max-folded max-level)
              (progn
                (treesit-fold-open-all)
                (message "Opened all levels"))
            (progn
              (treesit-fold-close-level (1+ current-max-folded))
              (message "Folded level %d" (1+ current-max-folded))))))))))

(defun treesit-fold--current-fold-state ()
  "Determine the current folding state of the buffer."
  (let ((overlays (treesit-fold--overlays-in 'invisible 'treesit-fold)))
    (cond
     ((null overlays) 'all-open)
     ((treesit-fold--all-foldable-folded-p) 'all-closed)
     (t 'partial))))

(defun treesit-fold--all-foldable-folded-p ()
  "Check if all foldable nodes are currently folded."
  (let* ((root (treesit-buffer-root-node))
         (mode-ranges (alist-get major-mode treesit-fold-range-alist))
         (all-folded t))
    (when mode-ranges
      (treesit-fold--check-all-folded root mode-ranges))
    all-folded))

(defun treesit-fold--check-all-folded (node mode-ranges)
  "Recursively check if all foldable nodes under NODE are folded."
  (when (and (alist-get (intern (treesit-node-type node)) mode-ranges)
             (not (treesit-fold--node-range-on-same-line node))
             (not (treesit-fold-overlay-at node)))
    nil)  ; Found an unfolded foldable node
  (cl-every (lambda (child)
              (treesit-fold--check-all-folded child mode-ranges))
            (treesit-node-children node)))

(defun treesit-fold--highest-folded-level ()
  "Return the highest level that has all its foldable nodes folded."
  (let ((max-level (treesit-fold--max-foldable-level))
        (highest -1))
    (dotimes (level (1+ max-level))
      (let ((nodes (treesit-fold--foldable-nodes-at-level level)))
        (when (and nodes
                   (cl-every (lambda (node) (treesit-fold-overlay-at node)) nodes))
          (setq highest level))))
    highest))

;;;###autoload
(defun treesit-fold-show-all-levels ()
  "Show a summary of all nodes at each level (for debugging)."
  (interactive)
  (treesit-fold--ensure-ts
    (let* ((root (treesit-buffer-root-node))
           (mode-ranges (alist-get major-mode treesit-fold-range-alist))
           (level-data (make-hash-table :test 'equal))
           (max-level 0))
      ;; Collect all nodes by level
      (treesit-fold--collect-all-by-level root level-data 0)
      ;; Find max level
      (maphash (lambda (level _) (setq max-level (max max-level level))) 
               level-data)
      ;; Display results
      (with-output-to-temp-buffer "*treesit-fold-levels*"
        (princ (format "Mode: %s\n" major-mode))
        (princ (format "Foldable types: %s\n\n" 
                      (mapcar #'car mode-ranges)))
        (dotimes (level (1+ max-level))
          (when-let ((nodes (gethash level level-data)))
            (let* ((type-counts (make-hash-table :test 'equal))
                   (foldable-count 0))
              ;; Count node types
              (dolist (node nodes)
                (let* ((type (treesit-node-type node))
                       (type-sym (intern type)))
                  (puthash type (1+ (gethash type type-counts 0)) type-counts)
                  (when (and (alist-get type-sym mode-ranges)
                            (treesit-fold--get-fold-range node)
                            (not (treesit-fold--range-on-same-line 
                                  (treesit-fold--get-fold-range node))))
                    (cl-incf foldable-count))))
              ;; Print level summary
              (princ (format "Level %d: %d nodes (%d foldable)\n" 
                            level (length nodes) foldable-count))
              ;; Print type breakdown
              (maphash (lambda (type count)
                        (let* ((type-sym (intern type))
                               (is-foldable (alist-get type-sym mode-ranges)))
                          (princ (format "  %s%s: %d\n" 
                                        (if is-foldable "*" " ")
                                        type count))))
                      type-counts)
              (princ "\n"))))))))

(defun treesit-fold--collect-all-by-level (node table level)
  "Collect all nodes by level into TABLE."
  (puthash level (cons node (gethash level table '())) table)
  (dolist (child (treesit-node-children node))
    (treesit-fold--collect-all-by-level child table (1+ level))))

;; Optional: Add keybindings for level-based folding
(defun treesit-fold-level-setup-keys ()
  "Setup convenient keybindings for level-based folding."
  (define-key treesit-fold-mode-map (kbd "C-c f 1") 
    (lambda () (interactive) (treesit-fold-toggle-level 1)))
  (define-key treesit-fold-mode-map (kbd "C-c f 2") 
    (lambda () (interactive) (treesit-fold-toggle-level 2)))
  (define-key treesit-fold-mode-map (kbd "C-c f 3") 
    (lambda () (interactive) (treesit-fold-toggle-level 3)))
  (define-key treesit-fold-mode-map (kbd "C-c f 4") 
    (lambda () (interactive) (treesit-fold-toggle-level 4)))
  (define-key treesit-fold-mode-map (kbd "C-c f c") 'treesit-fold-cycle-levels)
  (define-key treesit-fold-mode-map (kbd "C-c f l") 'treesit-fold-show-current-level))

(provide 'treesit-fold-level)
;;; treesit-fold-level.el ends here
