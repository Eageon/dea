;; .yas-setup.el for html-mode

(defvar yas-html-default-tag "p")

(defvar yas-html-xhtml-attr "")

(defvar yas-html-just-like-tm nil
  "Html-mode snippets behave as close to TextMate as possible.")

(defun yas-html-activate ()
  (add-to-list (make-local-variable 'yas-mode-symbol) 'html-mode))

(add-hook 'nxml-mode-hook 'yas-html-activate)
(add-hook 'rhtml-mode-hook 'yas-html-activate)

(defun yas-html-remove-preceding-word ()
  (interactive)
  (let (word-begin
        word-end
        (line-beginning-position (line-beginning-position))
        (orig-point (point))
        retval)
    (save-excursion
      (when (and (forward-word -1)
                 (setq word-begin (point))
                 (forward-word 1)
                 (setq word-end (point))
                 (< word-begin orig-point)
                 (>= word-end orig-point)
                 (<= (line-beginning-position) word-begin)
                 ;; (not (string-match "^[\s\t]+$" "          "))
                 )
      (setq retval
            (cons
             (buffer-substring-no-properties word-begin orig-point)
             (buffer-substring-no-properties word-end orig-point)))
      (delete-region word-begin word-end)
      retval))))


(defun yas-html-first-word (string)
  (replace-regexp-in-string "\\\W.*" "" string))

(defun yas-html-insert-tag-pair-snippet ()
  (let* ((tag-and-suffix (or (and yas-selected-text
                                  (cons yas-selected-text nil))
                             (yas-html-remove-preceding-word)))
         (tag    (car tag-and-suffix))
         (suffix (or (cdr tag-and-suffix) ""))
         (single-no-arg "\\(br\\|hr\\)")
         (single        "\\(img\\|meta\\|link\\|input\\|base\\|area\\|col\\|frame\\|param\\)"))
    (cond ((null tag)
           (yas-expand-snippet (format "<${1:%s}>%s</${1:$(yas-html-first-word yas-text)}>%s"
                                       (or yas-html-default-tag
                                           "p")
                                       (if yas-html-just-like-tm "$2" "$0")
                                       suffix)))
          ((string-match single-no-arg tag)
           (insert (format "<%s%s/>%s" tag yas-html-xhtml-attr suffix)))
          ((string-match single tag)
           (yas-expand-snippet (format "<%s $1%s/>%s" tag yas-html-xhtml-attr suffix)))
          (t
           (yas-expand-snippet (format "<%s>%s</%s>%s"
                                       tag
                                       (if yas-html-just-like-tm "$1" "$0")
                                       (replace-regexp-in-string "\\\W.*" "" tag)
                                       suffix))))))

(defun yas-html-wrap-each-line-in-openclose-tag ()
  (let* ((mirror "${1:$(yas-html-first-word yas-text)}")
         (yas-html-wrap-newline (when (string-match "\n" yas-selected-text) "\n"))
         (template (concat (format "<${1:%s}>" (or yas-html-default-tag "p"))
                           yas-selected-text
                           "</" mirror ">")))
    (setq template (replace-regexp-in-string "\n" (concat "</" mirror ">\n<$1>") template))
    (yas-expand-snippet template)))

(defun yas-html-toggle-wrap (string wrap)
  (or (and string
           (string-match (format "<%s>\\(.*\\)</%s>" wrap wrap)
                         string)
           (match-string 1 string))
      (concat wrap string wrap)))

(defun yas-html-between-tag-pair-p ()
  (save-excursion
    (backward-word)
    (looking-at "\\\w+></\\\w+>")))

(defun yas-html-id-from-string (string)
  (replace-regexp-in-string " " "_" (downcase string)))

(defun yas-html-tidy ()
  (interactive)
  (let ((start (or (and mark-active
                        (region-beginning))
                   (point-min)))
        (end (or (and mark-active
                      (region-end))
                 (point-max)))
        (orig (point))
        (orig-line (count-screen-lines (window-start) (line-beginning-position))))
    (shell-command-on-region start end "tidy" (current-buffer) t (get-buffer-create "*tidy errors*") t)
    (goto-char (min (point-max) orig))
    (recenter (1- orig-line))))

(defun yas-html-tag-description ()
  (interactive)
  (let* ((tag-at-point (sgml-beginning-of-tag))
         (fragment (and tag-at-point
                        (cdr (assoc (upcase tag-at-point) yas-html-tag-description-urls)))))
    (if fragment
        (browse-url (concat "http://www.w3.org/TR/html4/index/"
                            fragment))
      (if tag-at-point
          (message "No documentation for " tag-at-point)
        (message "Not on a HTML tag.")))))

(defvar yas-html-tag-description-urls
  '(("A"           . "../struct/links.html#edef-A")
    ("ABBR"        . "../struct/text.html#edef-ABBR")
    ("ACRONYM"     . "../struct/text.html#edef-ACRONYM")
    ("ADDRESS"     . "../struct/global.html#edef-ADDRESS")
    ("APPLET"      . "../struct/objects.html#edef-APPLET")
    ("AREA"        . "../struct/objects.html#edef-AREA")
    ("B"           . "../present/graphics.html#edef-B")
    ("BASE"        . "../struct/links.html#edef-BASE")
    ("BASEFONT"    . "../present/graphics.html#edef-BASEFONT")
    ("BDO"         . "../struct/dirlang.html#edef-BDO")
    ("BIG"         . "../present/graphics.html#edef-BIG")
    ("BLOCKQUOTE"  . "../struct/text.html#edef-BLOCKQUOTE")
    ("BODY"        . "../struct/global.html#edef-BODY")
    ("BR"          . "../struct/text.html#edef-BR")
    ("BUTTON"      . "../interact/forms.html#edef-BUTTON")
    ("CAPTION"     . "../struct/tables.html#edef-CAPTION")
    ("CENTER"      . "../present/graphics.html#edef-CENTER")
    ("CITE"        . "../struct/text.html#edef-CITE")
    ("CODE"        . "../struct/text.html#edef-CODE")
    ("COL"         . "../struct/tables.html#edef-COL")
    ("COLGROUP"    . "../struct/tables.html#edef-COLGROUP")
    ("DD"          . "../struct/lists.html#edef-DD")
    ("DEL"         . "../struct/text.html#edef-del")
    ("DFN"         . "../struct/text.html#edef-DFN")
    ("DIR"         . "../struct/lists.html#edef-DIR")
    ("DIV"         . "../struct/global.html#edef-DIV")
    ("DL"          . "../struct/lists.html#edef-DL")
    ("DT"          . "../struct/lists.html#edef-DT")
    ("EM"          . "../struct/text.html#edef-EM")
    ("FIELDSET"    . "../interact/forms.html#edef-FIELDSET")
    ("FONT"        . "../present/graphics.html#edef-FONT")
    ("FORM"        . "../interact/forms.html#edef-FORM")
    ("FRAME"       . "../present/frames.html#edef-FRAME")
    ("FRAMESET"    . "../present/frames.html#edef-FRAMESET")
    ("H1"          . "../struct/global.html#edef-H1")
    ("H2"          . "../struct/global.html#edef-H2")
    ("H3"          . "../struct/global.html#edef-H3")
    ("H4"          . "../struct/global.html#edef-H4")
    ("H5"          . "../struct/global.html#edef-H5")
    ("H6"          . "../struct/global.html#edef-H6")
    ("HEAD"        . "../struct/global.html#edef-HEAD")
    ("HR"          . "../present/graphics.html#edef-HR")
    ("HTML"        . "../struct/global.html#edef-HTML")
    ("I"           . "../present/graphics.html#edef-I")
    ("IFRAME"      . "../present/frames.html#edef-IFRAME")
    ("IMG"         . "../struct/objects.html#edef-IMG")
    ("INPUT"       . "../interact/forms.html#edef-INPUT")
    ("INS"         . "../struct/text.html#edef-ins")
    ("ISINDEX"     . "../interact/forms.html#edef-ISINDEX")
    ("KBD"         . "../struct/text.html#edef-KBD")
    ("LABEL"       . "../interact/forms.html#edef-LABEL")
    ("LEGEND"      . "../interact/forms.html#edef-LEGEND")
    ("LI"          . "../struct/lists.html#edef-LI")
    ("LINK"        . "../struct/links.html#edef-LINK")
    ("MAP"         . "../struct/objects.html#edef-MAP")
    ("MENU"        . "../struct/lists.html#edef-MENU")
    ("META"        . "../struct/global.html#edef-META")
    ("NOFRAMES"    . "../present/frames.html#edef-NOFRAMES")
    ("NOSCRIPT"    . "../interact/scripts.html#edef-NOSCRIPT")
    ("OBJECT"      . "../struct/objects.html#edef-OBJECT")
    ("OL"          . "../struct/lists.html#edef-OL")
    ("OPTGROUP"    . "../interact/forms.html#edef-OPTGROUP")
    ("OPTION"      . "../interact/forms.html#edef-OPTION")
    ("P"           . "../struct/text.html#edef-P")
    ("PARAM"       . "../struct/objects.html#edef-PARAM")
    ("PRE"         . "../struct/text.html#edef-PRE")
    ("Q"           . "../struct/text.html#edef-Q")
    ("S"           . "../present/graphics.html#edef-S")
    ("SAMP"        . "../struct/text.html#edef-SAMP")
    ("SCRIPT"      . "../interact/scripts.html#edef-SCRIPT")
    ("SELECT"      . "../interact/forms.html#edef-SELECT")
    ("SMALL"       . "../present/graphics.html#edef-SMALL")
    ("SPAN"        . "../struct/global.html#edef-SPAN")
    ("STRIKE"      . "../present/graphics.html#edef-STRIKE")
    ("STRONG"      . "../struct/text.html#edef-STRONG")
    ("STYLE"       . "../present/styles.html#edef-STYLE")
    ("SUB"         . "../struct/text.html#edef-SUB")
    ("SUP"         . "../struct/text.html#edef-SUP")
    ("TABLE"       . "../struct/tables.html#edef-TABLE")
    ("TBODY"       . "../struct/tables.html#edef-TBODY")
    ("TD"          . "../struct/tables.html#edef-TD")
    ("TEXTAREA"    . "../interact/forms.html#edef-TEXTAREA")
    ("TFOOT"       . "../struct/tables.html#edef-TFOOT")
    ("TH"          . "../struct/tables.html#edef-TH")
    ("THEAD"       . "../struct/tables.html#edef-THEAD")
    ("TITLE"       . "../struct/global.html#edef-TITLE")
    ("TR"          . "../struct/tables.html#edef-TR")
    ("TT"          . "../present/graphics.html#edef-TT")
    ("U"           . "../present/graphics.html#edef-U")
    ("UL"          . "../struct/lists.html#edef-UL")
    ("VAR"         . "../struct/text.html#edef-VAR")))

;;
;;
;; Substitutions for: content
;; # as in Snippets/Emphasize.yasnippet
;; ${TM_SELECTED_TEXT/\A<em>(.*)<\/em>\z|.*/(?1:$1:<em>$0<\/em>)/m}                    =yyas> `(yas-html-toggle-wrap yas-selected-text "em")`
;; ${TM_SELECTED_TEXT/\A<strong>(.*)<\/strong>\z|.*/(?1:$1:<strong>$0<\/strong>)/m}    =yyas> `(yas-html-toggle-wrap yas-selected-text "strong")`
;; ${1/\s.*//}                                                                         =yyas> ${1:$(replace-regexp-in-string "[\s\t\n].*" "" yas-text)}
;; ${1/[[:alpha:]]+|( )/(?1:_:\L$0)/g}                                                 =yyas> ${1:$(replace-regexp-in-string " " "_" (downcase yas-text))}
;; ${TM_XHTML}                                                                         =yyas> `yas-html-xhtml-attr`


;; # as in Commands/Preview in All Active Browsers.yasnippet
;; 970EE6B4-A091-11D9-A5A2-000D93C8BE28                                                       =yyas> (browse-url-of-buffer)
;; 637CEA2B-578C-429C-BB74-30E8D42BFA22                                                       =yyas> (yas-html-tag-description)
;; 2ED44A32-C353-447F-BAE4-E3522DB6944D                                                       =yyas> (yas-html-insert-tag-pair-snippet)
;; 991E7EBD-F3F5-469A-BA01-DC30E04AD472                                                       =yyas> (yas-html-wrap-each-line-in-openclose-tag)

;; Substitutions for: binding
;; 
;; # as in Snippets/Strong.yasnippet
;; @b                                                                                         =yyas> s-b
;; 
;; # as in Snippets/Emphasize.yasnippet
;; ^@i                                                                                        =yyas>   
;; @i                                                                                         =yyas> s-i
;; 
;; # as in Snippets/Wrap Selection In Tag.yasnippet
;; ^W                                                                                         =yyas> C-c M-w
;; 
;; # as in Commands/Insert Tag Pair.yasnippet
;; ^<                                                                                         =yyas> C-<
;;
;; # as in Commands/Documentation for Tag.yasnippet
;; ^h                                                                                         =yyas> C-c M-h
;; 
;; # as in Commands/Wrap Each Selected Line in OpenClose Tag.yasnippet
;; ^@W                                                                                        =yyas> C-c M-W
;; 
;; # as in Snippets/XHTML &nbsp NonBreakingSpace.yasnippet
;; ~                                                                                          =yyas> (yas-unknown)
;; 
;; # as in Commands/Insert Entity.yasnippet
;; @&                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Refresh All Active Browsers.yasnippet
;; @r                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Persistent Include.yasnippet
;; ^@i                                                                                        =yyas> (yas-unknown)
;; 
;; # as in Commands/CodeCompletion HTML Tags.yasnippet
;; ~                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Update Includes.yasnippet
;; ^@u                                                                                        =yyas> (yas-unknown)
;; 
;; # as in Macros/Delete whitespace between tags.yasnippet
;; ^~                                                                                      =yyas> (yas-unknown)
;; 
;; # as in Commands/Tidy.yasnippet
;; ^H                                                                                         =yyas> (yas-unknown)
;; 
;;
;; --**--
;; Automatically generated code, do not edit this part
;; 
;; Translated menu
;; 


;; Unknown substitutions
;; 
;; Substitutions for: content
;; 
;; 

;; Substitutions for: condition
;; 
;; 

;; Substitutions for: binding
;; 
;; 

;; .yas-setup.el for html-mode ends here
