;; .yas-setup.el for ruby-mode
;; -*- coding: utf-8 -*-
;;
(defvar yas-ruby-snippet-open-paren " "
  "The open parenthesis used in ruby-mode snippets. Normally blank but could be (")
(defvar yas-ruby-snippet-close-paren " "
  "The close parenthesis used in ruby-mode snippets. Normally blank but could be )")
(defvar yas-ruby-shebang-args " -wKU"
  "Arguments for the ruby shebang line.")

(defun yas-ruby-infer-class-name ()
  "Infer the class name from the buffer. Thanks to hitesh <hitesh.jasani@gmail.com>"
  (if buffer-file-name
      (let ((fn (capitalize (file-name-nondirectory
                             (file-name-sans-extension
                              (buffer-file-name))))))
        (cond
         ((string-match "_" fn) (replace-match "" nil nil fn))
         (t fn)))
    "SomeClass"))

(defun yas-ruby-chomp (x)
  "Chomp string X, return nil if X became empty"
  (let ((len (length x))
        (start 0)
        (end (1- (length x))))
    (unless (zerop len)
      (while (and (< start len)
                  (memq (aref x start)
                        '(?  ?\t ?\n)))
        (setq start (1+ start)))
      (while (and (> end start)
                  (memq (aref x end)
                        '(?  ?\t ?\n)))
        (setq end (1- end)))
      (unless (<= end start)
        (substring x start (1+ end))))))

(defvar yas-ruby-block-start-regexp "\\(^\\|[\s\t\n^]\\)\\(do\\)[\s\t\n]\\(|.*|\\)?")

(defun yas-ruby-toggle-single-multi-line-block ()
  "Toggle \"do .. end\" blocks into  \"{ .. }\" blocks back and forth."
  ;;
  ;; TODO: Some code to be refactored here.
  ;; 
  ;; FIXME: correctly detect statements in { .. } block, split-string(";") is no good
  ;;
  (interactive)
  (let* ((do-block-bounds (save-excursion
                            (when (or (save-excursion (beginning-of-line)
                                                      (looking-at yas-ruby-block-start-regexp))
                                      (save-excursion (ruby-beginning-of-block)
                                                      (looking-at yas-ruby-block-start-regexp)))
                              (cons (match-beginning 1)
                                    (progn (goto-char (match-beginning 1))
                                           (ruby-end-of-block) (point))))))
         (brace-block-bounds (condition-case nil
                                 (let ((syntax-info (syntax-ppss)))
                                   (if (fourth syntax-info)
                                       (goto-char (ninth syntax-info)))
                                   (while (progn (up-list -1) (not (eq (char-after) ?{))))
                                   (cons (point)
                                         (progn (forward-sexp) (point))))
                               (error nil)))
         (block-region)
         (statements))
    (if (and do-block-bounds brace-block-bounds)
        (if (< (car do-block-bounds) (car brace-block-bounds))
            (setq do-block-bounds nil)
          (setq brace-block-bounds nil)))
    (cond (do-block-bounds
           (goto-char (car do-block-bounds))
           (setq block-region (buffer-substring-no-properties (+ 2 (car do-block-bounds)) (cdr do-block-bounds)))
           (delete-region (car do-block-bounds) (+ 3 (cdr do-block-bounds)))
           (insert "{")
           (when (string-match "\\(|.*|\\).*" block-region)
             (insert " " (match-string 1 block-region))
             (setq block-region (substring block-region (match-end 1))))
           (setq statements (remove nil (mapcar #'yas-ruby-chomp
                                                (split-string block-region "\n"))))
           (mapc #'(lambda (string)
                     (insert " " string)
                     (if (member (aref string (1- (length string))) '(?;
                                                                      ?|))
                         (insert " ")
                       (insert ";")))
                 statements)
           (when statements (delete-backward-char 1))
           (save-excursion
             (insert " }")))
          (brace-block-bounds
           ;; (message "found a brace block")
           (goto-char (car brace-block-bounds))
           (setq block-region (buffer-substring (1+ (car brace-block-bounds)) (1- (cdr brace-block-bounds))))
           (delete-region (car brace-block-bounds) (cdr brace-block-bounds))
           (insert "do")
           (when (string-match "\\(|.*|\\).*" block-region)
             (insert " " (match-string 1 block-region))
             (setq block-region (substring block-region (match-end 1))))
           (setq statements (remove nil (mapcar #'yas-ruby-chomp
                                                (split-string block-region ";"))))
           (mapc #'(lambda (string)
                     (insert "\n" string)
                     (indent-according-to-mode))
                 statements)
           (unless statements (insert "\n") (indent-according-to-mode))
           (save-excursion
             (insert "\nend")
             (indent-according-to-mode)))
          (t
           (message "No enclosing block found.")))))

(defvar yas-ruby-require-regexps
  '(("abbrev"                            . ("abbrev"))
    ("base64"                            . ("Base64"))
    ("benchmark"                         . ("Benchmark"))
    ("bigdecimal"                        . ("BigDecimal"))
    ("bigdecimal/math"                   . ("BigMath"))
    ("cgi"                               . ("CGI"))
    ("complex"                           . ("Complex"))
    ("csv"                               . ("CSV"))
    ("curses"                            . ("Curses"))
    ("date"                              . ("Date(?:Time)?"))
    ("dbm"                               . ("DBM"))
    ("delegate"                          . ("DelegateClass" "Delegator" "SimpleDelegator "))
    ("digest"                            . ("MD5" "SHA1"))
    ("dl"                                . ("DL"))
    ("enumerator"                        . ("(?:enum|each)_(?:cons|slice)" "enum_(?:for|with_index)" "to_enum "))
    ("erb"                               . ("ERB"))
    ("etc"                               . ("Etc"))
    ("fcntl"                             . ("Fcntl"))
    ("fileutils"                         . ("FileUtils"))
    ("find"                              . ("Find(?:\.|::)find"))
    ("forwardable"                       . ("(?:Single)?Forwardable"))
    ("gdbm"                              . ("GDBM"))
    ("generator"                         . ("Generator" "SyncEnumerator"))
    ("getoptlong"                        . ("GetoptLong"))
    ("gserver"                           . ("GServer"))
    ("iconv"                             . ("Iconv"))
    ("ipaddr"                            . ("IpAddr"))
    ("logger"                            . ("Logger"))
    ("matrix"                            . ("Matrix" "Vector"))
    ("monitor"                           . ("Monitor(?:Mixin)?"))
    ("net/ftp"                           . ("Net::FTP"))
    ("net/http"                          . ("Net::HTTP"))
    ("net/imap"                          . ("Net::IMAP"))
    ("net/pop"                           . ("Net::(?:APOP|POP3)"))
    ("net/smtp"                          . ("Net::SMTP"))
    ("net/telnet"                        . ("Net::Telnet"))
    ("nkf"                               . ("NKF"))
    ("observer"                          . ("Observable"))
    ("open3"                             . ("Open3"))
    ("optparse"                          . ("OptionParser"))
    ("ostruct"                           . ("OpenStruct"))
    ("pathname"                          . ("Pathname"))
    ("ping"                              . ("Ping"))
    ("pp"                                . ("pp"))
    ("pstore"                            . ("PStore"))
    ("rational"                          . ("Rational"))
    ("rdoc/usage"                        . ("RDoc(?:\.|::)usage"))
    ("rdoc/markup/simple_markup"         . ("SM::SimpleMarkup"))
    ("rdoc/markup/simple_markup/to_html" . ("SM::SimpleMarkup"))
    ("rdoc/usage"                        . ("RDoc(?:\.|::)usage"))
    ("resolv"                            . ("Resolv"))
    ("rexml/document"                    . ("REXML"))
    ("rinda/tuplespace"                  . ("Rinda::TupleSpace(?:Proxy)?"))
    ("rinda/ring"                        . ("Rinda::Ring(?:Finger|Server)?"))
    ("rss"                               . ("RSS"))
    ("scanf"                             . ("scanf"))
    ("sdbm"                              . ("SDBM"))
    ("set"                               . ("(?:Sorted)?Set"))
    ("singleton"                         . ("Singleton"))
    ("soap"                              . ("SOAP"))
    ("socket"                            . (" (?:TCP|UNIX)(?:Socket|Server)" "(?:UDP)?Socket"))
    ("stringio"                          . ("StringIO"))
    ("strscan"                           . ("StringScanner"))
    ("syslog"                            . ("Syslog"))
    ("tempfile"                          . ("Tempfile"))
    ("test/unit"                         . ("Test::Unit"))
    ("thread"                            . (" ConditionVariable" "Mutex" "(?:Sized)?Queue "))
    ("time"                              . ("Time(?:\.|::)parse"))
    ("timeout"                           . ("Timeout(?:\.|::)timeout"))
    ("tk"                                . ("TK"))
    ("tmpdir"                            . ("Dir(?:\.|::)tmpdir"))
    ("tracer"                            . ("Tracer"))
    ("tsort"                             . ("TSort"))
    ("uri"                               . ("URI"))
    ("weakref"                           . ("WeakRef"))
    ("webrick"                           . ("WEBrick"))
    ("Win32API"                          . ("Win32(?:API)?"))
    ("win32ole"                          . ("WIN32OLE"))
    ("wsdl"                              . ("WSDL"))
    ("xmlrpc"                            . ("XMLRPC"))
    ("yaml"                              . ("YAML"))
    ("zlib"                              . ("Zlib"))))

(defun yas-ruby-require (package)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward-regexp (format "^[\s\t]*require[( ][ ]*\"%s\"[ )]*$"
                                           package) nil t)
      (unless (search-forward-regexp "^[\s\t]*require.*\n" nil t)
        (search-forward-regexp "^[\s\t]*[^#]" nil t)
        (goto-char (line-beginning-position)))
      (insert "require \"" package "\"\n"))))

(defun yas-ruby-pipe-through-xmpfilter ()
  (interactive)
  (let ((start (or (and mark-active
                        (region-beginning))
                   (point-min)))
        (end (or (and mark-active
                      (region-end))
                 (point-max)))
        (orig (point))
        retval
        (orig-line (count-screen-lines (window-start) (line-beginning-position))))
    
    (unless (zerop (shell-command-on-region start end "xmpfilter" (get-buffer-create "*xmpfilter*") t (get-buffer-create "*xmpfilter errors*") t))
      (undo)
      )
    (goto-char (min (point-max) orig))
    (recenter orig-line)
    retval))

(put (intern "ruby-thing") 'bounds-of-thing-at-point 'yas-ri-ruby-thing-bounds)
(defun yas-ri-ruby-thing-bounds ()
  (let ((start (point))
        (end (point)))
    (save-excursion
      (while (not (and (zerop (skip-syntax-forward "\w\_"))
                       (zerop (skip-chars-forward "#:"))))
        (setq end (point)))
      (while (not (and (zerop (skip-syntax-backward "\w\_"))
                       (zerop (skip-chars-backward "#:"))))
        (setq start (point))))
    (unless (= start end)
      (cons start end))))

(defvar yas-ri-history nil
  "History of yas-ri queries.")
(defvar yas-ri-executable "ri")
(require 'ansi-color)
(defun yas-ri (query)
  (interactive (list (read-from-minibuffer "Ri query: "
                                           (thing-at-point 'ruby-thing)
                                           nil
                                           nil
                                           'ri-history)))
  (with-current-buffer (get-buffer-create "*Ri*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory "~")
    (setq buffer-read-only nil)
    (shell-command (concat yas-ri-executable " -f ansi " query) "*Ri*")
    (ansi-color-apply-on-region (point-min) (point-max))
    (yas-ri-mode)
    (display-buffer (current-buffer)))
  t)

(defun yas-ri-mode ()
  "Mode for viewing Ruby documentation."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "RET") 'yas-ri)
  (setq mode-name "ri")
  (setq major-mode 'yas-ri-mode)
  (setq buffer-read-only t)
  (run-hooks 'yas-ri-mode-hook))

;; conditions
;; 
(yas-define-condition-cache yas-ruby-in-interpolated-string-p (member (fourth (syntax-ppss)) (list ?\" ?\`)))
(yas-define-condition-cache yas-ruby-in-comment-p (fifth (syntax-ppss)))
(yas-define-condition-cache yas-ruby-in-string-p (fourth (syntax-ppss)))
(yas-define-condition-cache yas-ruby-end-is-block-end-p
                            (save-excursion
                              (ruby-backward-sexp)
                              (not (eq (point) (point-min)))))

(provide 'yas-ruby)

;; My work in progress substitutions
;;
;; Substitutions for: content
;;
;; ${1/.+/(/}                                                                        =yyas> ${1:$(and (yas-text) "(")}
;; ${1/.+/)/}                                                                        =yyas> ${1:$(and (yas-text) ")")}
;; ${2/.+/ => /}                                                                     =yyas> ${2:$(and (yas-text) " => ")}
;; ${1:${TM_FILENAME/\.\w+//}                                                        =yyas> ${1:$(and buffer-file-name (file-name-sans-extension buffer-file-name))}
;; ${1/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${1:$(and (string-match "[^\s\t]" yas-text) "(" )}
;; ${1/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${1:$(and (string-match "[^\s\t]" yas-text) ")" )}
;; ${2/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas-text) "(" )}
;; ${2/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas-text) ")" )}
;; ${3/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas-text) "(" )}
;; ${3/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas-text) ")" )}
;; ${2/^\s*$|(.*\S.*)/(?1: )/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas-text) " " )}
;; ${3/^\s*$|(.*\S.*)/(?1: )/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas-text) " " )}
;; ${3/(^[rwab+]+$)|.*/(?1:, ")/}                                                    =yyas> ${3:$(and (string-match "^[rwab+]+$" yas-text) ", \\"" )}
;; ${3/(^[rwab+]+$)|.*/(?1:")/}                                                      =yyas> ${3:$(and (string-match "^[rwab+]+$" yas-text) "\\"" )}
;; ${3/^\s*$|(.*\S.*)/(?1:, )/}                                                      =yyas> ${3:$(and (string-match "[^\s\t]" (yas-text) ", ")}
;; ${TM_SELECTED_TEXT/([\t ]*).*/$1/m}                                               =yyas>
;; ${TM_SELECTED_TEXT/(\A.*)|(.+)|\n\z/(?1:$0:(?2:\t$0))/g}                          =yyas> `yas-selected-text`
;; (yas-multi-line-unknown BF487539-8085-4FF4-8601-1AD20FABAEDC)                     =yyas> `(yas-ruby-infer-class-name)`
;; (yas-multi-line-unknown 2B73EC5F-06D2-460C-A14F-6FA05AFCF0CC)                     =yyas> `(yas-ruby-infer-class-name)`
;; 
;; ${TM_FILENAME/(?:\A|_)([A-Za-z0-9]+)(?:\.rb)?/(?2::\u$1)/g}                       =yyas> `(yas-ruby-infer-class-name)`
;; 
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}             =yyas> ${1:$(and (yas-text) "|")}
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1: |)/}            =yyas> ${1:$(and (yas-text) " |")}
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}            =yyas> ${1:$(and (yas-text) "| ")}
;;
;; ${1/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}   =yyas> ${1:$(and (yas-text) "|")}
;; ${1/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}  =yyas> ${1:$(and (yas-text) "| ")}
;; ${2/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}   =yyas> ${2:$(and (yas-text) "|")}
;; ${2/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}  =yyas> ${2:$(and (yas-text) "| ")}
;; 
;; ${1/([\w&&[^_]]+)|./\u$1/g}                                                       =yyas> ${1:$(replace-regexp-in-string "[_/]" "" (capitalize yas-text))}
;;
;; 7990EE60-C850-4779-A8C0-7FD2C853B99B               =yyas> (yas-ruby-toggle-single-multi-line-block)
;; 7E084412-80E6-4B70-8092-C03D1ECE4CD2               =yyas> (yas-ruby-require "eac")(yas-expand-uuid 'ruby-mode "FDD73070-6D32-4301-A86A-C55B77C3D8ED")
;; FBFC214F-B019-4967-95D2-028F374A3221               =yyas> (yas-ruby-pipe-through-xmpfilter)
;; 63F3B3B7-CBE2-426B-B551-657733F3868B               =yyas> (call-interactively (if (featurep 'yari) 'yari 'yas-ri))

;;
;; `[[ $TM_LINE_INDEX != 0 ]] && echo; echo`                                         =yyas> `(concat (if (eq 0 current-line) "\n" "") "\n")`
;; `snippet_paren.rb`                                                                =yyas> `yas-ruby-snippet-open-paren`
;; `snippet_paren.rb end`                                                            =yyas> `yas-ruby-snippet-close-paren`
;; ${TM_RUBY_SWITCHES: -wKU}                                                         =yyas> `yas-ruby-shebang-args`
;; 
;; Substitutions for: condition
;;
;; 7990EE60-C850-4779-A8C0-7FD2C853B99B                                              =yyas> 'force-in-comment
;; FBFC214F-B019-4967-95D2-028F374A3221                                              =yyas> 'force-in-comment
;; 88BC3896-DC39-4307-A271-21D33340F15A                                              =yyas> 'force-in-comment
;; 0F940CBC-2173-49FF-B6FD-98A62863F8F2                                              =yyas> 'force-in-comment
;; 451A0596-1F72-4AFB-AF2F-45900FABB0F7                                              =yyas> (not (yas-ruby-end-is-block-end-p))
;; (string.quoted.double.ruby|string.interpolated.ruby) - string source              =yyas> (and (yas-ruby-in-interpolated-string-p) 'force-in-comment)
;; text.html.ruby, text.html source.ruby                                             =yyas> (yas-unimplemented)
;; text.html, source.yaml, meta.erb                                                  =yyas> (yas-unimplemented)
;; keyword.control.start-block.ruby, meta.syntax.ruby.start-block                    =yyas>
;; 
;; Substitutions for: binding
;;
;; # as in Commands/New Method.yasnippet
;; $                                                                               =yyas> C-c M-m
;; ^W                                                                                =yyas> C-c M-w
;; #                                                                                 =yyas> #
;; ^{                                                                                =yyas> C-c M-{
;; @R                                                                                =yyas> C-c M-R
;; @r                                                                                =yyas> C-c M-r
;; ^R                                                                                =yyas> C-c M-S-r
;; @i                                                                                =yyas> s-i
;; @b                                                                                =yyas> s-b
;; ^@E                                                                               =yyas> C-c M-e
;; ^:                                                                                =yyas> C-c M-:
;; ^>                                                                                =yyas> C-c M->
;; ^h                                                                                =yyas> C-c M-h
;;
;;
;; # as in Commands/Enclose in + (RDoc comments).yasnippet
;; @k                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Check Ruby Syntax.yasnippet
;; ^V                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Omit from RDoc.yasnippet
;; ^@O                                                                                        =yyas> (yas-unknown)
;; 
;; # as in Commands/Enclose in (RDoc comments).yasnippet
;; @b                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Snippets/hash pointer.yasnippet
;; ^l                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Make Destructive Call.yasnippet
;; ^!                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Toggle Quote Style.yasnippet
;; ^"                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Open Require.yasnippet
;; @D                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Execute Line with Ruby.yasnippet
;; ^E                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Completion Ruby (rcodetools).yasnippet
;; ~                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Macros/Delete forwardbackward.yasnippet
;;                                                                                           =yyas> (yas-unknown)
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

;; .yas-setup.el for ruby-mode ends here
