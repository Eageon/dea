;; .yas-setup.el for rails-mode
(defvar yas-rails-root-cache nil)

(defun yas-rails-online-doc ()
  (interactive)
  (browse-url (format "http://apidock.com/rails/search/quick?query=%s" (read-from-minibuffer "Word: " (thing-at-point 'word)))))

(if (require 'rhtml-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . yas-rails-erb-mode)))


(define-derived-mode yas-rails-erb-mode
  nxml-mode "eRB"
  "Embedded Ruby Mode, very thin layer over `nxml-mode'."
  (add-to-list (make-local-variable 'yas-extra-modes) 'html-mode)
  (rng-set-vacuous-schema)
  (message "hey erb mode"))

(defvar yas-rails-erb-font-lock-keywords
  '(("\\(<%=\\)\\(.*+\\)\\(%>\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-string-face)
     (3 font-lock-function-name-face))
    ("\\(<%\\)\\(.*+\\)\\(%>\\)"
     (1 font-lock-variable-name-face)
     (2 font-lock-string-face)
     (3 font-lock-variable-name-face)))
  "(Crummy) font lock highlighting for ERB constructs.."
  )
(font-lock-add-keywords 'yas-rails-erb-mode yas-rails-erb-font-lock-keywords)

;; stolen from rinari-mode's rinari-root
(defun yas-rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (or (and (featurep 'rinari) (rinari-root dir))
      yas-rails-root-cache
      (if (file-exists-p (expand-file-name
                          "environment.rb" (expand-file-name "config" dir)))
          (set (make-local-variable 'yas-rails-root-cache) dir)
        (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
          ;; regexp to match windows roots, tramp roots, or regular posix roots
          (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:\\|^/$\\)" dir)
            (yas-rails-root new-dir))))))

(defun yas-rails-find-alternate-file ()
  (if (featurep 'rinari)
      (cond ((yas-rails-view-p)
             (rinari-find-model))
            ((yas-rails-model-p)
             (rinari-find-controller))
            ((yas-rails-controller-p)
             (rinari-find-view))
            (t
             (message "oops, have to improve `yas-rails-find-alternate-file'")))
      (yas-unimplemented)))
  
;; stolen from rinari-mode's rinari-extract-partial
(defun yas-rails-extract-partial (begin end partial-name)
  (interactive "r\nsName your partial: ")
  (let* ((path (buffer-file-name)) ending)
    (if (string-match "view" path)
	(let ((ending (and (string-match ".+?\\(\\.[^/]*\\)$" path)
			   (match-string 1 path)))
	      (partial-name
	       (replace-regexp-in-string "[[:space:]]+" "_" partial-name)))
	  (kill-region begin end)
	  (if (string-match "\\(.+\\)/\\(.+\\)" partial-name)
	      (let ((default-directory (expand-file-name (match-string 1 partial-name)
							 (expand-file-name ".."))))
		(find-file (concat "_" (match-string 2 partial-name) ending)))
	    (find-file (concat "_" partial-name ending)))
	  (yank) (pop-to-buffer nil)
	  (insert (concat "<%= render :partial => '" partial-name "' %>\n")))
      (message "not in a view"))))
;;;
;;; The TextMate "intelligent" migration snippet
;;
(defvar yas-rails-intelligent-migration-snippet-bits
      '((:rename_column . ((:up   . "rename_column :${1:table_name}, :${2:column_name}, :${3:new_column_name}$0")
                           (:down . "rename_column :$1, :$3, :$2" )))

        (:rename_column_continue . ((:up   . "rename_column :${1:table_name}, :${2:column_name}, :${3:new_column_name}\nmncc$0")
                                    (:down . "rename_column :$1, :$3, :$2" )))

        (:rename_table . ((:up   . "rename_table :${1:old_table_name}, :${2:new_table_name}$0")
                          (:down . "rename_table :$2, :$1" )))

        (:rename_table_continue . ((:up   . "rename_table :${1:old_table_name}, :${2:new_table_name}\nmntc$0")
                                   (:down . "rename_table :$2, :$1" )))

        (:add_remove_column . ((:up   . "add_column :${1:table_name}, :${2:column_name}, :${3:string}$0")
                               (:down . "remove_column :$1, :$2" )))
        
        (:add_remove_column_continue . ((:up   . "add_column :${1:table_name}, :${2:column_name}, :${3:string}\nmarcc$0")
                                        (:down . "remove_column :$1, :$2" )))
        
        (:remove_add_column . ((:up   . "remove_column :${1:table_name}, :${2:column_name}$0")
                               (:down . "add_column :$1, :$2, :$3{string}" )))

        (:create_drop_table . ((:up   . "create_table :${1:table_name}, :force . true do |t|\nt.$0\nt.timestamps\nend")
                               (:down . "drop_table :$1" )))

        (:change_change_table . ((:up   . "change_table :${1:table_name} do |t|\nt.$0\nend")
                                 (:down . "change_table :$1 do |t|\nend" )))

        (:add_remove_index . ((:up   . "add_index :${1:table_name}, :${2:column_name}$0")
                              (:down . "remove_index :$1, :$2" )))

        (:add_remove_unique_index . ((:up   . "add_index :${1:table_name}, ${2:[:${3:column_name}${4:, :${5:column_name}}]}, :unique . true$0")
                                     (:down . "remove_index :$1, :column . $2" )))

        (:add_remove_named_index . ((:up   . "add_index :${1:table_name}, [:${2:column_name}${3:, :${4:column_name}}], :name . \"${5:index_name}\"${6:, :unique . true}$0")
                                    (:down . "remove_index :$1, :name . :$5" )))))


(defun yas-rails-intelligent-migration-snippet (type)
  (let* ((start  (point))
         (end (save-excursion
                (search-forward-regexp "^\s*def\sself\.down" nil 'noerror)))
         (up (cdr (assoc :up (cdr (assoc type yas-rails-intelligent-migration-snippet-bits)))))
         (down (cdr (assoc :down (cdr (assoc type yas-rails-intelligent-migration-snippet-bits)))))
         (snippet
          (and up down start end (concat up
                                         (buffer-substring-no-properties start end)
                                         "\n" down))))
    (when snippet
      (delete-region start end)
      (yas-expand-snippet snippet))))

(yas-define-condition-cache
  yas-rails-intelligent-migration-snippet-condition-p
  "Non-nil if an \"intelligent\" migration snippet should be expanded"
  (and (yas-rails-migration-p)
       (not (yas-rails-in-create-table-p))
       (not (yas-rails-in-change-table-p))
       (yas-rails-in-ruby-block-like "self\.up")))

(defun yas-rails-in-ruby-block-like (regexp)
  (save-excursion
    (ruby-accurate-end-of-block)
    (ruby-backward-sexp)
    (search-forward-regexp regexp (line-end-position) t)))

;;; conditions
(yas-define-condition-cache
 yas-rails-in-create-table-p
 "Non-nil if point is inside a 'create_table' method call."
 (yas-rails-in-ruby-block-like "create_table"))

(yas-define-condition-cache
 yas-rails-in-change-table-p
 "Non-nil if point is inside a 'change_table' method call."
 (yas-rails-in-ruby-block-like "change_table"))

(yas-define-condition-cache
 yas-rails-model-p
 "Non-nil if the current buffer is a rails model."
 (and (yas-rails-root)
      (string-match "app/models/$" default-directory)))

(yas-define-condition-cache
 yas-rails-view-p
 "Non-nil if the current buffer is a rails view."
 (and (yas-rails-root)
      (string-match "app/views/" default-directory)))

(yas-define-condition-cache
 yas-rails-helper-p
 "Non-nil if the current buffer is a rails helper."
 (and (yas-rails-root)
      (string-match "app/helpers/" default-directory)))

(yas-define-condition-cache
 yas-rails-controller-p
"Non-nil if the current buffer is a rails controller." 
 (and (yas-rails-root)
      (string-match "app/controllers/$" default-directory)))

(yas-define-condition-cache
 yas-rails-migration-p
 "Non-nil if the current buffer is a rails migration."
 (and (yas-rails-root)
      (string-match "db/migrate/" default-directory)))

(defun yas-rails-activate-maybe ()
  (when (and yas-minor-mode
             (yas-rails-root))
    (add-to-list (make-local-variable 'yas-extra-modes) 'rails-mode)))

(defadvice cd (after yas-rails-on-cd-activate activate)
  "Add `rails-mode' to `yas-extra-modes' so that rails snippets
are recognized. Stolen from `rinari-mode' more or`' less."
  (setq yas-rails-root-cache nil)
  (yas-rails-activate-maybe))

(add-hook 'yas-minor-mode-hook 'yas-rails-activate-maybe)
;; Substitutions for: content
;; 
;; # as in Macros/Remove 3A Add Column.yasnippet
;; 809BCA42-5C49-4B08-B3C4-BB773036C086                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Add 3A Remove Named Index.yasnippet
;; A7F692C1-778A-48B8-945E-573568BA0403                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Go To Unit Test.yasnippet
;; BDBB15A4-2824-4BEC-93A5-7475F9C46A39                                                       =yyas> (if (featurep 'rinari) (rinari-find-test) (yas-unimplemented 'rinari))
;; 
;; # as in Commands/Go To File on This Line.yasnippet
;; 09BB96F2-75FD-48A7-8314-B5B56B09B477                                                       =yyas> (ffap)
;; 
;; # as in Commands/Test Uncommitted.yasnippet
;; 212C3047-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Redo Last Migration.yasnippet
;; CFDA9F62-D071-4E0F-AD10-66AE0729FFCF                                                       =yyas> (yas-rails-compile "rake")
;; 
;; # as in Commands/Documentation for Word.yasnippet
;; 32F30207-D827-46D9-889A-451C35269D52                                                       =yyas> (yas-rails-online-doc)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; 0BCF0EE2-35EE-4959-A771-E74D55271D5A                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; 275C0B86-F735-49B6-8A22-218A8F4CC2E0                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Change Change Table.yasnippet
;; 20FC02C5-32A3-4F20-B163-FF75C9FDFABF                                                       =yyas> (yas-rails-intelligent-migration-snippet :change_change_table)
;; 
;; # as in Commands/Rake Migrate.yasnippet
;; 985F56D4-82ED-4C45-8250-2ECCFC71957E                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/DB Schema Import.yasnippet
;; 6DEF923E-2347-46EC-AFBE-183D08E63DC1                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Load Fixtures (Test DB).yasnippet
;; F758BFD1-00CA-4742-BE71-032580080F5C                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; D696FA2C-785A-4B73-A2F6-F750904DD7C2                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Remove 3A Add Timestamps.yasnippet
;; E885A3E8-8020-4AC3-A25E-510B26F114B2                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Add 3A Remove Several Columns (marcc).yasnippet
;; 27A6C58A-896B-4956-BA81-D671A2EF9C7D                                                       =yyas> (yas-rails-intelligent-migration-snippet :add_remove_column_continue)
;; 
;; # as in Macros/Add 3A Remove Column.yasnippet
;; 18C76913-061C-4D65-866D-67AA3724AFEF                                                       =yyas> (yas-rails-intelligent-migration-snippet :add_remove_column)
;; 
;; # as in Commands/Go To View.yasnippet
;; EE862691-A624-4797-90CF-EDD39EFB2D8E                                                       =yyas> (if (featurep 'rinari) (rinari-find-view) (yas-unimplemented 'rinari))
;; 
;; # as in Commands/Test Plugins.yasnippet
;; 0D966168-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Change Column.yasnippet
;; 42DE1441-D1B7-4998-BAF9-16B1EC7E210C                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Make Selection in to Partial.yasnippet
;; 1DD8A214-1C97-45BA-ADEE-8F888DDE8570                                                       =yyas> (call-interactively 'yas-rails-extract-partial)
;; 
;; # as in Commands/Go To Functional Test.yasnippet
;; DFE393BE-0764-49FE-B464-6350A50921E6                                                       =yyas> (if (featurep 'rinari) (rinari-find-test) (yas-unimplemented 'rinari))
;; 
;; # as in Commands/Test Recent.yasnippet
;; 190401C2-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Test All.yasnippet
;; DC549A45-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Rename Column.yasnippet
;; AC50762C-DE40-4EB9-9A22-2F6AF2EA4EA3                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Create 3A Drop Table.yasnippet
;; 25F8F5D8-2BD1-45D8-8B2A-9F2EA4F73AA2                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Add 3A Remove Unique Index.yasnippet
;; 33057A79-677B-4DFB-99D4-1492778BDDC6                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Add 3A Remove Timestamps.yasnippet
;; 221969A1-A5EA-4A8E-8817-C74EBED63901                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Go To Helper.yasnippet
;; 51C9C27A-D931-49F9-B6D8-C0E7ABEC992D                                                       =yyas> (if (featurep 'rinari) (rinari-find-helper) (yas-unimplemented 'rinari))
;; 
;; # as in Commands/DB Schema Dump.yasnippet
;; 310C901C-EF32-4E88-938A-804ABBF8C428                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Test Functionals.yasnippet
;; F4EA552D-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Migrate to Previous Version.yasnippet
;; 9A1AE6BA-8350-4AB7-B5BD-969A7E64CF29                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Go To Model.yasnippet
;; C7151BF3-7068-4344-9B09-86F3BF4A9C63                                                       =yyas> (if (featurep 'rinari) (rinari-find-model) (yas-unimplemented 'rinari))
;; 
;; # as in Macros/Drop 3A Create Table.yasnippet
;; A2135370-67A1-488D-B43C-B4F221127C2F                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Change Column Default.yasnippet
;; A219EBB8-004A-4012-B5B2-232C9A5C94F8                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Add 3A Remove Index.yasnippet
;; 95F83E1D-5B03-424F-8BEC-8AF66C8939BC                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Load Fixtures.yasnippet
;; 5EEA0C71-B34B-4408-953B-F47AAD343CCC                                                       =yyas> (yas-unknown)
;; 

;; 
;; # as in Commands/Clone Development DB to Test DB.yasnippet
;; 6F2AB859-46E3-4FF5-A9A7-E9A813AB5DE1                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns.yasnippet
;; F03162DE-9DB6-417B-9DD7-52D9F11EA736                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Go To Stylesheet.yasnippet
;; B207BBD4-D6AA-41E9-9530-27210F2D7B66                                                       =yyas> (if (featurep 'rinari) (rinari-find-stylesheet) (yas-unimplemented 'rinari))
;; 
;; # as in Commands/Go To Javascript.yasnippet
;; B078346F-61D8-4E75-9427-80720FBC67F7                                                       =yyas> (if (featurep 'rinari) (rinari-find-javascript) (yas-unimplemented 'rinari))
;; 
;; # as in Commands/Rake Migrate to Version.yasnippet
;; 07C696F8-79F5-4E0B-9EE9-03B693A54ABB                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Go To Alternate File.yasnippet
;; 9453F0B3-B946-445F-BDB0-B01DE70732FC                                                       =yyas> (yas-rails-find-alternate-file) 
;; 
;; # as in Commands/View demo help.yasnippet
;; 964436B8-E578-11DC-8177-00112475D960                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Go to Fixture.yasnippet
;; 638D94A4-BDFC-4FE9-8909-9934F3FD2899                                                       =yyas> (if (featurep 'rinari) (rinari-find-fixture) (yas-unimplemented 'rinari))
;; 
;; # as in Macros/Rename Table.yasnippet
;; FD8CC811-2AD3-480F-B975-DF959DC96C67                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns (mncc).yasnippet
;; 04A86178-71B1-430A-A06D-DFF7C9A338B5                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Generate.yasnippet
;; 4904EDC7-5ED3-4132-AAB2-C2AD87C97EFE                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Test Integration.yasnippet
;; 04A30A4D-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Show DB Schema.yasnippet
;; 1970AE74-3949-40B3-B263-727AA3FF167A                                                       =yyas> (yas-unknown)
;; 
;; # as in Macros/Add 3A Remove Several Columns.yasnippet
;; 7BC860E6-7561-4E6E-983B-507D7A6F6228                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Install Bundled Plugin.yasnippet
;; 46ECE243-0448-4A64-A223-27CC21E7704D                                                       =yyas> (yas-unknown)
;; 
;; # as in Commands/Go To File.yasnippet
;; 0CCC8443-40F3-4BAB-9440-D737562B5F45                                                       =yyas> (if (featurep 'rinari) (rinari-find-file-in-project) (yas-unimplemented 'rinari))
;; 
;; # as in Commands/Test Units.yasnippet
;; 2C60CBA1-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas-unknown)
;; 
;; # as in Snippets/returning do 7Cvariable7C E280A6 end.yasnippet
;; ${2/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1: |)/}                     =yyas> ${2:$(and (yas-text) " |")}
;; ${2/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}                      =yyas> ${2:$(and (yas-text) "|")}
;; 
;; # as in Snippets/form_for label.yasnippet
;; ${1/[[:alpha:]]+|(_)/(?1: :\u$0)/g}                                                        =yyas> ${1:$(capitalize (replace-regexp-in-string "_" " " yas-text))}
;;
;; # as in Snippets/has_one (ho).yasnippet
;; ${1/[[:alpha:]]+|(_)/(?1::\u$0)/g}                                                         =yyas> ${1:$(replace-regexp-in-string "_" "" (capitalize yas-text))}
;;
;; # as in Snippets/Create sweeper class.yasnippet
;; ${1/./\l$0/}                                                                               =yyas> ${1:$(and (yas-text) (concat (downcase (substring yas-text 0 1)) (substring yas-text 1)))} 
;;
;; # as in Snippets/image_submit_tag.yasnippet
;; ${1/^(\w+)(\.\w*)?$/$1/}                                                                   =yyas> ${1:$(file-name-sans-extension yas-text)}
;; 
;; # as in Snippets/respond_to (html).yasnippet
;; $TM_SELECTED_TEXT                                                                          =yyas> `yas-selected-text`
;;
;; # as in Snippets/find_in_batches.yasnippet
;; ${TM_CURRENT_WORD/(\w+)\./\L$1/g}                                                          =yyas> `(downcase (replace-regexp-in-string "\\..*$"  "" (current-word)))` 
;; 

;; Substitutions for: condition

;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.helper, meta.rails.unit_test, source.js, source.css, source.yaml, meta.rails.controller, meta.rails.functional_test, text.haml =yyas> t
;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.helper, meta.rails.functional_test, source.js, source.css, source.yaml, meta.rails.model, meta.rails.unit_test, text.haml      =yyas> t
;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.controller, meta.rails.model, meta.rails.unit_test, meta.rails.functional_test, text.haml                                      =yyas> t
;; meta.rails.controller, meta.rails.helper, meta.rails.model, meta.rails.unit_test, meta.rails.functional_test                                                                               =yyas> t
;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.controller, meta.rails.helper, text.haml                                                                                       =yyas> t
;; meta.rails.controller, meta.rails.helper, meta.rails.model, meta.rails.functional_test, source.yaml                                                                                        =yyas> t
;; meta.rails.controller, meta.rails.mailer, source.js, source.css                                                                                                                            =yyas> t
;; meta.rails.controller, meta.rails.helper, meta.rails.model, source.yaml, meta.rails.unit_test                                                                                              =yyas> t
;; meta.rails.migration - meta.rails.migration.create_table - meta.rails.migration.change_table                                                                                               =yyas> (yas-rails-intelligent-migration-snippet-condition-p)
;; meta.rails.migration.create_table, meta.rails.migration.change_table                                                                                                                       =yyas> (or (yas-rails-in-create-table-p) (yas-rails-in-change-table-p))
;; meta.rails.controller, meta.rails.mailer, source.js, source.css                                                                                                                            =yyas> (yas-unknown)
;; meta.rails.migration.create_table                                                                                                                                                          =yyas> (yas-rails-create-table-p)
;; meta.rails.functional_test                                                                                                                                                                 =yyas> (yas-rails-functional-test-p)
;; text.html.ruby, text.haml                                                                                                                                                                  =yyas> (yas-rails-view-p)
;; meta.rails.controller                                                                                                                                                                      =yyas> (yas-rails-controller-p)
;; meta.rails.routes                                                                                                                                                                          =yyas> (yas-rails-routes-p)
;; text.html.ruby                                                                                                                                                                             =yyas> (yas-unknown)
;;
;;
;; AC385ABF-96CD-4FCB-80AD-BF37D6EE79D2  =yyas> (and (member major-mode '(nxml-mode html-mode rhtml-mode)) (yas-rails-view-p))


;; Substitutions for: binding
;; 
;; # as in Snippets/rails session.yasnippet
;; ^j                                                                                         =yyas> C-c M-j
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; ~$                                                                                        =yyas> (yas-unknown)
;; 
;; # as in Commands/Go To View.yasnippet
;; ~$@                                                                                     =yyas> [M-S-s-down]
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; ^M                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Go To File on This Line.yasnippet
;; ~@                                                                                      =yyas> (yas-unknown)
;; 
;; # as in Commands/Show DB Schema.yasnippet
;; ^@S                                                                                        =yyas> C-c M-s
;; 
;; # as in Snippets/rails params.yasnippet
;; ^p                                                                                         =yyas> C-c M-p
;; 
;; # as in Commands/Go To File.yasnippet
;; 0CCC8443-40F3-4BAB-9440-D737562B5F45                                                       =yyas> [M-s-up]
;; # as in Commands/Go To Alternate File.yasnippet
;; 9453F0B3-B946-445F-BDB0-B01DE70732FC                                                       =yyas> [M-s-down]
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; ~                                                                                         =yyas> (yas-unknown)
;; 
;; # as in Commands/Test Units.yasnippet
;; ^\                                                                                         =yyas> C-c M-\
;; 
;; # as in Commands/Rake Migrate.yasnippet
;; ^|                                                                                         =yyas> C-c M-|
;; 
;; # as in Snippets/respond_to (html).yasnippet
;; @H                                                                                         =yyas> s-h
;; 
;; # as in Commands/Make Selection in to Partial.yasnippet
;; ^H                                                                                         =yyas> C-c M-m
;; 
;; # as in Commands/View demo help.yasnippet
;; ^h                                                                                         =yyas> C-c M-h
;; 
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

;; .yas-setup.el for rails-mode ends here
