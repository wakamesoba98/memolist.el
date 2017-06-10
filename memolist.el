;;; memolist.el --- memolist.el is Emacs port of memolist.vim.
;; Author: mikanfactory <k952i4j14x17_at_gmail.com>
;; Maintainer: mikanfactory
;; Copyright (C) 2015 mikanfactory all rights reserved.
;; Created: :2015-03-01
;; Version: 0.0.1
;; Keywords: markdown, memo
;; URL: http://github.com/mikanfactory/emacs-memolist
;; Package-Requires: ((markdown-mode "22.0") (ag "0.45"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;; memolist.el is Emacs port of memolist.vim. Org-mode is very useful and multifunction,
;; but it is also hard to approach and coufuse with markdown. memolist.el offer you
;; simple interface for writing and searching.
;; 
;; This program make markdown file in your `memolist-memo-directory' or
;; search markdown file there. By default, `memolist-memo-directory' is
;; set to "~/Memo" directory. If you would like to change it,
;; use custom-set-valiables function like this.
;; 
;; (custom-set-variables '(memolist-memo-directory "/path/to/your/memo/directory"))
;; 
;; Commands:
;; `memo-list': Show markdown file which placed in `memolist-memo-directory'.
;; `memo-grep': Search contents of markdown file by arg.
;; `memo-grep-tag': Search tags in markdown file by arg.
;; `memo-new': Create new markdown file in `memolist-memo-directory'.
;; 

;;; Code:
(require 'ag)
(require 'markdown-mode)
(require 'dash)

(defgroup memolist nil
  "memolist.el is Emacs port of memolist.vim."
  :prefix "memolist"
  :group 'convenience)

(defcustom memolist-memo-directory "~/Memo"
  "This package make markdown file in specified directory."
  :type 'string
  :group 'memolist)

(defun memolist-exist-directory? (directory)
  "Check `memolist-memo-directory' is already exist or not."
  (file-directory-p directory))

(defun memolist-create-directory (directory)
  "Make new directory specified by `memolist-memo-directory'."
  (make-directory directory))

(defun memolist-init-directory? (directory)
  (when (y-or-n-p (format "Create new directory in %s?" directory))
    (memolist-create-directory directory)
    t))

(defun memolist-overwrite-or-edit (title tags categories)
  "Ask whethre overwrite or edit file."
  (if (y-or-n-p "The file already exists. Do you want to edit the file? ")
      (memolist-edit-memo (memolist-make-file-name title categories))
    (memolist-overwrite-memo title tags categories)))

(defun memolist-create-new-memo (title tags categories)
  "Create new markdown file and insert header."
  (find-file (memolist-make-file-name title categories))
  (memolist-write-header title tags categories))

(defun memolist-overwrite-memo (title tags categories)
  "Overwrite markdown file."
  (find-file (memolist-make-file-name title categories))
  (erase-buffer)
  (memolist-write-header title tags categories))

(defun memolist-edit-memo (file)
  "Just open markdown file."
  (find-file file))

(defun memolist-write-header (title tags categories)
  "Insert headers."
  (progn
    (insert (format "title: %s\n" title))
    (insert "==========\n")
    (insert (format "date: %s" (memolist-format-current-time)))
    (insert (format "tags: [%s]\n" tags))
    (insert (format "categories: [%s]\n\n" categories))
    (insert "-----\n\n")))

(defun escape-title (title)
  "Escape some characters in title."
  (->> title
    (downcase)
    (replace-regexp-in-string "[ '\"]" "-")
    (replace-regexp-in-string "\(--\)\+" "-")
    (replace-regexp-in-string "\(^-\|-$\)" "")))

(defun memolist-make-title (title)
  "Format title."
  (format "%s-%s.md"
          (format-time-string "%Y-%m-%d" (current-time)) (escape-title title)))

(defun memolist-category-directory (categories)
  (concat memolist-memo-directory "/" (car (split-string categories " "))))

(defun memolist-make-file-name (title categories)
  "Create full path of markdown file."
  (expand-file-name (memolist-make-title title)
                    (memolist-category-directory categories)))

(defun memolist-format-current-time ()
  "Format current time."
  (format-time-string "%Y-%m-%d %H:%M\n" (current-time)))

;;;###autoload
(defun memo-new (title categories)
  "Create new markdown file in `memolist-memo-directory'.
If already same file was created, ask whether overwrite it or edit it.
And when same file does not exist, create new markdown file."
  (interactive "sMemo title: \nsMemo categories: ")
  (if (or (memolist-exist-directory? memolist-memo-directory)
          (memolist-init-directory? memolist-memo-directory))
      (let ((category-directory (memolist-category-directory categories))
            (tags ""))
        (if (or (memolist-exist-directory? category-directory)
                (memolist-init-directory? category-directory))
            (if (file-exists-p (memolist-make-file-name title categories))
                (memolist-overwrite-or-edit title tags categories)
              (memolist-create-new-memo title tags categories))))))

;;;###autoload
(defun memo-list ()
  "Show markdown file which placed in `memolist-memo-directory'."
  (interactive)
  (if (memolist-exist-directory? memolist-memo-directory)
      (find-file memolist-memo-directory)
    (message "Please create directory %s" memolist-memo-directory)))

;;;###autoload
(defun memo-grep (expr)
  "Search contents of markdown file by `expr'."
  (interactive "sag: ")
  (if (memolist-exist-directory? memolist-memo-directory)
      (ag-regexp expr memolist-memo-directory)
    (message "Please create directory %s" memolist-memo-directory)))

;;;###autoload
(defun memo-grep-tag (tag)
  "Search tags in markdown file by `tag'."
  (interactive "sInput tag: ")
  (if (memolist-exist-directory? memolist-memo-directory)
      (ag-regexp (format "tags:(.*)?%s(.*)?" tag) memolist-memo-directory)
    (message "Please create directory %s" memolist-memo-directory)))

(provide 'memolist)

;;; memolist.el ends here
