(in-package :emacs-update)

(defparameter *package-path* "~/.config/emacs-configs/default/doom/packages.el")

(defun trim-all (str)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab
                 #\Linefeed #\Page #\Return #\Rubout)
               str))

(defun get-password (account)
  (cond ((equalp account :github) (cl-ppcre:split
                                   "\\s+" (trim-all (uiop:read-file-string
                                                     "~/Sync/Apps/Github/githubreadonly_api_key.txt"))))
        ((equalp account :gitlab) '("foo bar"))
        (t     "Invalid account")))

(defun substringp (needle haystack &key (test 'char=))
  "Returns the index of the first occurrence of the string designated
by NEEDLE within the string designated by HAYSTACK, or NIL if it does
not occur.  Characters within the string are compared by TEST, which
defaults to CHAR= (for case-sensitive comparison)."
  (search (string needle)
          (string haystack)
          :test test))

(defun get-package-list (package-path)
  (uiop:read-file-forms package-path))

(defun github-commits (user/repo)
  (let* ((credentials  (get-password :github))
         (stream (drakma:http-request
                  (format nil "https://api.github.com/repos/~A/commits" user/repo)
                  :basic-authorization credentials
                  :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (setf yason:*parse-object-key-fn* (lambda (key) (intern (string-upcase key) "KEYWORD")))
    (let ((last-commit (first (yason:parse stream :object-as :plist))))
      (list :pin (getf last-commit :sha)
            :commit-date (getf (getf (getf last-commit :commit) :author) :date)))))

(defun without-recipes (packages)
  "This returns a list of packages that do not have any recipes."
  (remove-if (lambda (package)
               (or (getf package :recipe)
                   (getf package :disable)
                   (getf package :ignore))) packages))

(defun with-recipe (host packages)
  "Returns a list of package recipes that do have a recipe."
  (mapcar
   (lambda (recipe) (list :pin (getf recipe :pin)
                          :repo (getf (getf recipe :recipe) :repo)))
   (remove-if-not
    (lambda (package) (equalp (getf (getf package :recipe) :host) host))
    packages)))

(defun check-github-package (package)
  (let ((github-commit (github-commits (getf package :repo))))
    (when (not (substringp (getf package :pin) (getf github-commit :pin)))
      github-commit)))

(defun -main (&rest args)
  (let* ((packages (get-package-list *package-path*))
         (github-packages (with-recipe 'github packages))
         (gitlab-packages (with-recipe 'gitlab packages))
         (other-packages (without-recipes packages)))
    (mapcar
     (lambda (package)
       (check-github-package package))
     github-packages)))
