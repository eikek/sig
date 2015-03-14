#!/usr/bin/guile \
-e main -s
!#
;;; sig -- simple image gallery. Eike Kettner, 2015

;; sig is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; sig is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sig. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This script creates a directory containing a html file and a bunch
;; of javascript and css files. The html file shows a gallery of images
;; from a known directory.
;;
;; $ sig create mygallery
;; $ cd mygallery
;; $ sig make -i /folder/with/images

(use-modules
 (ice-9 format)
 (ice-9 futures)
 (ice-9 getopt-long)
 (ice-9 popen)
 (ice-9 rdelim)
 (ice-9 ftw))

;; configuration ------------------------------------------------------

;; javascript and css files
;;
;; These can be urls or path names. If a relative path name is
;; specified they are inserted verbatim into the html. If a absolute
;; path is given, it is symlinked into a local location. If this is a
;; recognized url it is downloaded using curl. "

(define *sig/resource-list*
  '((#:js . ("http://code.jquery.com/jquery-2.1.3.min.js"
             "https://blueimp.github.io/Gallery/js/jquery.blueimp-gallery.min.js"
             "https://raw.githubusercontent.com/blueimp/Bootstrap-Image-Gallery/master/js/bootstrap-image-gallery.min.js"))
    (#:css . ("http://netdna.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap.min.css"
              "https://raw.githubusercontent.com/blueimp/Gallery/master/css/blueimp-gallery.min.css"
              "https://raw.githubusercontent.com/blueimp/Bootstrap-Image-Gallery/master/css/bootstrap-image-gallery.min.css"))
    (#:img . ("https://raw.githubusercontent.com/blueimp/Gallery/master/img/error.png"
              "https://raw.githubusercontent.com/blueimp/Gallery/master/img/error.svg"
              "https://raw.githubusercontent.com/blueimp/Gallery/master/img/play-pause.png"
              "https://raw.githubusercontent.com/blueimp/Gallery/master/img/play-pause.svg"
              "https://raw.githubusercontent.com/blueimp/Gallery/master/img/loading.gif"
              "https://raw.githubusercontent.com/blueimp/Gallery/master/img/video-play.png"
              "https://raw.githubusercontent.com/blueimp/Gallery/master/img/video-play.svg"))))

(define *sig/supported-images*
  '("jpg" "png" "gif"))

(define *sig/supported-videos*
  '("mp4" "ogv" "webm" "mov" "avi"))

(define *sig/supported-files*
  (append *sig/supported-images* *sig/supported-videos*))

(define *sig/date-format* "%Y-%m-%d %H:%M")

(define *sig/curl* "curl")
(define *sig/convert* "convert")
(define *sig/jhead* "jhead")
(define *sig/ffmpeg* "ffmpeg")

(define (sig/resources key)
  (assq-ref *sig/resource-list* key))


;; ------------ utilities


(define (sig/absolute-path? name)
  "Return true if NAME is an absolute path"
  (absolute-file-name? name))

(define* (sig/path first . next)
  "Combines the arguments to a file system path name."
  (if (equal? first ".")
      (apply sig/path next)
      (if (or (not next) (null? next))
          first
          (apply sig/path
                 (string-append first
                                file-name-separator-string
                                (car next))
                 (cdr next)))))

(define (sig/mkdirs! . names)
  "Creates a nested directory from the given list."
  (if (and names (not (null? names)))
      (let ((head (car names))
            (tail (cdr names)))
        (when (and head
                   (not (file-exists? head))
                   (not (equal? "." head)))
          (mkdir head))
        (if (not (null? tail))
            (apply sig/mkdirs! (sig/path head (car tail)) (cdr tail))))))

(define (sig/url? name)
  "Return true if NAME seems to be a remote url."
  (let ((schemes '("http" "https" "ftp"))
        (idx (string-contains name "://")))
    (and idx (> idx 0)
         (member (string-take name idx) schemes)
         #t)))

(define (sig/dir? name)
  (file-exists? (sig/path name ".")))

(define (sig/empty-dir? dir)
  (let ((contents (scandir dir)))
    (or (not contents)
        (equal? 2 (length contents)))))

(define (sig/delete-rec! dir)
  "Deletes DIR recursively. Return the number of files and directories
removed."
  (define (enter? name . rest) #t)
  (define (leaf name stat result)
    (format #t "… delete file ~a ~%" name)
    (delete-file name)
    (+ result 1))
  (define (none name stat result) result)
  (define (up name stat result)
    (format #t "… delete directory ~a ~%" name)
    (rmdir name)
    (+ result 1))
  (define (error name stat errno result)
    (format (current-error-port)
            "Error: ~a: ~a~%" name (strerror errno))
    result)
  (format #t "Delete ~a recursively … ~%" dir)
  (let ((n (file-system-fold enter? leaf none up none error 0 dir)))
    (format #t "Removed ~d files." n)))

(define (sig/sig-directory? dir)
  (and (file-exists? (sig/path dir "resources"))
       (file-exists? (sig/path dir "thumbnails"))
       (file-exists? (sig/path dir "images"))))

(define (sig/string->keyword str)
  (define (space->dash s)
    (string-map (lambda (c)
                 (if (equal? c #\space) #\- c))
               s))
  (symbol->keyword
   (string-ci->symbol
    (string-downcase
     (space->dash (string-trim-both str))))))

(define (sig/select keys alist)
  "Filter ALIST and keep only elements whose key is in the list
KEYS."
  (filter (lambda (pair)
            (member (car pair) keys))
          alist))

(define (sig/as-future f)
  "Return a function that returns a future applying its arguments to F."
  (lambda (. args)
    (future (apply f args))))

(define (sig/file-extension filename)
  (if (or (equal? filename ".")
          (equal? filename "..")
          (sig/dir? filename)
          (not (string-contains filename ".")))
      #f
      (string-downcase
       (car (last-pair (string-split filename #\.))))))

(define (sig/basename-no-extension filename)
  (let ((base (basename filename))
        (ext  (sig/file-extension filename)))
    (string-drop-right base (1+ (string-length ext)))))

(define* (sig/supported-file? filename #:optional type)
  (let ((known (cond ((eq? type 'video) *sig/supported-videos*)
                     ((eq? type 'image) *sig/supported-images*)
                     (#t *sig/supported-files*))))
    (member (sig/file-extension filename) known)))

;; ---------- sig functions

(define* ((sig/make-resource-path resourcedir ext) resource)
  "Return the path where RESOURCE is stored."
  (let* ((name (basename resource)))
    (sig/path resourcedir ext name)))

(define* ((sig/get-resource! resourcedir ext) resource)
  "Download or symlink a RESOURCE into RESOURCEDIR."
  (let ((newpath ((sig/make-resource-path resourcedir ext) resource)))
    (sig/mkdirs! resourcedir ext)
    (cond ((sig/absolute-path? resource)
           (begin
             (format #t "Symlinking ~a → ~a~%" resource newpath)
             (symlink resource newpath)
             newpath))
          ((sig/url? resource)
           (let ((cmd (format #f "~a -o ~a ~a" *sig/curl* newpath resource)))
             (format #t "Downloading ~a to ~a~%" resource newpath)
             (if (equal? (system cmd) 0)
                 newpath
                 (begin
                   (format (current-error-port) "Failed to download ~a!~%" resource)
                   (throw 'download-failed)))))
          (#t resource))))

(define (sig/remove-gallery-files! directory)
  "Removes all gallery files."
  (when (sig/sig-directory? directory)
    (delete-file (sig/path directory "index.html"))
    (sig/delete-rec! (sig/path directory "resources"))
    (sig/delete-rec! (sig/path directory "thumbnails"))
    (sig/delete-rec! (sig/path directory "images"))
    #t))

(define (sig/create-gallery-dirs! directory)
  (for-each
   (lambda (name)
     (display (format #f "Creating directory ~a ... ~%"
                      (sig/path directory name)))
     (sig/mkdirs! directory name))
   '("resources" "images" "thumbnails"))
  #t)

(define (sig/exif-properties filename)
  "Return properties of FILENAME found by the exiv2 program."
  (define (split-line line)
    (let ((parts (string-split line #\:)))
      (cons (sig/string->keyword (car parts))
            (string-trim-both (string-join (cdr parts) ":")))))
  (define (parse data)
    (let* ((lines (string-split data #\newline))
           (props (map split-line
                       (filter (compose not string-null?) lines))))
      props))
  (define* ((change-keys keymap) pair)
    (let* ((key (car pair))
           (key2 (assq-ref keymap key)))
      (if key2 (cons key2 (cdr pair)) pair)))
  (let* ((keys '(#:camera-make #:camera-model #:date/time #:resolution))
         (keymap '((#:date/time . #:image-timestamp)))
         (cmd  (format #f "LC_ALL=C ~a -q ~a" *sig/jhead* filename))
         (data (if (sig/supported-file? filename 'image)
                   (let ((port (open-input-pipe cmd)))
                     (with-input-from-port port
                       (lambda () (parse (read-string port)))))
                   '())))
    (map (change-keys keymap)
         (filter (lambda (pair)
                   (or (not (string? (cdr pair)))
                       (not (string-null? (cdr pair)))))
                 (sig/select keys data)))))

(define* ((sig/image-properties directory) filename)
  "Return an alist of properties of the given file."
  (define (make-name ext)
    (string-append (sig/basename-no-extension filename) "." ext))
  (let* ((exif  (sig/exif-properties filename))
         (ext   (sig/file-extension filename))
         (media (cond ((member ext *sig/supported-images*) 'image)
                      ((member ext *sig/supported-videos*) 'video)
                      (#t 'unkown)))
         (video? (eq? media 'video))
         (basename  (basename filename))
         (info  (stat filename)))
    (append `((#:file-name . ,filename)
              (#:base-name . ,basename)
              (#:directory . ,directory)
              (#:image . ,(sig/path directory "images" (if video? (make-name "webm") basename)))
              (#:thumbnail . ,(sig/path directory "thumbnails" (if video? (make-name "jpg") basename)))
              (#:size . ,(stat:size info))
              (#:mtime . ,(stat:mtime info))
              (#:file-type . ,(stat:type info))
              (#:media . ,media))
            exif)))

(define* (sig/make-thumbnail! filename out #:optional (size 150))
  "Create a square thumbnail from the given image file. Save it to out
path."
  (let* ((resz (* size 2))
         (cmd (format #f "~a ~a -thumbnail x~d -resize '~dx<'\
                         -resize 50% -gravity center -crop ~dx~d+0+0 +repage ~a"
                      *sig/convert* filename resz resz size size out)))
    (when (not (equal? 0 (system cmd)))
      (throw 'thumbnail-failed))
    #t))

(define* (sig/resize-image! filename out #:optional (size 1200))
  "Resize the give image and save it to out."
  (let* ((resize (format #f "~a -resize ~d ~a ~a"
                         *sig/convert* size filename out))
         (autorot (format #f "~a -q -autorot ~a" *sig/jhead* out))
         (ext (sig/file-extension filename)))
    (when (not (equal? 0 (system resize)))
      (throw 'resize))
    (when (equal? ext "jpg")
      (when (not (equal? 0 (system autorot)))
        (throw 'autorot)))
    #t))

(define* (sig/convert-video! filename out #:optional (size "640x360"))
  (let ((ffmpeg (format #f "~a -loglevel quiet -y -i ~a -c:a libvorbis \
                           -ac 2 -ar 44100 -q:a 5 -q:v 5 -s ~a ~a"
                        *sig/ffmpeg*
                        filename
                        size
                        out))
        (ext (sig/file-extension filename)))
    (when (not (equal? ext "webm"))
      (system ffmpeg))))

(define* (sig/video-thumbnail! filename out #:optional (size 150))
  (let ((ffmpeg (format #f "~a -loglevel quiet -y -i ~a -an -ss 4 -s ~dx~d ~a"
                         *sig/ffmpeg*
                         filename
                         size size
                         out)))
    (system ffmpeg)))

(define* ((sig/do-file! size thumbsize) props)
  "Create a thumbnail and resized version of the given image."
  (let* ((filename (assq-ref props #:file-name))
         (basename (assq-ref props #:base-name))
         (media     (assq-ref props #:media))
         (directory (assq-ref props #:directory))
         (thumbpath (assq-ref props #:thumbnail))
         (imgpath (assq-ref props #:image)))
    (cond ((eq? media 'image)
           (begin
             (sig/resize-image! filename imgpath size)
             (sig/make-thumbnail! imgpath thumbpath thumbsize)))
          ((eq? media 'video)
           ;;ffmpeg it into imagedir
           (sig/convert-video! filename imgpath)
           ;;get a image from video and put it in thumbdir
           (sig/video-thumbnail! imgpath thumbpath thumbsize))
          (#t (format #t "~a: Don't know what to do with file.~%" filename)))
    (display ".")
    props))


(define (sig/create! directory)
  "Creates a new gallery outline. This creates several directories and
gets javascript and css resources."
  (format #t "Creating new gallery in ~a~%" directory)
  (sig/create-gallery-dirs! directory)
  (let* ((resourcedir (sig/path directory "resources"))
         (get-js (sig/get-resource! resourcedir "js"))
         (get-css (sig/get-resource! resourcedir "css"))
         (get-img (sig/get-resource! resourcedir "img")))
    (append (map get-js (sig/resources #:js))
            (map get-img (sig/resources #:img))
            (map get-css (sig/resources #:css)))))

(define* (sig/make-check! directory original #:optional (create-gallery? #t))
  "Check directory if a make would make sense."
  (when (sig/empty-dir? (sig/path directory original))
    (format (current-error-port) "ERROR: There is no folder '~a' containing images.~%" original)
    (throw 'no-images))
  (when (not (sig/sig-directory? directory))
    (if create-gallery?
        (begin
          (format (current-error-port)
                  "WARNING: The directory ~a does not seem to be a gallery directory." directory)
          (format (current-error-port)
                  "Creating one via `sig create'.~%")
          (sig/create! directory))
        (throw 'not-a-gallery-dir))))

(define (sig/sort-properties props)
  (define (props-less-by-time a b)
    (let ((itime-a (assq-ref a #:image-timestamp))
          (itime-b (assq-ref b #:image-timestamp))
          (mtime-a (assq-ref a #:mtime))
          (mtime-b (assq-ref b #:mtime)))
      (if (and itime-a itime-b)
          (string< itime-a itime-b)
          (< mtime-a mtime-b))))
  (sort-list props props-less-by-time))

(define* (sig/filesize-hr bytes #:optional (unit #:bytes) (factor 1024))
  (define (roundsize n)
    (/ (round (exact->inexact (* n 10))) 10))
  (let* ((nunit (if (eq? unit #:bytes) #:kb #:mb))
         (next (/ bytes factor)))
    (if (or (eq? unit #:mb) (< next 1))
        (cons (roundsize bytes)  unit)
        (sig/filesize-hr next nunit))))

(define* (sig/filesize-str bytes #:optional (unit #:bytes))
  (let ((size (sig/filesize-hr bytes unit)))
    (format #f "~s~a" (car size) (symbol->string (keyword->symbol (cdr size))))))

(define (sig/make-image-title props)
  "Create a string used as title for an image."
  (let ((name (assq-ref props #:base-name))
        (itime (assq-ref props #:image-timestamp))
        (mtime (assq-ref props #:mtime))
        (res   (assq-ref props #:resolution)))
    (format #f "~a, ~a, ~a (~a)"
            (or itime (strftime *sig/date-format* (localtime mtime)))
            (sig/filesize-str (assq-ref props #:size))
            (or res "n.a.")
            name)))

(define (sig/make-html-snippet props)
  (if (eq? 'video (assq-ref props #:media))
    (format #f "<a href=\"~a\" title=\"~a\" type=\"~a\" data-poster=\"~a\" data-gallery><img src=\"~a\"></a>"
            (assq-ref props #:image)
            (sig/make-image-title props)
            "video/webm"
            (assq-ref props #:thumbnail)
            (assq-ref props #:thumbnail))
    (format #f "<a href=\"~a\" title=\"~a\" data-gallery>\n<img src=\"~a\" alt=\"~a\">\n</a>"
            (assq-ref props #:image)
            (sig/make-image-title props)
            (assq-ref props #:thumbnail)
            (sig/make-image-title props))))

(define* (sig/make-html resourcedir props #:optional (title "Gallery"))
  (define (make-js p) (format #f "<script src=\"~a\"></script>" p))
  (define (make-css p) (format #f "<link rel=\"stylesheet\" href=\"~a\"/>" p))
  (let* ((out (sig/path resourcedir ".." "index.html"))
         (js  (map (compose make-js
                            (sig/make-resource-path resourcedir "js"))
                   (sig/resources #:js)))
         (css (map (compose make-css
                            (sig/make-resource-path resourcedir "css"))
                   (sig/resources #:css)))
         (links (map sig/make-html-snippet (sig/sort-properties props))))
    (with-output-to-string
      (lambda ()
        (display "<html>\n  <head>\n")
        (format #t "    <title>~a</title>~%" title)
        (display "    <meta charset=\"utf-8\">\n")
        (display "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />")
        (newline)
        (display (string-join css "\n"))
        (newline)
        (display "</head>\n  <body>\n")
        (display "    <!-- The Bootstrap Image Gallery lightbox, should be a child element of the document body -->
    <div id=\"blueimp-gallery\" class=\"blueimp-gallery\" data-use-bootstrap-modal=\"false\">
      <!-- The container for the modal slides -->
      <div class=\"slides\"></div>
      <!-- Controls for the borderless lightbox -->
      <h3 class=\"title\"></h3>
      <a class=\"prev\">&#8249;</a>
      <a class=\"next\">&#8250;</a>
      <a class=\"close\">&#215;</a>
      <a class=\"play-pause\"></a>
      <ol class=\"indicator\"></ol>
      <!-- The modal dialog, which will be used to wrap the lightbox content -->
      <div class=\"modal fade\">
        <div class=\"modal-dialog\">
          <div class=\"modal-content\">
            <div class=\"modal-header\">
              <button type=\"button\" class=\"close\" aria-hidden=\"true\">&times;</button>
              <h4 class=\"modal-title\"></h4>
            </div>
            <div class=\"modal-body next\"></div>
              <div class=\"modal-footer\">
                <button type=\"button\" class=\"btn btn-default pull-left prev\">
                  <i class=\"glyphicon glyphicon-chevron-left\"></i>
                  Previous
                </button>
                <button type=\"button\" class=\"btn btn-primary next\">
                  Next
                  <i class=\"glyphicon glyphicon-chevron-right\"></i>
                </button>
              </div>
            </div>
          </div>
      </div>
    </div>
    <div class=\"container\">\n")
        (format #t "  <h1>~a</h1>~%" title)
        (display "  <div id=\"links\">\n")
        (display (string-join links "\n"))
        (newline)
        (display "  </div>\n</div>\n")
        (display (string-join js "\n"))
        (newline)
        (display "  </body>\n</html>")))))

(define (sig/write-file! name contents)
  (format #t "~%Writing file ~a~%" name)
  (when (file-exists? name)
    (delete-file name))
  (with-output-to-file name
    (lambda() (display contents) #t)))

(define (sig/scandir original)
  (map (lambda (fname) (sig/path original fname))
       (let ((files (scandir original sig/supported-file?)))
         (or files '()))))

(define* (sig/make-images! directory #:optional original (thumbsize 150) (imgsize 1200))
  "Make the gallery by resizing images from directory ORIGINAL and
creating thumbnails. Return a list of image properties."
  (sig/make-check! directory original)
  (format #t "Creating gallery from images in ~a~%" original)
  (let* ((orgdir (or original (sig/path directory "original")))
         (dowork (compose (sig/do-file! imgsize thumbsize)
                          (sig/image-properties directory)))
         (work (map (sig/as-future dowork) (sig/scandir orgdir))))
    (map touch work)))


;; --- commands

(define (main-create args)
  (let ((len (length args)))
    (sig/create! (if (> len 1) (cadr args) "."))))

(define (main-make args)
  (define option-spec
    '((thumbsize (single-char #\t) (value #t))
      (imgsize   (single-char #\s) (value #t))
      (name      (single-char #\n) (value #t))
      (in        (single-char #\i) (value #t))))
  (let* ((opts  (getopt-long args option-spec))
         (dir   (option-ref opts 'in "original"))
         (size  (or (string->number (option-ref opts 'imgsize "1200"))
                    1200))
         (thsz  (or (string->number (option-ref opts 'thumbsize "150"))
                    150))
         (gallery (option-ref opts 'name "Gallery"))
         (props (sig/make-images! "." dir thsz size)))
    (sig/write-file! "index.html"
                    (sig/make-html "resources" props gallery))
    (display "Done.\n")))

(define (main-html args)
  (define option-spec
    '((in        (single-char #\i) (value #t))
      (name      (single-char #\n) (value #t))))
  (let* ((opts  (getopt-long args option-spec))
         (dir   (option-ref opts 'in "original"))
         (gallery (option-ref opts 'name "Gallery"))
         (props (map (sig/image-properties ".") (sig/scandir dir))))
    (sig/write-file! "index.html"
                     (sig/make-html (sig/path "resources") props gallery))))

(define (main-help args)
  (display "Not implemented, sorry.\n"))

(define (main args)
  (let* ((len (length (cdr args)))
         (action (if (> len 0) (cadr args) "")))
    (cond ((equal? 0 len)
           (format (current-error-port) "No arguments given. Try `help'.~%"))
          ((equal? action "create")
           (main-create (cdr args)))
          ((equal? action "make")
           (main-make (cdr args)))
          ((equal? action "html")
           (main-html (cdr args)))
          ((equal? action "help")
           (main-help (cdr args)))
          (#t (format (current-error-port)
                      "Unknown action: ~a. Try `help'.~%" action)))))
