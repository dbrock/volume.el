;;; volume.el --- tweak your sound card volume from Emacs
;; Copyright (C) 2005, 2006  Daniel Brockman
;; Copyright (C) 1998, 2000, 2001, 2002, 2003, 2004, 2005
;;   Free Software Foundation, Inc.

;; Version: 0.7
;; Author: Daniel Brockman <daniel@brockman.se>
;; URL: http://www.brockman.se/software/volume-el/
;; Created: September 9, 2005
;; Updated: September 22, 2006

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Installation:

;; To use this program, put this file in your `load-path',
;; and put the following autoload in your ~/.emacs:

;;   (autoload 'volume "volume"
;;     "Tweak your sound card volume." t)

;; Then type M-x volume RET to run the program.  Of course,
;; use M-x customize-group RET volume RET to customize it.

;;; Commentary:

;; Tweaking the volume of my music used to be one of the
;; few things I constantly went outside of Emacs to do.
;; I just decided I've had enough of that, and so I wrote
;; this simple mixer frontend.

;; It comes with backend glue for aumix and amixer, but the
;; latter is pretty slow, so I have to recommend the former.
;; If you can't use either, writing your own glue should be
;; straightforward.  And if you do, please consider sending
;; the code to me, so I can integrate it into this file.

;;; Code:

(defgroup volume nil
  "Tweak your sound card volume."
  :group 'multimedia)

(defcustom volume-backend
  (cond ((executable-find "aumix") 'volume-aumix-backend)
        ((executable-find "amixer") 'volume-amixer-backend))
  "The set of primitives used by Volume to do real work.
Value is an alist containing entries `get', `set', `nudge',
`current-channel', `switch-channel', `default-channel',
`channel-name', and `channels', or the name of a variable
containing such an alist."
  :type '(radio (const :tag "aumix" volume-aumix-backend)
                (const :tag "amixer" volume-amixer-backend)
                (const :tag "None" nil)
                (variable :tag "Custom"))
  :group 'volume)

(defcustom volume-electric-mode t
  "Run Volume electrically, in the echo area.
Electric mode saves some space, but uses its own command loop."
  :type 'boolean
  :group 'volume)

(defface volume-bar
  '((t (:inverse-video t :weight bold)))
  "Face used for the indicator bar in Volume mode."
  :group 'volume)

(defun volume-backend-call (primitive &rest arguments)
  "Call PRIMITIVE from the current backend with ARGUMENTS.
See the variable `volume-backend'."
  (let ((backend (symbol-value (indirect-variable volume-backend))))
    (when (null backend)
      (volume-error "No backend (see `volume-backend')"))
    (let ((function (cdr (assoc primitive backend))))
      (when (null function)
        (volume-error "No `%s' operation for current backend" primitive))
      (apply function arguments))))

(defvar volume-buffer nil
  "The current Volume buffer, or nil.")

(defun volume-call-process (program &rest arguments)
  "Like `shell-command-to-string', but doesn't have to use the shell."
  (with-output-to-string
    (with-current-buffer standard-output
      (unless (eq 0 (apply 'call-process program nil t nil arguments))
        (volume-error
         "Process `%s' exited abnormally"
         (mapconcat 'identity (cons program arguments) " "))))))


;;;; The aumix backend

(defvar volume-aumix-backend
  '((get . volume-aumix-get)
    (set . volume-aumix-set)
    (nudge . volume-aumix-nudge)
    (current-channel . volume-aumix-current-channel)
    (switch-channel . volume-aumix-switch-channel)
    (default-channel . volume-aumix-default-channel)
    (channel-name . volume-aumix-channel-name)
    (channels . volume-aumix-channels)))

(defgroup volume-aumix nil
  "The aumix backend."
  :group 'volume)

(defcustom volume-aumix-program "aumix"
  "The name of the aumix program."
  :type 'string
  :group 'volume-aumix)

(defcustom volume-aumix-device nil
  "The name of the mixer device, or nil for the default.
This corresponds to the `-d' option of aumix."
  :type '(choice (const :tag "/dev/mixer" nil)
                 (const "/dev/mixer1")
                 (const "/dev/mixer2")
                 (const "/dev/mixer3")
                 (const "/dev/mixer4")
                 file)
  :group 'volume-aumix)

(defvar volume-aumix-all-channels
  '(("-v" . "Master")
    ("-b" . "Bass")
    ("-t" . "Treble")
    ("-w" . "PCM")
    ("-W" . "PCM 2")
    ("-l" . "Line")
    ("-1" . "Line 1")
    ("-2" . "Line 2")
    ("-3" . "Line 3")
    ("-s" . "Synthesizer")
    ("-p" . "PC speaker")
    ("-c" . "CD")
    ("-x" . "Mix monitor")
    ("-m" . "Microphone")
    ("-r" . "Record")
    ("-i" . "Input gain")
    ("-o" . "Output gain"))
  "Alist mapping aumix options to channel names.")

(defvar volume-aumix-default-channels
  (mapcar (lambda (channel)
            (rassoc channel volume-aumix-all-channels))
          '("Master" "Bass" "Treble" "PCM" "Line"))
  "The default value of `volume-aumix-channels'.")

(defcustom volume-aumix-channels volume-aumix-default-channels
  "Alist mapping aumix options to channel names."
  :type `(set ,@(mapcar (lambda (entry)
                             `(const :tag ,(cdr entry) ,entry))
                           volume-aumix-all-channels)
              (repeat :tag "Others" :inline t
                      (cons :tag "Channel"
                            (string :tag "Option (see `aumix -h')")
                            (string :tag "Name"))))
  :group 'volume-aumix)

(defcustom volume-aumix-default-channel '("-v" . "Master")
  "The aumix option for the default audio channel to manipulate."
  :type `(choice ,@(mapcar (lambda (entry)
                             `(const :tag ,(cdr entry) ,entry))
                           volume-aumix-all-channels)
                 (string :tag "Other (specify aumix option)"))
  :group 'volume-aumix)

(defvar volume-aumix-current-channel volume-aumix-default-channel
  "The aumix option for the audio channel to manipulate.")

(defun volume-aumix-current-channel-option ()
  "Return the aumix option for the current channel."
  (car volume-aumix-current-channel))

(defcustom volume-aumix-extra-arguments nil
  "Extra arguments to pass to the aumix program."
  :type '(repeat string)
  :group 'volume-aumix)

(defun volume-aumix-call (&rest arguments)
  "Call aumix with ARGUMENTS and return the output."
  (apply 'volume-call-process volume-aumix-program
         (append (when volume-aumix-device
                   (list "-d" volume-aumix-device))
                 volume-aumix-extra-arguments
                 arguments)))

(defun volume-aumix-parse-output (string)
  "Parse the output of an aumix volume query.
Return the volume percentage as a floating-point number.
If STRING cannot be parsed, raise an error."
  (if (string-match "^\\S-+ \\([0-9]+\\)" string)
      (float (string-to-number (match-string 1 string)))
    (volume-error "Failed to parse aumix output")))

(defun volume-aumix-get ()
  "Return the current volume in percent, using aumix to get it."
  (volume-aumix-parse-output
   (volume-aumix-call
    (concat (volume-aumix-current-channel-option) "q"))))

(defun volume-aumix-set (n)
  "Use aumix to set the current volume to N percent.
Return the new volume in percent."
  (volume-aumix-parse-output
   (volume-aumix-call
    (concat (volume-aumix-current-channel-option)
            (number-to-string n))
    (concat (volume-aumix-current-channel-option) "q"))))

(defun volume-aumix-nudge (n)
  "Use aumix to change the volume by N percentage units.
Return the new volume in percent."
  (let ((sign (if (>= n 0) "+" "-")))
    (volume-aumix-parse-output
     (volume-aumix-call
      (concat (volume-aumix-current-channel-option)
              sign (number-to-string (abs n)) )
      (concat (volume-aumix-current-channel-option) "q")))))

(defun volume-aumix-current-channel ()
  "Return the current channel for aumix."
  volume-aumix-current-channel)

(defun volume-aumix-switch-channel (channel)
  "Make CHANNEL current for aumix."
  (setq volume-aumix-current-channel channel))

(defun volume-aumix-default-channel ()
  "Return the default channel for aumix."
  volume-aumix-default-channel)

(defun volume-aumix-channel-name (channel)
  "Return the name of CHANNEL."
  (cdr channel))

(defun volume-aumix-channels ()
  "Return the list of available channels for aumix."
  volume-aumix-channels)


;;;; The amixer backend

(defvar volume-amixer-backend
  '((get . volume-amixer-get)
    (set . volume-amixer-set)
    (nudge . volume-amixer-nudge)
    (current-channel . volume-amixer-current-channel)
    (switch-channel . volume-amixer-switch-channel)
    (default-channel . volume-amixer-default-channel)
    (channel-name . volume-amixer-channel-name)
    (channels . volume-amixer-channels)))

(defgroup volume-amixer nil
  "The amixer backend."
  :group 'volume)

(defcustom volume-amixer-program "amixer"
  "The name of the amixer program."
  :type 'string
  :group 'volume-amixer)

(defcustom volume-amixer-card nil
  "The ALSA sound card number to use, or nil for the default.
This corresponds to the `-c' option of amixer."
  :type '(choice integer (const :tag "Default" nil))
  :group 'volume-amixer)

(defcustom volume-amixer-device nil
  "The ALSA device name to use, or nil for the default.
This corresponds to the `-D' option of amixer."
  :type '(choice string (const :tag "Default" nil))
  :group 'volume-amixer)

(defun volume-amixer-channel-has-volume-p (channel)
  "Return non-nil if CHANNEL uses the concept of a volume."
  (condition-case nil
      (string-match "^\\s-*Capabilities: \\<[a-z]*volume\\>"
                    (volume-amixer-call "get" channel))
    (error nil)))

(defvar volume-amixer-default-channels
  '("Master" "Bass" "Treble" "PCM" "Line")
  "The default value of `volume-amixer-channels'.")

(defcustom volume-amixer-channels
  (if (executable-find volume-amixer-program)
      (apply 'append
             (mapcar (lambda (channel)
                       (when (volume-amixer-channel-has-volume-p channel)
                         (list channel)))
                     volume-amixer-default-channels))
    volume-amixer-default-channels)
  "The names of the ALSA mixer channels to manipulate."
  :type `(set ,@(if (executable-find volume-amixer-program)
                    (let (channels)
                      (with-temp-buffer
                        (call-process volume-amixer-program nil t)
                        (goto-char (point-min))
                        (while (search-forward-regexp
                                "^\\s-*Capabilities: \\<[a-z]*volume\\>"
                                nil t)
                          (save-excursion
                            (search-backward-regexp
                             "^\\s-*Simple mixer control '\\(.*\\)'")
                            (setq channels (cons (match-string 1)
                                                 channels)))))
                      (mapcar (lambda (channel)
                                `(const ,channel))
                              channels))
                  volume-amixer-default-channels)
          (repeat :tag "Others" :inline t
                  (string :tag "Channel")))
  :group 'volume-amixer)

(defcustom volume-amixer-default-channel
  (if (executable-find volume-amixer-program)
      (cond
       ((volume-amixer-channel-has-volume-p "Master") "Master")
       ((volume-amixer-channel-has-volume-p "PCM") "PCM")
       (t (or (car-safe volume-amixer-channels) "Master")))
    "Master")
  "The name of the default ALSA mixer channel to manipulate."
  :type `(radio ,@(mapcar (lambda (channel)
                            `(const ,channel))
                          volume-amixer-channels)
                (string :tag "Other"))
  :group 'volume-amixer)

(define-obsolete-variable-alias 'volume-amixer-control
  'volume-amixer-default-channel)

(defvar volume-amixer-current-channel volume-amixer-default-channel
  "The name of the ALSA mixer channel to manipulate.")

(defcustom volume-amixer-extra-arguments nil
  "Extra arguments to pass to the amixer program."
  :type '(repeat string)
  :group 'volume-amixer)

(defun volume-amixer-call (&rest arguments)
  "Call amixer with ARGUMENTS and return the output."
  (apply 'volume-call-process volume-amixer-program
         (append (when volume-amixer-card
                   (list "-c" (number-to-string volume-amixer-card)))
                 (when volume-amixer-device
                   (list "-D" volume-amixer-device))
                 volume-amixer-extra-arguments
                 arguments)))

(defun volume-amixer-parse-output (output)
  "Parse the OUTPUT of an amixer control dump.
Return the volume percentage as a floating-point number.
If OUTPUT cannot be parsed, raise an error."
  (if (string-match "\\[\\([0-9]+\\)%\\]" output)
      (float (string-to-number (match-string 1 output)))
    (volume-error "Failed to parse amixer output")))

(defun volume-amixer-get ()
  "Return the current volume, using amixer to get it."
  (volume-amixer-parse-output
   (volume-amixer-call "get" volume-amixer-current-channel)))

(defun volume-amixer-set (n)
  "Use amixer to set the current volume to N percent."
  (volume-amixer-parse-output
   (volume-amixer-call "set" volume-amixer-current-channel
                       (format "%d%%" n))))

(defun volume-amixer-nudge (n)
  "Use amixer to change the volume by N percentage units."
  (let ((sign (if (>= n 0) "+" "-"))
        (current (volume-amixer-get)))
    (when (= current
             (volume-amixer-parse-output
              (volume-amixer-call "set" volume-amixer-current-channel
                                  (format "%d%%%s" (abs n) sign))))
      ;; If nudging by `N%' didn't work, try `N'.
      (volume-amixer-parse-output
       (volume-amixer-call "set" volume-amixer-current-channel
                           (format "%d%s" (abs n) sign))))))

(defun volume-amixer-current-channel ()
  "Return the current channel for amixer."
  volume-amixer-current-channel)

(defun volume-amixer-switch-channel (channel)
  "Make CHANNEL current for amixer."
  (setq volume-amixer-current-channel channel))

(defun volume-amixer-default-channel ()
  "Return the default channel for amixer."
  volume-amixer-default-channel)

(defun volume-amixer-channel-name (channel)
  "Return the name of CHANNEL."
  channel)

(defun volume-amixer-channels ()
  "Return the list of available channels for amixer."
  volume-amixer-channels)


;;;; User interface

(defun volume-get ()
  "Return the current volume in percent."
  (volume-backend-call 'get))

(defun volume-set (n)
  "Set the volume to N percent."
  (volume-backend-call 'set n))

(defun volume-nudge (n)
  "Change the volume by N percentage units.
Return either the new volume or nil, depending on the backend."
  (volume-backend-call 'nudge n))

(defun volume-current-channel ()
  "Return the current channel."
  (volume-backend-call 'current-channel))

(defun volume-switch-channel (channel)
  "Make CHANNEL current."
  (volume-backend-call 'switch-channel channel))

(defun volume-default-channel ()
  "Retur the default channel."
  (volume-backend-call 'default-channel))

(defun volume-channel-name (channel)
  "Return the name of CHANNEL."
  (volume-backend-call 'channel-name channel))

(defun volume-channels ()
  "Return the list of available channels."
  (volume-backend-call 'channels))

(defun volume-next-channel ()
  "Switch to the next channel."
  (interactive)
  (let* ((channels (volume-channels))
         (channel (or (car-safe
                       (cdr-safe
                        (member (volume-current-channel) channels)))
                      (car-safe channels))))
    (if channel
        (volume-switch-channel channel)
      (volume-error "Channel list is empty"))))

(defun volume-previous-channel ()
  "Switch to the previous channel."
  (interactive)
  (let* ((reverse-channels (reverse (volume-channels)))
         (channel (or (car-safe
                       (cdr-safe
                        (member (volume-current-channel) reverse-channels)))
                      (car-safe reverse-channels))))
    (if channel
        (volume-switch-channel channel)
      (volume-error "Channel list is empty"))))

(defun volume-show (&optional volume)
  "Display the current volume in the minibuffer.
If VOLUME is non-nil, take that to be the current volume."
  (interactive)
  (message "Volume%s: %d%%"
           (if (equal (volume-current-channel)
                      (volume-default-channel)) ""
             (concat " (" (volume-channel-name
                           (volume-current-channel)) ")"))
           (or volume (round (volume-get)))))

(defun volume-redisplay (&optional volume)
  "Update the Volume buffer to reflect the current volume.
If VOLUME is non-nil, take that to be the current volume."
  (interactive)
  (when (null volume)
    (setq volume (volume-get)))
  (let ((inhibit-read-only t))
    (set-buffer volume-buffer)
    (delete-region (point-min) (point-max))
    (insert "Volume")
    (unless (equal (volume-current-channel)
                   (volume-default-channel))
      (insert " (" (volume-channel-name
                    (volume-current-channel)) ")"))
    (insert ": ")
    (let* ((bar-start (point))
           (available-width (- (window-width) bar-start))
           (bar-width (round (* (/ volume 100.0) available-width)))
           (label (format " %d%% " volume))
           (label-width (length label)))
      (insert-char ?\  available-width)
      (goto-char
       (+ bar-start
          (if (< bar-width label-width) (1+ bar-width)
            (/ (1+ (- bar-width label-width)) 2))))
      (delete-char label-width) (insert label)
      (put-text-property bar-start (+ bar-start bar-width)
                         'face 'volume-bar)
      (goto-char (+ bar-start bar-width)))))

(defun volume-update (&optional volume)
  "Maybe call `volume-show' or `volume-redisplay'; return VOLUME.
This function should be called by UI commands that change the volume."
  (prog1 volume
    (if volume-buffer
        ;; The electric command loop will trigger a redisplay
        ;; after each command anyway, so avoid doing it twice.
        (unless volume-electric-mode
          (volume-redisplay volume))
      (volume-show volume))))

(defun volume-error (format &rest strings)
  "Either signal a real error, or manually beep and display message.
Real errors cannot be used in electric mode."
  (if (or (not volume-electric-mode)
          (null volume-buffer))
      (apply 'error format strings)
    (beep)
    (with-current-buffer volume-buffer
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert (apply 'format format strings))
        (sit-for 2)))
    (volume-redisplay)))

(defun volume-assign (n)
  "Set the volume to N percent.
If N is negative, call `volume-raise' instead."
  (interactive "P")
  (if (integerp n)
      (if (< n 0) (volume-raise n)
        (volume-update (volume-set n)))
    (volume-error "Need integer argument")))

(defun volume-lower (&optional n)
  "Lower the volume by N percentage units."
  (interactive "p")
  (volume-update (volume-nudge (- (or n 1)))))

(defun volume-raise (&optional n)
  "Raise the volume by N percentage units."
  (interactive "p")
  (volume-update (volume-nudge (or n 1))))

(defun volume-minimize ()
  "Lower the volume as much as possible."
  (interactive)
  (volume-set 0))

(defun volume-maximize ()
  "Raise the volume as much as possible."
  (interactive)
  (volume-set 100))

(defun volume-assign-and-quit (&optional n)
  "Set the volume to N percent and then quit Volume mode.
If N is nil, just quit Volume mode."
  (interactive "P")
  (when (integerp n)
    (volume-redisplay (volume-assign n))
    (sit-for 1))
  (volume-quit))

(defun volume-quit ()
  "Quit Volume mode."
  (interactive)
  (if volume-electric-mode
      (throw 'volume-done nil)
    (condition-case nil
        (while (get-buffer-window volume-buffer)
          (delete-window (get-buffer-window volume-buffer)))
      (error nil))
    (kill-buffer volume-buffer)
    (setq volume-buffer nil)))

(defun volume-mode ()
  "Major mode for tweaking your audio volume.

\\{volume-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'volume-mode)
  (setq mode-name "Volume")
  (use-local-map volume-mode-map)
  (volume-update)
  (run-mode-hooks 'volume-mode-hook))

(defvar volume-mode-map
  (let ((map (make-sparse-keymap))
        (lower-more (lambda (n)
                      (interactive "p")
                      (volume-lower (* n 10))))
        (raise-more (lambda (n)
                      (interactive "p")
                      (volume-raise (* n 10)))))
    (suppress-keymap map)
    (define-key map "b" 'volume-lower)
    (define-key map "f" 'volume-raise)
    (define-key map "\C-b" 'volume-lower)
    (define-key map "\C-f" 'volume-raise)
    (define-key map "\M-b" lower-more)
    (define-key map "\M-f" raise-more)
    (define-key map [left] 'volume-lower)
    (define-key map [right] 'volume-raise)
    (define-key map [(control left)] lower-more)
    (define-key map [(control right)] raise-more)
    (define-key map [(meta left)] lower-more)
    (define-key map [(meta right)] raise-more)
    (define-key map "s" 'volume-assign)
    (define-key map "a" 'volume-minimize)
    (define-key map "e" 'volume-maximize)
    (define-key map "\C-a" 'volume-minimize)
    (define-key map "\C-e" 'volume-maximize)
    (define-key map [home] 'volume-minimize)
    (define-key map [end] 'volume-maximize)
    (define-key map "n" 'volume-next-channel)
    (define-key map "p" 'volume-previous-channel)
    (define-key map "\C-n" 'volume-next-channel)
    (define-key map "\C-p" 'volume-previous-channel)
    (define-key map "\M-n" 'volume-next-channel)
    (define-key map "\M-p" 'volume-previous-channel)
    (define-key map [up] 'volume-next-channel)
    (define-key map [down] 'volume-previous-channel)
    (define-key map "g" 'volume-redisplay)
    (define-key map "\C-m" 'volume-assign-and-quit)
    (define-key map "q" 'volume-quit)
    (define-key map [escape escape] 'volume-quit)
    map)
  "Keymap for Volume mode.")

;; This function was based on the function `calculator' from
;; calculator.el, which is copyrighted by the FSF.
;;;###autoload
(defun volume ()
  "Tweak your sound card volume."
  (interactive)
  (setq volume-buffer (get-buffer-create "*volume*"))
  (if volume-electric-mode
      (unwind-protect
          (save-window-excursion
            (require 'electric) (message nil)
            (let ((echo-keystrokes 0)
                  (garbage-collection-messages nil))
              (set-window-buffer (minibuffer-window) volume-buffer)
              (select-window (minibuffer-window))
              (let ((old-local-map (current-local-map))
                    (old-global-map (current-global-map)))
                (use-local-map nil)
                (use-global-map volume-mode-map)
                (unwind-protect
                    (progn
                      (volume-redisplay)
                      (run-hooks 'volume-mode-hook)
                      (catch 'volume-done
                        (Electric-command-loop
                         'volume-done
                         ;; Avoid `noprompt' due to
                         ;; a bug in electric.el.
                         '(lambda () 'noprompt)
                         nil
                         (lambda (x y) (volume-redisplay)))))
                  (use-local-map old-local-map)
                  (use-global-map old-global-map)))))
        (when volume-buffer
          (kill-buffer volume-buffer)
          (setq volume-buffer nil)))
    (cond
     ((null (get-buffer-window volume-buffer))
      (let ((window-min-height 2)
            (split-window-keep-point nil))
        (select-window
         (split-window-vertically
          (if (and (fboundp 'face-attr-construct)
                   (plist-get (face-attr-construct 'modeline) :box))
              -3 -2)))
        (switch-to-buffer volume-buffer)))
     ((not (eq (current-buffer) volume-buffer))
      (select-window (get-buffer-window volume-buffer))))
    (volume-mode)
    (setq buffer-read-only t)))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-format: "%:b %:d, %:y"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; End:

(provide 'volume)
;;; volume.el ends here.
