
;; database

(defvar db '())

(defun get-word (key &optional (force nil))
  (if (numberp key)
      (list key '((valor)))
    (let ((word (assoc key db)))
      (when (and (null word) force)
	(setf word (list key '() nil))
	(push word db))
      word)))

(defun add-mean (key mean)
  (push mean (second (get-word key t))))

(defun implicit (key value)
  (setf (third (get-word key t)) value))

(defun prepositions (type keys)
  (mapcar #'(lambda (key) (add-mean key (list 'preposicao type))) keys))

(defun obj (&key name properties methods)
  (add-mean name (list 'objeto properties methods))
  (mapcar #'(lambda (key) (add-mean key (list 'of name))) properties)
  (mapcar #'(lambda (key) (add-mean (first key) '(verb))) methods))

;; iteraction

(defvar iter-list '())
(defvar iter-curs 0)

(defun iter-set (the-list)
  (setf iter-list the-list
	iter-curs 0))

(defun iter-next ()
  (unless (>= iter-curs (length iter-list))
    (let* ((curs iter-curs)
	   (next (1+ curs))
	   (labl (nth curs iter-list))
	   (item (get-word labl)))
      (setf iter-curs next)
      (or item (list labl '((undefined)) nil)))))

;; processing text

(defvar word nil)
(defvar action nil)
(defvar context '(nil nil nil))

(defun reset-conversation ()
  (setf word nil
	action nil
	context '(nil nil nil)))

(defun bank (text)
  (format t "~%~%INPUT: ~{~a~^ ~}~%~%" text)
  (iter-set text)
  (do ((curr (iter-next) (iter-next)))
      ((null curr))
      
      ))

;; feeding

(prepositions '1-ordem '(de da do))
(prepositions '2-ordem '(para pra))

(implicit 'transferir 'conta)
(implicit 'aplicar 'poupanca)
(implicit 'resgatar 'poupanca)

(obj :name 'conta
     :properties '(banco agencia conta)
     :methods '((transferir (objeto) (valor))))

(obj :name 'poupanca
     :properties '()
     :methods '((aplicar (valor))
		(resgatar (valor))))

(print db)

;; talking

(bank '(transferir 500 para agencia 123 conta 456 banco itau))
