Векторизатор на лиспе (sbcl)

Обработка изображения производится с помощью пакета imagemagick.

В файле main.lisp содержится основная логика разбиения изображения на фрагменты,
векторизация и пр.



Инструкции для запуска

(compile-file "main.lisp")
(load "main.fasl")
(time (thin-image-file "./out/30.pgm" "./out/result.pgm"))

(time (thin-image-file "./out/test-line.pgm" "./out/result.pgm"))
(thin-image-file "./out/small.pgm" "./out/result.pgm")


Новые инфструкции
(require 'asdf)
(asdf:oos 'asdf:load-op 'cl-vectorizer)
(cl-vectorizer:set-working-dir-in #p"/home/selead/vector-test/in/")
(cl-vectorizer:set-working-dir-out #p"/home/selead/vector-test/out/") 
;; "/" в конце обязательно	 
(cl-vectorizer:thin-image-file #p"01.tif")


(let (path "/home/selead/vector-test/in/01.tif")
 (cl-vectorizer::run-command-return-output (cl-vectorizer::get-identify-path) :arguments (list "-format" "'%[width] %[height] %[xresolution] %[yresolution]'" path)))
