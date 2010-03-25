Векторизатор на лиспе (sbcl)

Обработка изображения производится с помощью пакета imagemagick.

В файле main.lisp содержится основная логика разбиения изображения на фрагменты,
векторизация и пр.



Инструкции для запуска

Новые инструкции
(require 'asdf)
(asdf:oos 'asdf:load-op 'cl-vectorizer)
(cl-vectorizer:set-working-dir-in #p"/path/to/vector-test/in/")
(cl-vectorizer:set-working-dir-out #p"/path/to/vector-test/out/") 
;; "/" в конце обязательно	 
(cl-vectorizer:thin-image-file #p"image.tif")

Для справки 
файл Data003.tif
     конвертация (imagemagick)  86 минут (5167 сек)
     обработка	 (lisp)		181 секунда
