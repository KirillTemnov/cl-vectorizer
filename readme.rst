Векторизатор на лиспе (sbcl)
============================

Обработка изображения производится с помощью пакета imagemagick.



Установка через asdf
--------------------

Скачать исходники в каталог, потом выполнить команду.

  .. code-block:: 
     $ sbcl --eval "(require 'asdf)" --eval "(require 'asdf-install)" \
          --eval "(asdf-install:install \"cl-vectorizer.tar.gz\")" --eval "(quit)"


Пример работы
-------------

  .. code-block:: common-lisp
      (require 'asdf)
      (require 'cl-vectorizer)

      ;; "/" в конце обязательно		
      (cl-vectorizer:set-working-dir-in #p"/path/to/dir/in/")
      (cl-vectorizer:set-working-dir-out #p"/path/to/dir/out/")

      (time (cl-vectorizer:thin-image-file #p"image.tif"))


