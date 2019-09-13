LISP ?= sbcl

all:
	$(LISP) --eval '(ql:quickload :asdf)' \
		--eval '(ql:quickload :scheme-compiler)' \
		--eval '(asdf:make :scheme-compiler)' \
		--eval '(quit)'
