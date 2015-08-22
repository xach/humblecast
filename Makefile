QUICKLISP=$(HOME)/quicklisp/

QLCMD=sbcl --noinform --no-userinit --no-sysinit --non-interactive --load "$(QUICKLISP)/setup.lisp"

humblecast: *.lisp *.asd system-index.txt
	buildapp --output humblecast --load-system humblecast --asdf-path . \
		--manifest-file system-index.txt --entry humblecast::main

system-index.txt: deps.txt
	$(QLCMD) --eval '(ql:write-asdf-manifest-file "system-index.txt")'

deps.txt:
	$(QLCMD) --non-interactive \
		--eval "(push '*default-pathname-defaults* asdf:*central-registry*)" \
		--eval "(ql:quickload :humblecast)"
	touch deps.txt

clean:
	rm -f deps.txt system-index.txt humblecast

