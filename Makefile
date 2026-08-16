.PHONY: build clean run install test

# Executable name
BINARY = gilt

# Installation directory
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin

# Build the executable
build: $(BINARY)

$(BINARY): build.lisp gilt.asd src/*.lisp
	sbcl --non-interactive --load build.lisp

# Run from source (development)
run:
	sbcl --eval "(require :asdf)" \
	     --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #p\"$(PWD)/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :gilt)" \
	     --eval "(gilt:run)"

# Run the built executable
run-bin: $(BINARY)
	./$(BINARY)

# Install to system (does not rebuild; run 'make build' first)
install:
	@if [ ! -f $(BINARY) ]; then \
		echo "Error: $(BINARY) not found. Run 'make build' first."; \
		exit 1; \
	fi
	install -d $(DESTDIR)$(BINDIR)
	install -m 755 $(BINARY) $(DESTDIR)$(BINDIR)/

# Uninstall from system
uninstall:
	rm -f $(DESTDIR)$(BINDIR)/$(BINARY)

# Clean build artifacts
clean:
	rm -f $(BINARY)
	rm -rf ~/.cache/common-lisp/sbcl-*/**/gilt/

# Load and test in REPL
repl:
	sbcl --eval "(require :asdf)" \
	     --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #p\"$(PWD)/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :gilt)"

# Check syntax without running
check:
	sbcl --eval "(require :asdf)" \
	     --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #p\"$(PWD)/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :gilt)" \
	     --eval "(sb-ext:exit)"

help:
	@echo "Gilt - A LazyGit-style Git TUI in Common Lisp"
	@echo ""
	@echo "Targets:"
	@echo "  build    - Build standalone executable"
	@echo "  run      - Run from source (requires Quicklisp)"
	@echo "  run-bin  - Run the built executable"
	@echo "  install  - Install to $(BINDIR)"
	@echo "  uninstall- Remove from $(BINDIR)"
	@echo "  clean    - Remove build artifacts"
	@echo "  repl     - Start SBCL with gilt loaded"
	@echo "  check    - Check if code compiles"
	@echo "  help     - Show this help"
