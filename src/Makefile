# Space Language F* Build

FSTAR = fstar.exe
FSTAR_FLAGS = --z3version 4.15.4

# Core files in dependency order
CORE_FILES = \
  Space.Types.fst \
  Space.Stack.fst \
  Space.Stack.Properties.fst \
  Space.Discipline.fst \
  Space.Memory.fst \
  Space.Universe.fst \
  Space.Arithmetic.fst \
  Space.Bitwise.fst \
  Space.Comparison.fst \
  Space.World.fst \
  Space.Control.fst \
  Space.Transfer.fst \
  Space.Borrow.fst \
  Space.Borrow.Properties.fst \
  Space.BorrowOps.fst \
  Space.Warp.fst \
  Space.Warp.Properties.fst \
  Space.WarpOps.fst

# UCD tables (large generated files)
UCD_FILES = \
  Space.Text.UCD.Types.fst \
  Space.Text.UCD.CCC.fst \
  Space.Text.UCD.Decomp.fst \
  Space.Text.UCD.Comp.fst \
  Space.Text.UCD.Case.fst \
  Space.Text.UCD.fst

# Text processing
TEXT_FILES = \
  Space.Text.Types.fst \
  Space.Text.UTF8.fst \
  Space.Text.Grapheme.fst \
  Space.Text.Create.fst \
  Space.Text.Iter.fst \
  Space.Text.Ops.fst \
  Space.Text.Props.fst \
  Space.Text.Warp.fst \
  Space.Text.Codepoint.fst \
  Space.Text.Properties.fst \
  Space.Text.UTF16.fst \
  Space.Text.Normalize.fst \
  Space.Text.Case.fst

# Interpreter
INTERP_FILES = \
  Space.Instruction.fst \
  Space.Execute.fst \
  Space.Interpreter.fst \
  Space.Step.fst

# System and properties
SYSTEM_FILES = \
  Space.System.fst \
  Space.System.Universes.fst \
  Space.Bytes.fst \
  Space.Universe.Properties.fst \
  Space.Arithmetic.Properties.fst

SPACE_FILES = $(CORE_FILES) $(UCD_FILES) $(TEXT_FILES) $(INTERP_FILES) $(SYSTEM_FILES)

COMPILER_FILES = \
  Space.Compiler.Token.fst \
  Space.Compiler.Lexer.fst \
  Space.Compiler.AST.fst \
  Space.Compiler.Parser.fst \
  Space.Compiler.Bytecode.fst \
  Space.Compiler.Codegen.fst \
  Space.Compiler.Decoder.fst \
  Space.Compiler.Test.fst

.PHONY: all verify verify-core verify-compiler extract extract-c repl clean

all: verify

# Verify core only (faster iteration)
verify-core:
	$(FSTAR) $(FSTAR_FLAGS) $(CORE_FILES)

# Verify all Space files
verify:
	$(FSTAR) $(FSTAR_FLAGS) $(SPACE_FILES)

# Verify with compiler
verify-compiler:
	$(FSTAR) $(FSTAR_FLAGS) $(SPACE_FILES) $(COMPILER_FILES)

verify-all: verify verify-compiler

# Extract to OCaml
extract-ocaml:
	mkdir -p _output/ocaml
	$(FSTAR) $(FSTAR_FLAGS) --odir _output/ocaml --codegen OCaml $(SPACE_FILES)
	@echo "OCaml files in _output/ocaml/"

# Extract to Karamel (.krml files) - requires karamel
extract-krml:
	mkdir -p _output/krml
	$(FSTAR) $(FSTAR_FLAGS) --odir _output/krml --codegen krml $(SPACE_FILES)
	@echo "Krml files in _output/krml/"

# Generate C code from Karamel - requires krml
extract-c: extract-krml
	cd _output/krml && krml *.krml \
		-skip-linking \
		-no-prefix Space \
		-bundle 'Space.*' \
		-o space
	@echo "C files generated in _output/krml/"

# Build OCaml REPL
repl: extract-ocaml
	cd _output/ocaml && \
		ocamlfind ocamlopt -package zarith -linkpkg \
		-I +zarith \
		*.ml -o space-repl
	@echo "Built _output/ocaml/space-repl"

# Extract compiler to C
extract-compiler:
	mkdir -p _output
	$(FSTAR) $(FSTAR_FLAGS) --odir _output --codegen krml $(SPACE_FILES) $(COMPILER_FILES)
	cd _output && krml *.krml \
		-skip-linking \
		-no-prefix Space \
		-bundle 'Space.*' \
		-o spacec
	@echo "Compiler C files in _output/"

clean:
	rm -f *.checked *.krml
	rm -rf _output
	rm -f space-repl
