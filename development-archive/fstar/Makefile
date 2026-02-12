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
  Space.Compiler.CGen.fst \
  Space.Compiler.CGen.UCD.fst \
  Space.Compiler.CGen.Runtime.fst \
  Space.Compiler.Decoder.fst \
  Space.Compiler.Test.fst \
  Space.Compiler.Main.fst \
  Space.Compiler.GenUCD.fst \
  Space.Compiler.GenRuntime.fst

.PHONY: all verify verify-core verify-compiler extract-ocaml extract-fsharp extract-krml extract-c build-fsharp clean

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

# Extract to F#
extract-fsharp:
	mkdir -p _output/fsharp
	$(FSTAR) $(FSTAR_FLAGS) --odir _output/fsharp --codegen FSharp $(SPACE_FILES)
	@echo "F# files in _output/fsharp/"

# Build F# project (creates .NET IL bytecode)
build-fsharp: extract-fsharp
	@if [ ! -f _output/fsharp/Space.fsproj ]; then \
		cd _output/fsharp && dotnet new classlib -lang F# -n Space -o . --force; \
	fi
	cd _output/fsharp && dotnet build
	@echo "Built _output/fsharp/bin/"

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

# Build spacec compiler (OCaml)
spacec: extract-compiler-ocaml
	cd _output/ocaml-compiler && \
		ocamlfind ocamlopt -package zarith -linkpkg \
		-I +zarith \
		*.ml -o spacec
	@echo "Built _output/ocaml-compiler/spacec"
	@echo "Usage: ./spacec input.space > output.c"

# Extract compiler to OCaml
extract-compiler-ocaml:
	mkdir -p _output/ocaml-compiler
	$(FSTAR) $(FSTAR_FLAGS) --odir _output/ocaml-compiler --codegen OCaml \
		$(SPACE_FILES) $(COMPILER_FILES)
	@echo "OCaml compiler files in _output/ocaml-compiler/"

# Generate UCD header from F* (verified generation)
gen-ucd: extract-compiler-ocaml
	cd _output/ocaml-compiler && \
		ocamlfind ocamlopt -package zarith -linkpkg \
		-I +zarith \
		*.ml -o gen-ucd
	./_output/ocaml-compiler/gen-ucd > runtime/space_ucd_full.h
	@echo "Generated runtime/space_ucd_full.h from F*"

# Generate runtime header from F* (verified generation)
gen-runtime: extract-compiler-ocaml
	cd _output/ocaml-compiler && \
		ocamlfind ocamlopt -package zarith -linkpkg \
		-I +zarith \
		*.ml -o gen-runtime
	./_output/ocaml-compiler/gen-runtime > runtime/space_runtime.h
	@echo "Generated runtime/space_runtime.h from F*"

# Generate all runtime headers
gen-headers: gen-ucd gen-runtime
	@echo "All runtime headers generated from F*"

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
