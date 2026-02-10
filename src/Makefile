# Space Language F* Build

FSTAR = fstar.exe
Z3_VERSION = 4.15.4
FSTAR_FLAGS = --z3version $(Z3_VERSION)

SPACE_FILES = \
  Space.Types.fst \
  Space.Stack.fst \
  Space.Stack.Properties.fst \
  Space.Discipline.fst \
  Space.Universe.fst \
  Space.Arithmetic.fst \
  Space.Bitwise.fst \
  Space.Comparison.fst \
  Space.World.fst \
  Space.Control.fst \
  Space.Transfer.fst \
  Space.Instruction.fst \
  Space.Interpreter.fst \
  Space.Execute.fst \
  Space.Step.fst \
  Space.Memory.fst \
  Space.Borrow.fst \
  Space.Borrow.Properties.fst \
  Space.BorrowOps.fst \
  Space.Warp.fst \
  Space.Warp.Properties.fst \
  Space.WarpOps.fst \
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
  Space.System.fst \
  Space.System.Universes.fst \
  Space.Bytes.fst \
  Space.Universe.Properties.fst \
  Space.Arithmetic.Properties.fst \
  Space.Text.UTF16.fst \
  Space.Text.Normalize.fst \
  Space.Text.Case.fst

COMPILER_FILES = \
  Space.Compiler.Token.fst \
  Space.Compiler.Lexer.fst \
  Space.Compiler.AST.fst \
  Space.Compiler.Parser.fst \
  Space.Compiler.Bytecode.fst \
  Space.Compiler.Codegen.fst \
  Space.Compiler.Test.fst

.PHONY: all verify verify-compiler extract clean

all: verify

verify:
	$(FSTAR) $(FSTAR_FLAGS) $(SPACE_FILES)

verify-compiler:
	$(FSTAR) $(FSTAR_FLAGS) $(SPACE_FILES) $(COMPILER_FILES)

verify-all: verify verify-compiler

# Extract to Karamel (requires krml in PATH)
extract:
	$(FSTAR) $(FSTAR_FLAGS) --codegen krml $(SPACE_FILES)
	@echo "Run 'krml *.krml -o space.c' to generate C code"

# Extract compiler to C
extract-compiler:
	$(FSTAR) $(FSTAR_FLAGS) --codegen krml $(SPACE_FILES) $(COMPILER_FILES)
	@echo "Run 'krml *.krml -o spacec' to generate compiler binary"

clean:
	rm -f *.checked *.krml
	rm -rf _output
