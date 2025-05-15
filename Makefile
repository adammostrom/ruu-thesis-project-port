HASKELL_DIR = haskell
PYTHON_DIR  = python

# Targets
.PHONY: all haskell_clean haskell_build haskell_test run_haskell install_python python-test

# Clean Haskell build artifacts
haskell-clean:
	@echo "Cleaning Haskell build..."
	cabal clean

# Build Haskell project
haskell-build:
	@echo "Building Haskell project..."
	cabal build -v2

haskell-run:
	@echo "Starting cabal"
	cabal repl -v0

# Run the haskell tests
haskell-test:
	@echo "Running property tests..."
	cabal test -v0

python-test:
	@echo "Launching python test suite"
	cd $(PYTHON_DIR) && pytest -s tests/test_propertiesMemo.py

python-run:
	@echo "Running Greenhouse Gas implementation..."
	cd $(PYTHON_DIR) && python3 main.py

