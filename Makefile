HASKELL_DIR = haskell
PYTHON_DIR  = python

# Targets
.PHONY: all haskell_clean haskell_build haskell_test run_haskell install_python python-test

# Clean Haskell build artifacts
haskell_clean:
	@echo "Cleaning Haskell build..."
	cabal clean

# Build Haskell project
haskell_build:
	@echo "Building Haskell project..."
	cabal build -v2

haskell_test:
	@echo "Running property tests..."
	cabal test -v0

python-test:
	@echo "Launching python test suite"
	cd $(PYTHON_DIR) && pytest -s tests/test_propertiesMemo.py

python_run:
	@echo "Running Greenhouse Gas implementation..."
	cd $(PYTHON_DIR) && python3 main.py
