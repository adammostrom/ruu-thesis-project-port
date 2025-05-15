# Start from official Python 3.10 image (Debian-based)
FROM python:3.10-slim-buster

# Set working directory
WORKDIR /workspace

# Install system deps
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    build-essential \
    libgmp-dev \
    git \
    ca-certificates \
    xz-utils \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install GHC and Cabal via GHCup (lets you pick specific versions)
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh \
    && ~/.ghcup/bin/ghcup install ghc 9.4.8 \
    && ~/.ghcup/bin/ghcup set ghc 9.4.8 \
    && ~/.ghcup/bin/ghcup install cabal \
    && ~/.ghcup/bin/ghcup set cabal

# Add ghcup tools to PATH
ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:$PATH"

# Install Python deps
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy project files
COPY . .

# Default command
CMD ["bash"]
