# Start with a slim Debian image
FROM debian:stable-slim

# Set environment variables for non-interactive installs
ENV DEBIAN_FRONTEND=noninteractive

# Install basic dependencies (curl, wget, git, etc.)
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    git \
    build-essential \
    python3 \
    python3-pip \
    python3-venv \
    ghc \
    cabal-install \
    && apt-get clean

# Set up Haskell (GHC & Cabal)
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:${PATH}"

# Set up Python (create a virtual environment)
RUN python3 -m venv /venv
ENV PATH="/venv/bin:${PATH}"

# Install Python packages inside the virtual environment
RUN pip install --upgrade pip && pip install numpy pandas matplotlib jupyter

# Set the working directory to the project folder
WORKDIR /workspace

# Command to run the container (could be modified for your needs)
CMD ["bash"]
