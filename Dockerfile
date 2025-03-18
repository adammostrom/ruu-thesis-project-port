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
    software-properties-common \
    lsb-release \
    ca-certificates \
    gnupg \
    && apt-get clean

# Add the deadsnakes repository to install Python 3.10
RUN curl -fsSL https://packages.python.org/debian/python3.10/deadsnakes-archive-keyring.asc | tee /etc/apt/trusted.gpg.d/deadsnakes.asc
RUN echo "deb http://ppa.launchpad.net/deadsnakes/ppa/ubuntu focal main" > /etc/apt/sources.list.d/deadsnakes-ppa.list

# Install Python 3.10 and necessary packages
RUN apt-get update && apt-get install -y \
    python3.10 \
    python3.10-pip \
    python3.10-venv \
    ghc \
    cabal-install \
    && apt-get clean

# Set up Haskell (GHC & Cabal)
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:${PATH}"

# Set up Python (create a virtual environment with Python 3.10)
RUN python3.10 -m venv /venv
ENV PATH="/venv/bin:${PATH}"

# Install Python packages inside the virtual environment
RUN pip install --upgrade pip && pip install numpy pandas matplotlib jupyter

# Set the working directory to the project folder
WORKDIR /workspace

# Command to run the container (could be modified for your needs)
CMD ["bash"]
