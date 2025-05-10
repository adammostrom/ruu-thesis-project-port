# Start with an official Python image (on top of Debian)
FROM python:3.10-slim-buster

# Set the working directory to the project folder
WORKDIR /workspace

# Install basic system dependencies that might be needed
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    build-essential \
    # Add any other essential system packages here
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Copy the project's requirements file if you have one
# This allows Docker to cache the dependencies layer if requirements.txt doesn't change
# TODO specify versions in that file
COPY requirements.txt .

# TODO probably should set up venv here

# Install Python dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Copy the rest of your project's source code
COPY . .

# If you need Haskell, install it in a separate step.
# TODO specify ghc version
RUN apt-get update && apt-get install -y --no-install-recommends \
    ghc \   
    cabal-install \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Set up Haskell environment (adjust as needed)
ENV PATH="/root/.cabal/bin:${PATH}"

# Command to run the container. Adjust this based on your application's needs.
# TODO probably needs adjusting - run testsuite as default?
CMD ["bash"]
