FROM node:22-slim

# Set non-interactive for package installations to avoid prompts
ENV DEBIAN_FRONTEND=noninteractive

# Install base dependencies including Java for Selenium, Python for Bazel, and tools for Chrome
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    unzip \
    gnupg \
    jq \
    default-jre \
    python3 \
    python3-pip \
    pkg-config \
    libgmp-dev \
    libffi-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Bazelisk globally using npm
RUN npm install -g @bazel/bazelisk

# Install Google Chrome Stable
RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
    && dpkg -i google-chrome-stable_current_amd64.deb || apt-get -f install -y \
    && rm google-chrome-stable_current_amd64.deb

# Install Chromedriver with fixed version
RUN wget https://storage.googleapis.com/chrome-for-testing-public/126.0.6478.126/linux64/chromedriver-linux64.zip \
    && unzip chromedriver-linux64.zip \
    && mv chromedriver-linux64/chromedriver /usr/bin/ \
    && rm chromedriver-linux64.zip \
    && rm -rf chromedriver-linux64 \
    && chmod +x /usr/bin/chromedriver

# Fix libtinfo5 for Bazel
RUN wget http://security.ubuntu.com/ubuntu/pool/universe/n/ncurses/libtinfo5_6.3-2ubuntu0.1_amd64.deb \
    && apt install -y ./libtinfo5_6.3-2ubuntu0.1_amd64.deb \
    && rm libtinfo5_6.3-2ubuntu0.1_amd64.deb

# Download Selenium Server
RUN wget https://github.com/SeleniumHQ/selenium/releases/download/selenium-3.9.1/selenium-server-standalone-3.9.1.jar -O /opt/selenium-server-standalone.jar

# Create a non-root user and set home directory
RUN useradd -ms /bin/bash ubuntu
USER ubuntu
WORKDIR /home/ubuntu

# Expose ports for Selenium and a potential web application
EXPOSE 4444
EXPOSE 3000

