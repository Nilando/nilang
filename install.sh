#!/bin/bash
set -e

# Nilang Installation Script
# This script installs nilang and its standard library

INSTALL_DIR="$HOME/.nilang"
STD_LIB_DIR="$INSTALL_DIR/std"
REPO_URL="https://github.com/Nilando/nilang"
BRANCH="main"

echo "========================================="
echo "  Nilang Installer"
echo "========================================="
echo ""

# Check if cargo is installed
if ! command -v cargo &> /dev/null; then
    echo "Error: cargo is not installed"
    echo "Please install Rust and cargo from https://rust-lang.org/tools/install/"
    exit 1
fi

# Install nilang via cargo
echo "📦 Installing nilang..."
cargo install nilang

echo ""
echo "✓ Nilang installed successfully"
echo ""

# Download and install standard library
echo "📚 Installing standard library..."
mkdir -p "$STD_LIB_DIR"

# Download std lib from GitHub
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

echo "  Downloading from $REPO_URL..."
if command -v curl &> /dev/null; then
    curl -sSL "$REPO_URL/archive/refs/heads/$BRANCH.tar.gz" | tar -xz -C "$TEMP_DIR"
elif command -v wget &> /dev/null; then
    wget -qO- "$REPO_URL/archive/refs/heads/$BRANCH.tar.gz" | tar -xz -C "$TEMP_DIR"
else
    echo "Error: Neither curl nor wget is available"
    exit 1
fi

# Copy std library files
if [ -d "$TEMP_DIR/nilang-$BRANCH/std" ]; then
    cp -r "$TEMP_DIR/nilang-$BRANCH/std"/* "$STD_LIB_DIR/"
    echo "✓ Standard library installed to $STD_LIB_DIR"
else
    echo "Error: Could not find std directory in downloaded archive"
    exit 1
fi

echo ""
echo "========================================="
echo "  Installation Complete!"
echo "========================================="
echo ""
echo "Try it out:"
echo "  nilang                    # Start the REPL"
echo "  nilang program.nl         # Run a program"
echo ""
echo "The standard library is installed at:"
echo "  $STD_LIB_DIR/main.nl"
echo ""
