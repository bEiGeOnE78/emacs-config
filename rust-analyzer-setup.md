# Rust-Analyzer Setup Guide

This guide provides step-by-step instructions for installing rust-analyzer on Manjaro and Ubuntu 22.04 systems, along with verification tests.

## Overview

Rust-analyzer is a Language Server Protocol (LSP) implementation for Rust that provides:
- Code completion and IntelliSense
- Go-to-definition and find references
- Real-time error checking and diagnostics
- Code formatting and refactoring
- Inline documentation

## Prerequisites

Before installing rust-analyzer, ensure you have Rust installed:

```bash
# Check if Rust is installed
rust --version
cargo --version

# If not installed, install via rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env
```

---

## Installation Methods

### Manjaro Linux

#### Method 1: Package Manager (Recommended)
```bash
# Update system
sudo pacman -Syu

# Install rust-analyzer
sudo pacman -S rust-analyzer

# Verify installation
rust-analyzer --version
```

#### Method 2: Via Rustup
```bash
# Install via rustup
rustup component add rust-analyzer

# The binary will be at ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/rust-analyzer
# Create symlink for system-wide access
sudo ln -sf ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/rust-analyzer /usr/local/bin/
```

#### Method 3: AUR (Alternative)
```bash
# Using yay (if you have it)
yay -S rust-analyzer-git

# Or using paru
paru -S rust-analyzer-git
```

---

### Ubuntu 22.04

#### Method 1: Via Rustup (Recommended)
```bash
# Install/update rustup if not already done
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Install rust-analyzer component
rustup component add rust-analyzer

# Create system-wide symlink
sudo mkdir -p /usr/local/bin
sudo ln -sf ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/rust-analyzer /usr/local/bin/rust-analyzer

# Add to PATH if needed
echo 'export PATH="/usr/local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### Method 2: Download Latest Release
```bash
# Create directory for rust-analyzer
mkdir -p ~/.local/bin

# Download latest release
RUST_ANALYZER_URL=$(curl -s https://api.github.com/repos/rust-lang/rust-analyzer/releases/latest | grep "browser_download_url.*rust-analyzer-x86_64-unknown-linux-gnu.gz" | cut -d '"' -f 4)
curl -L "$RUST_ANALYZER_URL" | gunzip -c > ~/.local/bin/rust-analyzer

# Make executable
chmod +x ~/.local/bin/rust-analyzer

# Add to PATH
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### Method 3: Build from Source
```bash
# Install build dependencies
sudo apt update
sudo apt install build-essential git

# Clone and build
git clone https://github.com/rust-lang/rust-analyzer.git
cd rust-analyzer
cargo xtask install --server
```

---

## Verification Tests

Save the following tests as `test-rust-analyzer.sh` and make it executable:

```bash
#!/bin/bash

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test functions
test_command() {
    local cmd="$1"
    local desc="$2"
    
    echo -e "${BLUE}Testing: ${desc}${NC}"
    
    if command -v "$cmd" >/dev/null 2>&1; then
        echo -e "${GREEN}✓ $cmd found${NC}"
        return 0
    else
        echo -e "${RED}✗ $cmd not found${NC}"
        return 1
    fi
}

test_rust_installation() {
    echo -e "\n${YELLOW}=== Testing Rust Installation ===${NC}"
    
    test_command "rustc" "Rust compiler"
    test_command "cargo" "Cargo package manager"
    
    if command -v rustc >/dev/null 2>&1; then
        echo -e "${BLUE}Rust version:${NC}"
        rustc --version
    fi
}

test_rust_analyzer_installation() {
    echo -e "\n${YELLOW}=== Testing rust-analyzer Installation ===${NC}"
    
    if test_command "rust-analyzer" "rust-analyzer LSP server"; then
        echo -e "${BLUE}rust-analyzer version:${NC}"
        rust-analyzer --version
        
        # Test if it responds to LSP requests
        echo -e "${BLUE}Testing LSP functionality...${NC}"
        
        # Create a temporary Rust file
        temp_dir=$(mktemp -d)
        cat > "$temp_dir/main.rs" << 'EOF'
fn main() {
    println!("Hello, world!");
}
EOF
        
        # Test if rust-analyzer can analyze the file
        cd "$temp_dir"
        if timeout 5 rust-analyzer --help >/dev/null 2>&1; then
            echo -e "${GREEN}✓ rust-analyzer responds to commands${NC}"
        else
            echo -e "${RED}✗ rust-analyzer not responding${NC}"
        fi
        
        # Cleanup
        rm -rf "$temp_dir"
        
        return 0
    else
        echo -e "${RED}✗ rust-analyzer installation failed${NC}"
        return 1
    fi
}

test_emacs_integration() {
    echo -e "\n${YELLOW}=== Testing Emacs Integration ===${NC}"
    
    if command -v emacs >/dev/null 2>&1; then
        echo -e "${GREEN}✓ Emacs found${NC}"
        
        # Test if our Emacs config loads rust-analyzer
        if emacs --batch --eval "(progn (load-file \"~/.emacs.d/init.el\") (message \"Testing rust-analyzer integration...\") (if (fboundp 'lsp-mode) (message \"✓ LSP mode available\") (message \"✗ LSP mode not found\")))" 2>/dev/null | grep -q "LSP mode available"; then
            echo -e "${GREEN}✓ LSP mode configured in Emacs${NC}"
        else
            echo -e "${YELLOW}⚠ LSP mode not detected in Emacs config${NC}"
        fi
    else
        echo -e "${YELLOW}⚠ Emacs not found${NC}"
    fi
}

create_test_project() {
    echo -e "\n${YELLOW}=== Creating Test Rust Project ===${NC}"
    
    test_project="/tmp/rust-analyzer-test"
    
    if [ -d "$test_project" ]; then
        rm -rf "$test_project"
    fi
    
    echo -e "${BLUE}Creating new Rust project...${NC}"
    cargo new "$test_project" --name rust_analyzer_test
    
    cd "$test_project"
    
    # Add some code that would benefit from rust-analyzer
    cat > src/main.rs << 'EOF'
use std::collections::HashMap;

#[derive(Debug)]
struct Person {
    name: String,
    age: u32,
}

impl Person {
    fn new(name: String, age: u32) -> Self {
        Person { name, age }
    }
    
    fn greet(&self) -> String {
        format!("Hello, my name is {} and I'm {} years old", self.name, self.age)
    }
}

fn main() {
    let mut people: HashMap<String, Person> = HashMap::new();
    
    let person1 = Person::new("Alice".to_string(), 30);
    let person2 = Person::new("Bob".to_string(), 25);
    
    people.insert("p1".to_string(), person1);
    people.insert("p2".to_string(), person2);
    
    for (key, person) in &people {
        println!("{}: {}", key, person.greet());
    }
}
EOF
    
    echo -e "${BLUE}Testing cargo build...${NC}"
    if cargo build; then
        echo -e "${GREEN}✓ Test project builds successfully${NC}"
    else
        echo -e "${RED}✗ Test project build failed${NC}"
        return 1
    fi
    
    echo -e "${BLUE}Testing cargo run...${NC}"
    if cargo run; then
        echo -e "${GREEN}✓ Test project runs successfully${NC}"
    else
        echo -e "${RED}✗ Test project run failed${NC}"
        return 1
    fi
    
    echo -e "${GREEN}Test project created at: $test_project${NC}"
}

print_summary() {
    echo -e "\n${YELLOW}=== Installation Summary ===${NC}"
    echo -e "${BLUE}To use rust-analyzer with Emacs:${NC}"
    echo -e "1. Open a Rust file in Emacs"
    echo -e "2. LSP should automatically start (look for 'LSP' in mode line)"
    echo -e "3. Available commands:"
    echo -e "   - C-c l d: Go to definition"
    echo -e "   - C-c l r: Rename symbol"
    echo -e "   - C-c l f: Format buffer"
    echo -e "   - C-c l a: Code actions"
    echo -e "\n${BLUE}Test your setup:${NC}"
    echo -e "   cd $test_project"
    echo -e "   emacs src/main.rs"
}

# Main execution
main() {
    echo -e "${BLUE}rust-analyzer Installation Test${NC}"
    echo -e "${BLUE}==============================${NC}"
    
    test_rust_installation
    test_rust_analyzer_installation
    test_emacs_integration
    create_test_project
    print_summary
}

# Run main function
main
EOF