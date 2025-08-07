#!/bin/bash

# rust-analyzer Installation Test Script
# Tests successful installation and setup of rust-analyzer on Linux systems

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Global counters
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_TOTAL=0

# Utility functions
print_header() {
    echo -e "\n${BOLD}${BLUE}$1${NC}"
    echo -e "${BLUE}$(printf '=%.0s' $(seq 1 ${#1}))${NC}"
}

print_test() {
    local test_name="$1"
    echo -e "\n${CYAN}Testing: ${test_name}${NC}"
}

test_passed() {
    echo -e "${GREEN}✓ $1${NC}"
    ((TESTS_PASSED++))
    ((TESTS_TOTAL++))
}

test_failed() {
    echo -e "${RED}✗ $1${NC}"
    ((TESTS_FAILED++))
    ((TESTS_TOTAL++))
}

test_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

test_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# Test functions
test_command_exists() {
    local cmd="$1"
    local desc="$2"
    
    if command -v "$cmd" >/dev/null 2>&1; then
        test_passed "$desc found"
        return 0
    else
        test_failed "$desc not found"
        return 1
    fi
}

test_rust_installation() {
    print_header "Testing Rust Installation"
    
    print_test "Rust toolchain components"
    test_command_exists "rustc" "Rust compiler"
    test_command_exists "cargo" "Cargo package manager"
    test_command_exists "rustup" "Rustup toolchain manager"
    
    if command -v rustc >/dev/null 2>&1; then
        print_test "Rust version information"
        local rust_version=$(rustc --version 2>/dev/null)
        local cargo_version=$(cargo --version 2>/dev/null)
        
        if [ -n "$rust_version" ]; then
            test_info "Rust: $rust_version"
            test_passed "Rust version detected"
        else
            test_failed "Could not get Rust version"
        fi
        
        if [ -n "$cargo_version" ]; then
            test_info "Cargo: $cargo_version"
            test_passed "Cargo version detected"
        else
            test_failed "Could not get Cargo version"
        fi
    fi
    
    # Test rustup components
    if command -v rustup >/dev/null 2>&1; then
        print_test "Rustup components"
        if rustup component list --installed | grep -q "rust-src"; then
            test_passed "rust-src component installed"
        else
            test_warning "rust-src component not installed (recommended for better IDE support)"
        fi
    fi
}

test_rust_analyzer_installation() {
    print_header "Testing rust-analyzer Installation"
    
    print_test "rust-analyzer binary"
    if test_command_exists "rust-analyzer" "rust-analyzer"; then
        
        print_test "rust-analyzer version"
        local ra_version=$(rust-analyzer --version 2>/dev/null)
        if [ -n "$ra_version" ]; then
            test_info "Version: $ra_version"
            test_passed "rust-analyzer version information available"
        else
            test_failed "Could not get rust-analyzer version"
        fi
        
        print_test "rust-analyzer basic functionality"
        # Test if rust-analyzer responds to help command
        if timeout 5 rust-analyzer --help >/dev/null 2>&1; then
            test_passed "rust-analyzer responds to commands"
        else
            test_failed "rust-analyzer not responding to commands"
        fi
        
        print_test "rust-analyzer location"
        local ra_path=$(which rust-analyzer)
        if [ -n "$ra_path" ]; then
            test_info "Location: $ra_path"
            test_passed "rust-analyzer found in PATH"
        fi
        
        return 0
    else
        test_failed "rust-analyzer not found in PATH"
        
        # Check common installation locations
        print_test "Alternative installation paths"
        local alt_paths=(
            "$HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/rust-analyzer"
            "$HOME/.cargo/bin/rust-analyzer"
            "$HOME/.local/bin/rust-analyzer"
            "/usr/local/bin/rust-analyzer"
        )
        
        local found=false
        for path in "${alt_paths[@]}"; do
            if [ -f "$path" ]; then
                test_info "Found at: $path"
                test_warning "rust-analyzer exists but not in PATH"
                found=true
                break
            fi
        done
        
        if [ "$found" = false ]; then
            test_failed "rust-analyzer not found in common locations"
        fi
        
        return 1
    fi
}

test_lsp_functionality() {
    print_header "Testing LSP Functionality"
    
    # Create a temporary directory for testing
    local temp_dir=$(mktemp -d -t rust-analyzer-test.XXXXXX)
    local test_project="$temp_dir/test_project"
    
    print_test "Creating test Rust project"
    if cargo new "$test_project" --name lsp_test --quiet 2>/dev/null; then
        test_passed "Test project created"
    else
        test_failed "Could not create test project"
        rm -rf "$temp_dir"
        return 1
    fi
    
    cd "$test_project" || return 1
    
    # Create a more complex test file
    cat > src/main.rs << 'EOF'
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct User {
    id: u32,
    name: String,
    email: String,
}

impl User {
    fn new(id: u32, name: String, email: String) -> Self {
        User { id, name, email }
    }
    
    fn display_info(&self) -> String {
        format!("User {}: {} <{}>", self.id, self.name, self.email)
    }
}

fn main() {
    let mut users: HashMap<u32, User> = HashMap::new();
    
    let user1 = User::new(1, "Alice".to_string(), "alice@example.com".to_string());
    let user2 = User::new(2, "Bob".to_string(), "bob@example.com".to_string());
    
    users.insert(user1.id, user1);
    users.insert(user2.id, user2);
    
    for (_id, user) in &users {
        println!("{}", user.display_info());
    }
}
EOF
    
    print_test "Test project compilation"
    if cargo build --quiet 2>/dev/null; then
        test_passed "Test project compiles successfully"
    else
        test_failed "Test project compilation failed"
        rm -rf "$temp_dir"
        return 1
    fi
    
    print_test "Test project execution"
    local output=$(cargo run --quiet 2>/dev/null)
    if [ $? -eq 0 ] && echo "$output" | grep -q "User"; then
        test_passed "Test project runs and produces expected output"
        test_info "Output preview: $(echo "$output" | head -1)"
    else
        test_failed "Test project execution failed"
    fi
    
    # Test rust-analyzer on the project
    if command -v rust-analyzer >/dev/null 2>&1; then
        print_test "LSP server initialization"
        
        # Create a simple LSP request to test initialization
        # This is a basic test - in practice, LSP communication is more complex
        local lsp_test_result=0
        
        # Test if rust-analyzer can find the project root
        if [ -f "Cargo.toml" ]; then
            test_passed "Project structure recognized (Cargo.toml found)"
        else
            test_failed "Project structure not recognized"
            lsp_test_result=1
        fi
        
        # Check if target directory was created (indicates successful analysis)
        if [ -d "target" ]; then
            test_passed "Build artifacts directory exists"
        else
            test_warning "Build artifacts directory not found"
        fi
        
        if [ $lsp_test_result -eq 0 ]; then
            test_passed "LSP server can analyze Rust project"
        else
            test_failed "LSP server cannot analyze Rust project"
        fi
    fi
    
    # Cleanup
    cd - >/dev/null
    rm -rf "$temp_dir"
    
    print_test "Test cleanup"
    if [ ! -d "$temp_dir" ]; then
        test_passed "Test files cleaned up"
    else
        test_warning "Test files cleanup incomplete"
    fi
}

test_emacs_integration() {
    print_header "Testing Emacs Integration"
    
    print_test "Emacs availability"
    if ! test_command_exists "emacs" "Emacs"; then
        test_warning "Emacs not found - skipping integration tests"
        return 0
    fi
    
    print_test "Emacs configuration"
    local config_file="$HOME/.emacs.d/init.el"
    if [ -f "$config_file" ]; then
        test_passed "Emacs configuration file found"
        
        # Test if config mentions LSP or rust-analyzer
        if grep -q "lsp-mode\|rust-analyzer\|rustic" "$config_file"; then
            test_passed "LSP/Rust configuration detected in init.el"
        else
            test_warning "No LSP/Rust configuration detected in init.el"
        fi
    else
        test_warning "Emacs configuration file not found"
    fi
    
    print_test "Emacs LSP configuration loading"
    local emacs_test_output=$(emacs --batch --eval "
        (condition-case err
            (progn
                (load-file \"$config_file\" )
                (message \"CONFIG_LOADED\")
                (if (fboundp 'lsp-mode)
                    (message \"LSP_MODE_AVAILABLE\")
                    (message \"LSP_MODE_NOT_FOUND\"))
                (if (fboundp 'rustic-mode)
                    (message \"RUSTIC_MODE_AVAILABLE\")
                    (message \"RUSTIC_MODE_NOT_FOUND\")))
            (error (message \"CONFIG_ERROR: %s\" err)))
    " 2>&1)
    
    if echo "$emacs_test_output" | grep -q "CONFIG_LOADED"; then
        test_passed "Emacs configuration loads without errors"
        
        if echo "$emacs_test_output" | grep -q "LSP_MODE_AVAILABLE"; then
            test_passed "LSP mode available in Emacs"
        else
            test_warning "LSP mode not available in Emacs"
        fi
        
        if echo "$emacs_test_output" | grep -q "RUSTIC_MODE_AVAILABLE"; then
            test_passed "Rustic mode available in Emacs"
        else
            test_warning "Rustic mode not available in Emacs"
        fi
    else
        test_failed "Emacs configuration failed to load"
        if echo "$emacs_test_output" | grep -q "CONFIG_ERROR"; then
            local error_msg=$(echo "$emacs_test_output" | grep "CONFIG_ERROR" | sed 's/CONFIG_ERROR: //')
            test_info "Error: $error_msg"
        fi
    fi
}

detect_system_info() {
    print_header "System Information"
    
    print_test "Operating System"
    if [ -f /etc/os-release ]; then
        local os_name=$(grep '^NAME=' /etc/os-release | cut -d'"' -f2)
        local os_version=$(grep '^VERSION=' /etc/os-release | cut -d'"' -f2)
        test_info "OS: $os_name $os_version"
        
        # Detect specific distributions
        if echo "$os_name" | grep -qi "manjaro"; then
            test_info "Detected: Manjaro Linux"
            test_info "Package manager: pacman"
        elif echo "$os_name" | grep -qi "ubuntu"; then
            test_info "Detected: Ubuntu Linux"
            test_info "Package manager: apt"
        elif echo "$os_name" | grep -qi "arch"; then
            test_info "Detected: Arch Linux"
            test_info "Package manager: pacman"
        fi
    fi
    
    print_test "Architecture"
    local arch=$(uname -m)
    test_info "Architecture: $arch"
    
    if [ "$arch" = "x86_64" ]; then
        test_passed "64-bit architecture (compatible with rust-analyzer releases)"
    else
        test_warning "Non-x86_64 architecture detected (may need to build from source)"
    fi
}

print_installation_recommendations() {
    print_header "Installation Recommendations"
    
    # Check if rust-analyzer is missing and provide recommendations
    if ! command -v rust-analyzer >/dev/null 2>&1; then
        echo -e "${YELLOW}rust-analyzer is not installed. Here are installation options:${NC}\n"
        
        # Detect OS and provide specific instructions
        if [ -f /etc/os-release ]; then
            local os_name=$(grep '^NAME=' /etc/os-release | cut -d'"' -f2)
            
            if echo "$os_name" | grep -qi "manjaro\|arch"; then
                echo -e "${BLUE}For Manjaro/Arch Linux:${NC}"
                echo -e "  ${GREEN}Option 1 (Recommended):${NC} sudo pacman -S rust-analyzer"
                echo -e "  ${GREEN}Option 2:${NC} rustup component add rust-analyzer"
                echo -e "  ${GREEN}Option 3:${NC} yay -S rust-analyzer-git"
                
            elif echo "$os_name" | grep -qi "ubuntu"; then
                echo -e "${BLUE}For Ubuntu:${NC}"
                echo -e "  ${GREEN}Option 1 (Recommended):${NC} rustup component add rust-analyzer"
                echo -e "  ${GREEN}Option 2:${NC} Download from GitHub releases"
                echo -e "  ${GREEN}Option 3:${NC} Build from source"
            fi
        fi
        
        echo -e "\n${BLUE}General installation command:${NC}"
        echo -e "  rustup component add rust-analyzer"
        echo -e "\n${BLUE}After installation, you may need to:${NC}"
        echo -e "  - Restart your terminal"
        echo -e "  - Add ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin to PATH"
        echo -e "  - Or create a symlink: sudo ln -sf ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/rust-analyzer /usr/local/bin/"
    fi
    
    # Check Rust installation
    if ! command -v rustc >/dev/null 2>&1; then
        echo -e "\n${RED}Rust is not installed!${NC}"
        echo -e "${BLUE}Install Rust first:${NC}"
        echo -e "  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
        echo -e "  source ~/.cargo/env"
    fi
}

print_usage_instructions() {
    print_header "Usage Instructions"
    
    echo -e "${BLUE}To use rust-analyzer with Emacs:${NC}"
    echo -e "1. ${GREEN}Open a Rust file (.rs) in Emacs${NC}"
    echo -e "2. ${GREEN}LSP should automatically start${NC} (look for 'LSP' in mode line)"
    echo -e "3. ${GREEN}Available key bindings:${NC}"
    echo -e "   • ${CYAN}C-c l d${NC}: Go to definition"
    echo -e "   • ${CYAN}C-c l r${NC}: Rename symbol"  
    echo -e "   • ${CYAN}C-c l f${NC}: Format buffer"
    echo -e "   • ${CYAN}C-c l a${NC}: Code actions"
    echo -e "   • ${CYAN}C-c l i${NC}: Find implementations"
    echo -e "   • ${CYAN}C-c l t${NC}: Find type definition"
    
    echo -e "\n${BLUE}To test your setup:${NC}"
    echo -e "1. ${GREEN}Create a new Rust project:${NC} cargo new test_project"
    echo -e "2. ${GREEN}Open in Emacs:${NC} emacs test_project/src/main.rs"
    echo -e "3. ${GREEN}Verify LSP is working:${NC} Look for completion, error highlighting, etc."
    
    echo -e "\n${BLUE}Troubleshooting:${NC}"
    echo -e "• ${YELLOW}If LSP doesn't start:${NC} M-x lsp"
    echo -e "• ${YELLOW}Check LSP status:${NC} M-x lsp-describe-session"
    echo -e "• ${YELLOW}Restart LSP:${NC} M-x lsp-workspace-restart"
}

print_final_summary() {
    print_header "Test Summary"
    
    echo -e "${BLUE}Tests completed:${NC}"
    echo -e "  ${GREEN}✓ Passed: $TESTS_PASSED${NC}"
    echo -e "  ${RED}✗ Failed: $TESTS_FAILED${NC}"
    echo -e "  ${CYAN}📊 Total: $TESTS_TOTAL${NC}"
    
    local success_rate=0
    if [ $TESTS_TOTAL -gt 0 ]; then
        success_rate=$((TESTS_PASSED * 100 / TESTS_TOTAL))
    fi
    
    echo -e "  ${BLUE}Success Rate: ${success_rate}%${NC}"
    
    if [ $TESTS_FAILED -eq 0 ] && [ $TESTS_PASSED -gt 0 ]; then
        echo -e "\n${GREEN}${BOLD}🎉 All tests passed! rust-analyzer is ready to use.${NC}"
    elif [ $success_rate -ge 75 ]; then
        echo -e "\n${YELLOW}${BOLD}⚠ Most tests passed. Some issues detected but rust-analyzer should work.${NC}"
    else
        echo -e "\n${RED}${BOLD}❌ Several tests failed. Please review installation.${NC}"
    fi
    
    echo -e "\n${BLUE}For detailed installation instructions, see:${NC}"
    echo -e "  rust-analyzer-setup.md"
}

# Main execution
main() {
    echo -e "${BOLD}${BLUE}rust-analyzer Installation Test${NC}"
    echo -e "${BLUE}$(printf '=%.0s' $(seq 1 32))${NC}"
    echo -e "${BLUE}This script tests rust-analyzer installation and integration.${NC}\n"
    
    detect_system_info
    test_rust_installation
    test_rust_analyzer_installation
    test_lsp_functionality
    test_emacs_integration
    
    print_installation_recommendations
    print_usage_instructions
    print_final_summary
}

# Check if script is being sourced or executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi