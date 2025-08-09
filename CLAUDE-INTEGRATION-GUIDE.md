# Claude Integration for Emacs - User Guide

## Overview

This Emacs configuration implements a two-phase Claude AI integration:
- **Phase 1**: gptel for direct Claude API chat functionality
- **Phase 2**: Custom Claude Code CLI integration with terminal interface

## Prerequisites

1. **Claude Code CLI** - Install from https://github.com/anthropics/claude-code
2. **Claude Pro Subscription** - Required for Claude Code access
3. **Anthropic API Key** (Optional) - Only needed for gptel direct API access

## Phase 1: gptel Chat Integration

### Features
- Direct Claude API chat in Emacs buffers
- Multiple system directive presets
- Org-mode integration
- Streaming responses
- Custom backend configuration

### Key Bindings
- `C-c a c` - Start Claude chat session
- `C-c a s` - Send current region/buffer to Claude  
- `C-c a r` - Set system directive (code-review, debugging, documentation)
- `C-c a k` - Reload API key
- `C-c a d` - Debug configuration
- `C-c a u` - Unified Claude experience menu

### Usage Examples

#### Basic Chat
1. Press `C-c a c` to open a new Claude chat buffer
2. Type your question and press `C-c a s` to send
3. Claude responds with streaming text

#### Code Review
1. Open a code file
2. Select code region or use entire buffer
3. Press `C-c a r` to set code-review directive
4. Press `C-c a s` to send for review

#### System Directives
Available directives:
- `default` - General helpful assistant
- `code-review` - Expert code reviewer focused on quality
- `debugging` - Debugging expert for identifying issues
- `documentation` - Technical writer for clear documentation

## Phase 2: Claude Code CLI Integration

### Features
- Terminal-based Claude Code sessions
- Project detection and isolation
- Multiple command support (init, resume, basic terminal)
- Side window display
- File and region sending
- Interactive menu system

### Key Bindings
- `C-c a i` - Interactive menu for Claude Code options
- `C-c a t` - Start Claude Code terminal
- `C-c a p` - Start with explicit project context
- `C-c a n` - Initialize new Claude session (`claude init`)
- `C-c a R` - Resume previous session (`claude resume`)  
- `C-c a f` - Send current file to Claude terminal
- `C-c a x` - Execute command in Claude context
- `C-c a g` - Send selected region to Claude terminal

### Usage Workflows

#### Starting a New Project
1. Navigate to your project directory
2. Press `C-c a n` to run `claude init`
3. Follow Claude's setup prompts
4. Begin coding with AI assistance

#### Resuming Work
1. Navigate to project with existing Claude session
2. Press `C-c a R` to run `claude resume`
3. Continue previous conversation

#### Interactive Development
1. Press `C-c a t` for basic Claude terminal
2. Type commands directly in the terminal
3. Use `C-c a f` to send current file for analysis
4. Use `C-c a g` to send selected code regions

#### Quick Access Menu
Press `C-c a i` for menu options:
- `t` - Terminal
- `i` - Init new session  
- `r` - Resume session
- `p` - Project context
- `s` - Status

## Unified Interface

### Unified Claude Menu (`C-c a u`)
Choose between different Claude modes:
- `c` - Chat mode (gptel)
- `t` - Terminal mode (Claude Code)
- `p` - Project mode (Claude Code with context)
- `s` - Status information

## Configuration

### API Key Setup
The configuration automatically attempts to load API keys from:
1. `ANTHROPIC_API_KEY` environment variable
2. `~/.claude/.credentials.json` file (OAuth token from Claude Code)
3. auth-source password store

### Customization
Key configuration variables in `config-claude.el`:
- `gptel-model` - Claude model selection
- `gptel-default-mode` - Default mode for chat buffers
- `eat-term-name` - Terminal emulation type
- `eat-kill-buffer-on-exit` - Cleanup behavior

## Project Structure

```
~/.emacs.d/
├── init.el                          # Main init with Claude loading
├── lisp/
│   └── config-claude.el            # Main Claude configuration
├── test-claude-comprehensive.el     # Phase 1 tests
├── test-claude-phase2.el           # Phase 2 tests
└── CLAUDE-INTEGRATION-GUIDE.md     # This guide
```

## Testing

### Automated Tests
- **Phase 1**: `M-x test-claude-comprehensive-run-all`
- **Phase 2**: `M-x test-claude-phase2-run-all`

### Interactive Tests  
- **Phase 1**: `M-x test-claude-comprehensive-interactive`
- **Phase 2**: `M-x test-claude-phase2-interactive`

## Troubleshooting

### Common Issues

1. **Claude CLI not found**
   - Ensure Claude CLI is installed and in PATH
   - Check with `which claude` in terminal
   - GUI Emacs may need explicit PATH configuration

2. **API Authentication Errors**
   - OAuth tokens from Claude Code don't work with direct API
   - Need separate Anthropic API key for gptel
   - Check key format: `sk-ant-api-...` vs `sk-ant-oat01-...`

3. **Terminal Integration Issues**
   - Ensure `eat` package is installed
   - Check project detection with `M-x project-current`
   - Verify terminal backend in configuration

### Debug Commands
- `M-x claude-debug-config` - Check Phase 1 configuration
- `M-x claude-project-status` - Check Phase 2 status
- `M-x describe-key` followed by key binding - Verify bindings

## Limitations

- **No MCP Integration** - Unlike claude-code-ide.el, no bidirectional protocol
- **Manual Context** - Must explicitly send files/regions to Claude
- **Basic Terminal** - Simple wrapper without advanced IDE features
- **No Session Persistence** - Sessions don't restore across Emacs restarts

## Future Enhancements

Potential improvements:
- MCP protocol integration for bidirectional communication
- LSP and tree-sitter integration for better context
- Persistent session management
- Diff view integration with Flycheck/Flymake
- Advanced project analysis tools

---

For technical details and code walkthrough, see the detailed implementation documentation.