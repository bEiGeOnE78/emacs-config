# Prime Directives
- Explain things first before doing them
- Provide an assessment of you findings
- Give me 3 different options and let me choose
- Always ask me for confirmation of next steps prior to changing code

# Problem
I am a software developer. I use Emacs as my primary software developer tool across multiple Linux PCs with different distributions. This folder /.emacs.d contains my current version of my Emacs configuration. I need to make sure this configuration has all of the settings that I need and prefer. I like modularity in my configuration in order to more easily understand my configuration. I write code in multiple languages. I also use Emacs Org Mode along with Org-Roam for time management, todo lists, simple note taking and a zettlekasten. I want this configuration to work across different Linux PCs with possibly different distributions. When deploying to a new PC I want some kind of staged setup so that all dependencies are met without issue.

# Development Process
- I will provide an initial list of requirements
- First review these requirements, inspect my project folder and provide a list of the requirements and how they are implemented. Additionally highlightt which features are not implemented
- We will develop the configuration together in steps
- Describe the changes you are proposing as we go
- We may need to refine the requirements as we develop the configurations
- Keep a log of changes we make in PROJECT.md
- Create unit tests for the configuration if possible
- Leverage version control and GitHub CLI to keep track of changes to the configuration and share it

# Functional Requirements
The Emacs configurations must:
- Prioritize native Emacs functionality, then extend with packages if functionality does not exist
- Work across multiple PCs
- Turn off the toolbar
- Turn off the menu bar
- Display line numbers
- Display relative line numbers
- Support a reading mode for org and markdown files
- Show a dashboard at start up that shows recent files, an entry point to editing my Emacs config
- Add new lines to the botton of a file when pressing C-n
- Use modern indent style at the beginning of lines
- Not force single line comments
- Have sentences that end with a single space
- A completion engnine
- Support executing command with C-x C-m
- Support killing the previous word with C-w
- Support killing a region ith C-x C-k
- Automatically detect proxy and ask to switch to pre-defined proxy settings
- Provide hints for all key commands
- Provide hints for all commands when pressing executing a command
- Support the Eldritch theme
- Support melpa and gnu package archives
- Support using language servers
- Support the following language servers (clangd, rust, matlab, lua)
- Leverage treesitter for syntax highlighting when needed
- Leverage treesitter for view the abstract syntax tree