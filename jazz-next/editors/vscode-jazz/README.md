# Jazz syntax highlighting for VS Code

This directory contains dependency-free syntax highlighting and basic editor
configuration for Jazz `.jz` files. It intentionally provides lexical editor
support only: there is no language server, formatter, completion engine, or
semantic-token implementation.

The extension source lives in `jazz-next/editors/vscode-jazz`. To use it from a
repository checkout, copy or symlink that directory into your VS Code extensions
directory and reload the editor window. VS Code will then associate `.jz` files
with the `Jazz` language and the `source.jazz` TextMate grammar.

Producing an installable VSIX in the future will require the standard VS Code
extension packaging tools. Those tools are not runtime or build dependencies of
Jazz and are not required to use the checked-in extension directory locally.
