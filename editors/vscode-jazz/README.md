# Jazz for Visual Studio Code

This directory contains dependency-free syntax highlighting and basic editor
configuration for Jazz `.jz` files. It intentionally provides lexical editor
support only: there is no language server, formatter, completion engine, or
semantic-token implementation.

Constructor names in `data` declarations use the dedicated
`entity.name.function.constructor.jazz` TextMate scope, independently of their
payload types. Selected constructor names in grouped module exports use that
same constructor scope while the owning type retains `entity.name.type.jazz`.
The active editor theme determines the rendered colors.

The extension source lives at `editors/vscode-jazz` from the repository root.
It is not published on the Visual Studio Marketplace.

## Install from a checkout

For a manual installation, copy the extension directory into VS Code's local
extensions directory and reload the editor window. On macOS or Linux, run these
commands from the Jazz repository root:

```sh
mkdir -p "$HOME/.vscode/extensions/jazz-lang.jazz-language-0.1.0"
cp -R editors/vscode-jazz/. "$HOME/.vscode/extensions/jazz-lang.jazz-language-0.1.0/"
```

On Windows, copy `editors/vscode-jazz` to
`%USERPROFILE%\.vscode\extensions\jazz-lang.jazz-language-0.1.0`. VS Code will then
associate `.jz` files with the `Jazz` language and the `source.jazz` TextMate
grammar.

## Build a VSIX

The extension has no runtime dependencies. To package the checked-in sources
with the standard VS Code extension tool, run:

```sh
cd editors/vscode-jazz
npx --yes @vscode/vsce package
code --install-extension jazz-language-0.1.0.vsix
```

The packaging tool is downloaded for this command only; it is not a Jazz build
or runtime dependency. Packaging a VSIX does not publish it to a marketplace.
