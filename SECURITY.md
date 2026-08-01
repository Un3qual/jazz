# Security policy

Jazz is experimental and pre-1.0. Security-sensitive reports are still taken
seriously, but the project does not promise production suitability or long-term
support for development snapshots.

## Supported versions

| Version                 | Supported |
| ----------------------- | --------- |
| `main`                  | Yes       |
| Latest published alpha  | Yes       |
| Older alphas or commits | No        |

No alpha has been published yet. Once alphas are available, only the latest
alpha and the current `main` branch receive security fixes.

## Report a vulnerability privately

Use [GitHub's private vulnerability reporting form](https://github.com/un3qual/jazz/security/advisories/new).
Do not open a public issue for a suspected vulnerability.

Include, where possible:

- the affected revision or alpha version;
- the affected platform and toolchain;
- a minimal reproduction or proof of concept;
- the expected and observed behavior;
- the security impact and realistic attack scenario; and
- any known mitigations or suggested fixes.

Maintainers aim to acknowledge a report and provide an initial assessment
within seven days. This is a best-effort target, not a guaranteed response SLA.
Complex reports may take longer to reproduce or assess.

Please allow maintainers a reasonable opportunity to investigate, fix, and
coordinate disclosure before publishing details. The reporter and maintainers
should agree on disclosure timing based on severity, exploitability, and fix
availability. Credit will be offered when desired and appropriate.

Crashes, incorrect diagnostics, language-design concerns, and other ordinary
bugs without a security impact belong in the
[public issue tracker](https://github.com/un3qual/jazz/issues).
