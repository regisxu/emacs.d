---
name: emacsclient-elisp-debug
description: |
  Debug and test Emacs Lisp code by connecting to a running Emacs server via emacsclient.
  Use when Kimi needs to verify Emacs configuration changes, inspect variable values,
  test function behavior, check advice status, or debug elisp code without restarting Emacs.
  Applicable whenever the user's Emacs is running in server mode (emacs --daemon or
  (server-start)) and Kimi needs to execute elisp remotely.
---

# Emacsclient Elisp Debug

Debug and test Emacs Lisp in a live Emacs session without restarting.

## Prerequisites

- Emacs must be running in server mode (`emacs --daemon` or `(server-start)` in init)
- `emacsclient` binary must be available on PATH

## Core Technique

PowerShell has severe quote-escaping issues when passing elisp strings to `emacsclient --eval`.
Use one of these two reliable approaches.

### Approach A: Write a temp .el file and load it

Most robust. Write elisp to a temp file, then tell emacsclient to load it.

```powershell
# Write elisp code to a temp file
$elCode = @"
(progn
  (message "Result: %s" (some-function))
  (with-temp-buffer
    (insert (format "Result: %s" (some-function)))
    (write-file "c:/temp/output.txt")))
"@
$elCode | Out-File -Encoding UTF8 c:\temp\test.el

# Load it via emacsclient
$psi = New-Object System.Diagnostics.ProcessStartInfo
$psi.FileName = "emacsclient.exe"
$psi.Arguments = '--eval "(load-file \"c:/temp/test.el\")"'
$psi.UseShellExecute = $false
$psi.RedirectStandardOutput = $true
$psi.RedirectStandardError = $true
$p = [System.Diagnostics.Process]::Start($psi)
$p.WaitForExit()
$p.StandardOutput.ReadToEnd()
```

**To capture output**, write results to a temp file inside the elisp code, then read the file with PowerShell.

### Approach B: Use ProcessStartInfo for inline --eval

For simple one-liners, use .NET `ProcessStartInfo` to bypass PowerShell's argument parsing.

```powershell
$psi = New-Object System.Diagnostics.ProcessStartInfo
$psi.FileName = "emacsclient.exe"
$psi.Arguments = '--eval "(+ 1 2 3)"'
$psi.UseShellExecute = $false
$psi.RedirectStandardOutput = $true
$psi.RedirectStandardError = $true
$p = [System.Diagnostics.Process]::Start($psi)
$p.WaitForExit()
$p.StandardOutput.ReadToEnd()
```

## Common Debug Patterns

### Check if a function exists and get its definition
```elisp
(symbol-function 'my-function)
```

### List all advice on a function
```elisp
(advice-mapc (lambda (props function)
               (message "%S -> %S" props function))
             'target-function)
```

### Check all buffers
```elisp
(mapconcat #'buffer-name (buffer-list) "\n")
```

### Get the *Messages* buffer content
```elisp
(with-temp-buffer
  (insert-buffer "*Messages*")
  (buffer-string))
```

### Disassemble a function to inspect byte-code
```elisp
(disassemble (symbol-function 'target-function))
```

### Check if a buffer exists by name
```elisp
(get-buffer "*rg-defun*")
```

### Load the user's init.el to verify changes
```elisp
(load-file "~/.emacs.d/init.el")
```

## Important Tips

- **Always write output to a temp file** when you need to read complex results back. `emacsclient` stdout can be unreliable for multi-line output.
- **Check if Emacs server is alive** first: `Get-Process emacs`
- **If emacsclient hangs**, the Emacs server may be stuck in an interactive prompt (yes/no, debugger, etc.). Kill the stale emacsclient process and try again.
- **For debugging advice issues**, remember `symbol-function` returns the advised wrapper, not the original. Use `advice--cdr (advice--symbol-function 'fn)` to get the underlying function.
