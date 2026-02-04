# Cross-Platform Unix Support for Gilt

This document describes the cross-platform terminal handling improvements that enable gilt to work on various Unix systems, including NixOS, different terminal emulators, and other Unix variants.

## Problem Solved

The original gilt codebase had hard-coded assumptions that caused keyboard lockout issues on:
- **NixOS** (different stty and TTY paths)
- **Alacritty terminal** (timing-sensitive escape sequences)
- **Other Unix variants** with non-standard paths

## Solution Overview

The fix implements dynamic system detection and configuration:

### 1. Dynamic Path Resolution
- **stty command**: Automatically finds stty in NixOS locations (`/run/current-system/sw/bin/stty`) and standard Unix paths
- **TTY devices**: Discovers available TTY devices (`/dev/tty`, `/dev/pts/0`, `/dev/console`, etc.)

### 2. Terminal-Specific Optimizations
- **Alacritty detection**: Via `ALACRITTY_SOCKET` environment variable or `TERM` containing "alacritty"
- **Adaptive timing**: Faster escape sequence detection for fast terminals like Alacritty

### 3. Environment Variable Configuration
Users can override automatic detection:

```bash
# Override stty path for custom installations
export GILT_STTY_PATH=/path/to/stty

# Override TTY device
export GILT_TTY_PATH=/dev/pts/1

# Override escape sequence timeout (seconds)
export GILT_ESCAPE_TIMEOUT=0.01
```

### 4. Robust Error Handling
- Multiple fallback mechanisms for stty command execution
- Graceful degradation when terminal features aren't available
- Better error messages for troubleshooting

## Usage

### For NixOS Users
No special configuration needed - gilt will automatically detect and use the correct paths:
```bash
gilt
```

### For Alacritty Users  
Gilt automatically detects Alacritty and optimizes escape sequence handling:
```bash
gilt
```

### Manual Configuration
If automatic detection fails, use environment variables:
```bash
# For NixOS
export GILT_STTY_PATH=/run/current-system/sw/bin/stty
gilt

# For systems with non-standard TTY
export GILT_TTY_PATH=/dev/pts/2
gilt

# For fast terminals needing shorter timeouts
export GILT_ESCAPE_TIMEOUT=0.005
gilt
```

## Diagnostic Tool

Run the diagnostic script to test your system compatibility:
```bash
sbcl --load diagnose.lisp
```

This will check:
- System detection (stty path, TTY path, terminal properties)
- TTY access permissions
- stty command functionality
- Raw mode switching
- Keyboard input reception
- Environment configuration

## Technical Details

### Functions Added

- `find-stty()`: Discovers stty command across Unix systems
- `find-tty()`: Finds available TTY devices
- `detect-terminal-type()`: Identifies terminal emulator and returns optimization settings
- `wait-for-escape-sequence()`: Dynamic timeout for escape sequence detection

### Global Parameters

- `*stty-path*`: Path to stty command (auto-detected or from `GILT_STTY_PATH`)
- `*tty-path*`: Path to TTY device (auto-detected or from `GILT_TTY_PATH`)  
- `*escape-timeout*`: Escape sequence timeout (auto-detected or from `GILT_ESCAPE_TIMEOUT`)

### Backward Compatibility

All changes are backward compatible:
- Existing systems continue to work unchanged
- No breaking changes to API or user interface
- Graceful fallbacks for all new functionality

## Testing

Tested on:
- ✅ NixOS with Alacritty
- ✅ Standard Linux distributions
- ✅ macOS
- ✅ WSL2 Ubuntu
- ✅ Various terminal emulators (xterm, gnome-terminal, kitty, etc.)

## Troubleshooting

If you still experience keyboard lockout:

1. **Run diagnostics**: `sbcl --load diagnose.lisp`
2. **Check environment**: Ensure no conflicting TTY settings
3. **Try manual configuration**: Set `GILT_STTY_PATH` and `GILT_TTY_PATH`
4. **Report issues**: Include diagnostic output in bug reports

## Contributing

When adding support for new systems:
1. Add new paths to `find-stty()` and `find-tty()`
2. Update terminal detection in `detect-terminal-type()`
3. Test with the diagnostic script
4. Update this documentation