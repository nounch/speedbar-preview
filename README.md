# speebar-preview.el

`speedbar-preview.el` adds three new `speedbar-...` commands and four key
bindings (configurable) to the Speedbar keymap:

| Key     | Description                | Command                     |
|---------|----------------------------|-----------------------------|
| `N`     | Preview the next thing     | `speedbar-preview-next`     |
| `P`     | Preview the previous thing | `speedbar-preview-previous` |
| `K`/`k` | Preview the current thing  | `speedbar-preview`          |

Each command keeps the cursor in the Speedbar buffer. This allows for
rapidly hitting `N` and `P` successively to preview files and directories.
If the thing to be previewed is a directory, a Dired buffer listing its
contents is displayed, if the thing is a file, it is opened in a buffer.

## Use cases:

- Browsing system log files and log file directory structures
- Browsing /proc/sys
- Browsing IRC logs
- Browsing old directories
- Browsing dotfiles
- Rails, Django, Node development (multiple directories, unfamiliat
  project)
- Templating (hierarchically sorted directories)
- Conventionalized projects (directories full of DB dumps, flat-file CMS
  structures, static site generator files)
- Exploring config directories (`.git`, `virtualenv`, etc.)
- Diving into an unfamiliar codebase
- Previewing images
- etc.

## Usage

        (require 'speedbar-preview)
