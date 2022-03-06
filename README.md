# Treeleaves

Treeleaves is a program designed to create file tags based on directory location
of the file. Treeleaves is made with Common Lisp (SBCL).

## Installation

Run `make build` which should fetch all the required packages
and create the `treeleaves` binary inside `bin`.

Then to install run `sudo make install` to install `treeleaves`.

### Requirements

Treeleaves requires the following common lisp packages:

- uiop
- cl-utilities
- mito
- unix-opts
- iterate

Additionally if you want to build the project from source you will need these dependencies.
- asdf
- rove

## Usage

To create the initial file database run:
``` bash
treeleaves -d ~/Documents -o ~/Documents/documents.sqlite -p "/**/*.pdf"
```

This will generate a `documents.sqlite` containing hierarchical tags
for all files specified in the directory matching the pattern glob in the output directory.

For argument defaults and more information run:
``` bash
treeleaves -h
```

To search for any matching file paths run:
``` bash
treeleaves -q "[:keyword]" "[search-term] %"
# Example:
# treeleaves -q ":tags" "Books %"
```
