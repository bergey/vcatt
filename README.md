# vcatt

**Version Control All The Things**

This is a simple script to check that all directories below a
specified directory are managed by some sort of version control.

`vcatt` assumes that a directory is managed if it contains any of the following VC directories:

    - .git
    - _darcs
    - .hg
    - .svn
    - CVS
    - .fslckout
    
Additionally, if a directory contains only subdirectories (no files) and each of those directories is managed, the parent directory is OK.  `vcatt` will list any directories which do not satisfy one of the criterion above.

## Usage

`vcatt ~/code`
