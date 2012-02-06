mark-multiple.el
================

An emacs extension that sorta lets you mark several regions at once.

More precisely, it allows for one master region, with several mirror
regions. The mirrors are updated inline while you type. This allows for some
awesome functionality. Or at least, some more visually pleasing insert and
replace operations.

Video
-----
You can [watch an intro to mark-multiple at Emacs Rocks](http://emacsrocks.com/e08.html).

Done
----
* A general library for managing master and mirrors
* `mark-more-like-this` which selects next/previous substring in the buffer that
  matches the current region.
* `inline-string-rectangle` which works like `string-rectangle` but lets you
  write inline - making it less error prone.
* `rename-sgml-tag` which updates the matching tag while typing.

Note: `js2-rename-var` has been moved to [js2-refactor.el](https://github.com/magnars/js2-refactor.el). 

Installation
------------

    git submodule add https://github.com/magnars/mark-multiple.el.git site-lisp/mark-multiple

Then add the modules you want to your init-file:

    (require 'inline-string-rectangle)
    (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

    (require 'mark-more-like-this)
    (global-set-key (kbd "C-<") 'mark-previous-like-this)
    (global-set-key (kbd "C->") 'mark-next-like-this)
    (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)

    (require 'rename-sgml-tag)
    (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

    (require 'js2-rename-var)
    (define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)

Feel free to come up with your own keybindings.

Bugs and gotchas
----------------
* Adding a master and mirrors does not remove the active region. This might feel
  strange, but turns out to be practical.

* The current mark-multiple general library lets you do stupid shit, like adding
  overlapping mirrors. That's only a problem for people who want to write their
  own functions using `mm/create-master` and `mm/add-mirror`.

* Seems like there is some conflict with undo-tree.el, which sometimes clobbers
  the undo history. I might be doing something particularly stupid. Looking into it.

* Reverting a buffer with active marks makes them unremovable.

A wild idea
-----------

Is this a subset of a crazy multiple-point module? How would that even work?

There is one use for it I can see, which is editing the end of lines. Set up one
cursor at the end of each line, then just edit normally. The command is repeated
for each position.

Might be too far out there. I still want to do edit-end-of-lines tho.

Contribute
----------

If you make some nice commands with mark-multiple, it would be
great if you opened a pull-request. The repo is at:

    https://github.com/magnars/mark-multiple.el

Contributors
------------

* [Pao-Chin Wu](https://github.com/abaw) fixed `mark-next-like-this` if no region is active.
* [Syohei YOSHIDA](https://github.com/syohex) improved the error messages for `mark-next-like-this`

Thanks!

License
-------

Copyright (C) 2011 Magnar Sveen

Author: Magnar Sveen <magnars@gmail.com>
Keywords: marking library

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
